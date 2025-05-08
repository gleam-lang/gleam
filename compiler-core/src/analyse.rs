mod imports;
pub(crate) mod name;

#[cfg(test)]
mod tests;

use crate::{
    GLEAM_CORE_PACKAGE_NAME,
    ast::{
        self, Arg, BitArrayOption, CustomType, Definition, DefinitionLocation, Function,
        GroupedStatements, Import, ModuleConstant, Publicity, RecordConstructor,
        RecordConstructorArg, SrcSpan, Statement, TypeAlias, TypeAst, TypeAstConstructor,
        TypeAstFn, TypeAstHole, TypeAstTuple, TypeAstVar, TypedDefinition, TypedExpr,
        TypedFunction, TypedModule, UntypedArg, UntypedCustomType, UntypedFunction, UntypedImport,
        UntypedModule, UntypedModuleConstant, UntypedStatement, UntypedTypeAlias,
    },
    build::{Origin, Outcome, Target},
    call_graph::{CallGraphNode, into_dependency_order},
    config::PackageConfig,
    dep_tree,
    line_numbers::LineNumbers,
    parse::SpannedString,
    reference::{EntityKind, ReferenceKind},
    type_::{
        self, AccessorsMap, Deprecation, FieldMap, ModuleInterface, Opaque, PatternConstructor,
        RecordAccessor, References, Type, TypeAliasConstructor, TypeConstructor,
        TypeValueConstructor, TypeValueConstructorField, TypeVariantConstructors, ValueConstructor,
        ValueConstructorVariant, Warning,
        environment::*,
        error::{Error, FeatureKind, MissingAnnotation, Named, Problems, convert_unify_error},
        expression::{ExprTyper, FunctionDefinition, Implementations, Purity},
        fields::FieldMapBuilder,
        hydrator::Hydrator,
        prelude::*,
    },
    uid::UniqueIdGenerator,
    warning::TypeWarningEmitter,
};
use camino::Utf8PathBuf;
use ecow::EcoString;
use hexpm::version::Version;
use itertools::Itertools;
use name::{check_argument_names, check_name_case};
use std::{
    collections::HashMap,
    ops::Deref,
    sync::{Arc, OnceLock},
};
use vec1::Vec1;

use self::imports::Importer;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Inferred<T> {
    Known(T),
    Unknown,
}

impl<T> Inferred<T> {
    pub fn expect(self, message: &str) -> T {
        match self {
            Inferred::Known(value) => Some(value),
            Inferred::Unknown => None,
        }
        .expect(message)
    }

    pub fn expect_ref(&self, message: &str) -> &T {
        match self {
            Inferred::Known(value) => Some(value),
            Inferred::Unknown => None,
        }
        .expect(message)
    }
}

impl Inferred<PatternConstructor> {
    pub fn definition_location(&self) -> Option<DefinitionLocation> {
        match self {
            Inferred::Known(value) => value.definition_location(),
            Inferred::Unknown => None,
        }
    }

    pub fn get_documentation(&self) -> Option<&str> {
        match self {
            Inferred::Known(value) => value.get_documentation(),
            Inferred::Unknown => None,
        }
    }

    pub fn field_map(&self) -> Option<&FieldMap> {
        match self {
            Inferred::Known(value) => value.field_map.as_ref(),
            Inferred::Unknown => None,
        }
    }
}

/// How the compiler should treat target support.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TargetSupport {
    /// Target support is enfored, meaning if a function is found to not have an implementation for
    /// the current target then an error is emitted and compilation halts.
    ///
    /// This is used when compiling the root package, with the exception of when using
    /// `gleam run --module $module` to run a module from a dependency package, in which case we do
    /// not want to error as the root package code isn't going to be run.
    Enforced,
    /// Target support is enfored, meaning if a function is found to not have an implementation for
    /// the current target it will continue onwards and not generate any code for this function.
    ///
    /// This is used when compiling dependencies.
    NotEnforced,
}

impl TargetSupport {
    /// Returns `true` if the target support is [`Enforced`].
    ///
    /// [`Enforced`]: TargetSupport::Enforced
    #[must_use]
    pub fn is_enforced(&self) -> bool {
        match self {
            Self::Enforced => true,
            Self::NotEnforced => false,
        }
    }
}

impl<T> From<Error> for Outcome<T, Vec1<Error>> {
    fn from(error: Error) -> Self {
        Outcome::TotalFailure(Vec1::new(error))
    }
}

/// This struct is used to take the data required for analysis. It is used to
/// construct the private ModuleAnalyzer which has this data plus any
/// internal state.
///
#[derive(Debug)]
pub struct ModuleAnalyzerConstructor<'a, A> {
    pub target: Target,
    pub ids: &'a UniqueIdGenerator,
    pub origin: Origin,
    pub importable_modules: &'a im::HashMap<EcoString, ModuleInterface>,
    pub warnings: &'a TypeWarningEmitter,
    pub direct_dependencies: &'a HashMap<EcoString, A>,
    pub target_support: TargetSupport,
    pub package_config: &'a PackageConfig,
}

impl<A> ModuleAnalyzerConstructor<'_, A> {
    /// Crawl the AST, annotating each node with the inferred type or
    /// returning an error.
    ///
    pub fn infer_module(
        self,
        module: UntypedModule,
        line_numbers: LineNumbers,
        src_path: Utf8PathBuf,
    ) -> Outcome<TypedModule, Vec1<Error>> {
        ModuleAnalyzer {
            target: self.target,
            ids: self.ids,
            origin: self.origin,
            importable_modules: self.importable_modules,
            warnings: self.warnings,
            direct_dependencies: self.direct_dependencies,
            target_support: self.target_support,
            package_config: self.package_config,
            line_numbers,
            src_path,
            problems: Problems::new(),
            value_names: HashMap::with_capacity(module.definitions.len()),
            hydrators: HashMap::with_capacity(module.definitions.len()),
            module_name: module.name.clone(),
            minimum_required_version: Version::new(0, 1, 0),
        }
        .infer_module(module)
    }
}

struct ModuleAnalyzer<'a, A> {
    target: Target,
    ids: &'a UniqueIdGenerator,
    origin: Origin,
    importable_modules: &'a im::HashMap<EcoString, ModuleInterface>,
    warnings: &'a TypeWarningEmitter,
    direct_dependencies: &'a HashMap<EcoString, A>,
    target_support: TargetSupport,
    package_config: &'a PackageConfig,
    line_numbers: LineNumbers,
    src_path: Utf8PathBuf,
    problems: Problems,
    value_names: HashMap<EcoString, SrcSpan>,
    hydrators: HashMap<EcoString, Hydrator>,
    module_name: EcoString,

    /// The minimum Gleam version required to compile the analysed module.
    minimum_required_version: Version,
}

impl<'a, A> ModuleAnalyzer<'a, A> {
    pub fn infer_module(mut self, mut module: UntypedModule) -> Outcome<TypedModule, Vec1<Error>> {
        if let Err(error) = validate_module_name(&self.module_name) {
            return self.all_errors(error);
        }

        let documentation = std::mem::take(&mut module.documentation);
        let env = EnvironmentArguments {
            ids: self.ids.clone(),
            current_package: self.package_config.name.clone(),
            gleam_version: self
                .package_config
                .gleam_version
                .clone()
                .map(|version| version.as_pubgrub()),
            current_module: self.module_name.clone(),
            target: self.target,
            importable_modules: self.importable_modules,
            target_support: self.target_support,
            current_origin: self.origin,
        }
        .build();

        let statements = GroupedStatements::new(module.into_iter_statements(self.target));
        let statements_count = statements.len();

        // Register any modules, types, and values being imported
        // We process imports first so that anything imported can be referenced
        // anywhere in the module.
        let mut env = Importer::run(self.origin, env, &statements.imports, &mut self.problems);

        // Register types so they can be used in constructors and functions
        // earlier in the module.
        for t in &statements.custom_types {
            if let Err(error) = self.register_types_from_custom_type(t, &mut env) {
                return self.all_errors(error);
            }
        }

        let sorted_aliases = match sorted_type_aliases(&statements.type_aliases) {
            Ok(it) => it,
            Err(error) => return self.all_errors(error),
        };
        for t in sorted_aliases {
            self.register_type_alias(t, &mut env);
        }

        for f in &statements.functions {
            self.register_value_from_function(f, &mut env);
        }

        // Infer the types of each statement in the module
        let mut typed_statements = Vec::with_capacity(statements_count);
        for i in statements.imports {
            optionally_push(&mut typed_statements, self.analyse_import(i, &env));
        }
        for t in statements.custom_types {
            optionally_push(&mut typed_statements, self.analyse_custom_type(t, &mut env));
        }
        for t in statements.type_aliases {
            typed_statements.push(analyse_type_alias(t, &mut env));
        }

        // Sort functions and constants into dependency order for inference. Definitions that do
        // not depend on other definitions are inferred first, then ones that depend
        // on those, etc.
        let definition_groups =
            match into_dependency_order(statements.functions, statements.constants) {
                Ok(it) => it,
                Err(error) => return self.all_errors(error),
            };
        let mut working_group = vec![];

        for group in definition_groups {
            // A group may have multiple functions that depend on each other through
            // mutual recursion.

            for definition in group {
                let def = match definition {
                    CallGraphNode::Function(f) => self.infer_function(f, &mut env),
                    CallGraphNode::ModuleConstant(c) => self.infer_module_constant(c, &mut env),
                };
                working_group.push(def);
            }

            // Now that the entire group has been inferred, generalise their types.
            for inferred in working_group.drain(..) {
                typed_statements.push(generalise_statement(inferred, &self.module_name, &mut env));
            }
        }

        // Generate warnings for unused items
        env.convert_unused_to_warnings(&mut self.problems);

        // Remove imported types and values to create the public interface
        // Private types and values are retained so they can be used in the language
        // server, but are filtered out when type checking to prevent using private
        // items.
        env.module_types
            .retain(|_, info| info.module == self.module_name);

        // Ensure no exported values have private types in their type signature
        for value in env.module_values.values() {
            self.check_for_type_leaks(value)
        }

        let Environment {
            module_types: types,
            module_types_constructors: types_constructors,
            module_values: values,
            accessors,
            names: type_names,
            module_type_aliases: type_aliases,
            echo_found,
            ..
        } = env;

        let is_internal = self
            .package_config
            .is_internal_module(self.module_name.as_str());

        // We sort warnings and errors to ensure they are emitted in a
        // deterministic order, making them easier to test and debug, and to
        // make the output predictable.
        self.problems.sort();

        let warnings = self.problems.take_warnings();
        for warning in &warnings {
            // TODO: remove this clone
            self.warnings.emit(warning.clone());
        }

        let module = ast::Module {
            documentation: documentation.clone(),
            name: self.module_name.clone(),
            definitions: typed_statements,
            type_info: ModuleInterface {
                name: self.module_name,
                types,
                types_value_constructors: types_constructors,
                values,
                accessors,
                origin: self.origin,
                package: self.package_config.name.clone(),
                is_internal,
                line_numbers: self.line_numbers,
                src_path: self.src_path,
                warnings,
                minimum_required_version: self.minimum_required_version,
                type_aliases,
                documentation,
                contains_echo: echo_found,
                references: References {
                    imported_modules: env
                        .imported_modules
                        .values()
                        .map(|(_location, module)| module.name.clone())
                        .collect(),
                    value_references: env.references.value_references,
                    type_references: env.references.type_references,
                },
            },
            names: type_names,
        };

        match Vec1::try_from_vec(self.problems.take_errors()) {
            Err(_) => Outcome::Ok(module),
            Ok(errors) => Outcome::PartialFailure(module, errors),
        }
    }

    fn all_errors<T>(&mut self, error: Error) -> Outcome<T, Vec1<Error>> {
        Outcome::TotalFailure(Vec1::from_vec_push(self.problems.take_errors(), error))
    }

    fn infer_module_constant(
        &mut self,
        c: UntypedModuleConstant,
        environment: &mut Environment<'_>,
    ) -> TypedDefinition {
        let ModuleConstant {
            documentation: doc,
            location,
            name,
            name_location,
            annotation,
            publicity,
            value,
            deprecation,
            ..
        } = c;
        self.check_name_case(name_location, &name, Named::Constant);

        environment.references.begin_constant();

        let definition = FunctionDefinition {
            has_body: true,
            has_erlang_external: false,
            has_javascript_external: false,
        };
        let mut expr_typer = ExprTyper::new(environment, definition, &mut self.problems);
        let typed_expr = expr_typer.infer_const(&annotation, *value);
        let type_ = typed_expr.type_();
        let implementations = expr_typer.implementations;

        let minimum_required_version = expr_typer.minimum_required_version;
        if minimum_required_version > self.minimum_required_version {
            self.minimum_required_version = minimum_required_version;
        }

        match publicity {
            Publicity::Private
            | Publicity::Public
            | Publicity::Internal {
                attribute_location: None,
            } => (),

            Publicity::Internal {
                attribute_location: Some(location),
            } => self.track_feature_usage(FeatureKind::InternalAnnotation, location),
        }

        let variant = ValueConstructor {
            publicity,
            deprecation: deprecation.clone(),
            variant: ValueConstructorVariant::ModuleConstant {
                documentation: doc.as_ref().map(|(_, doc)| doc.clone()),
                location,
                literal: typed_expr.clone(),
                module: self.module_name.clone(),
                name: name.clone(),
                implementations,
            },
            type_: type_.clone(),
        };

        environment.insert_variable(
            name.clone(),
            variant.variant.clone(),
            type_.clone(),
            publicity,
            Deprecation::NotDeprecated,
        );
        environment.insert_module_value(name.clone(), variant);

        environment
            .references
            .register_constant(name.clone(), location, publicity);

        environment.references.register_value_reference(
            environment.current_module.clone(),
            name.clone(),
            &name,
            name_location,
            ReferenceKind::Definition,
        );

        Definition::ModuleConstant(ModuleConstant {
            documentation: doc,
            location,
            name,
            name_location,
            annotation,
            publicity,
            value: Box::new(typed_expr),
            type_,
            deprecation,
            implementations,
        })
    }

    // TODO: Extract this into a class of its own! Or perhaps it just wants some
    // helper methods extracted. There's a whole bunch of state in this one
    // function, and it does a handful of things.
    fn infer_function(
        &mut self,
        f: UntypedFunction,
        environment: &mut Environment<'_>,
    ) -> TypedDefinition {
        let Function {
            documentation: doc,
            location,
            name,
            publicity,
            arguments,
            body,
            return_annotation,
            end_position: end_location,
            deprecation,
            external_erlang,
            external_javascript,
            return_type: (),
            implementations: _,
            purity: _,
        } = f;
        let (name_location, name) = name.expect("Function in a definition must be named");
        let target = environment.target;
        let body_location = body.last().location();
        let preregistered_fn = environment
            .get_variable(&name)
            .expect("Could not find preregistered type for function");
        let field_map = preregistered_fn.field_map().cloned();
        let preregistered_type = preregistered_fn.type_.clone();
        let (prereg_args_types, prereg_return_type) = preregistered_type
            .fn_types()
            .expect("Preregistered type for fn was not a fn");

        // Ensure that folks are not writing inline JavaScript expressions as
        // the implementation for JS externals.
        self.assert_valid_javascript_external(&name, external_javascript.as_ref(), location);

        // Find the external implementation for the current target, if one has been given.
        let external =
            target_function_implementation(target, &external_erlang, &external_javascript);

        // The function must have at least one implementation somewhere.
        let has_implementation = self.ensure_function_has_an_implementation(
            &body,
            &external_erlang,
            &external_javascript,
            location,
        );

        if external.is_some() {
            // There was an external implementation, so type annotations are
            // mandatory as the Gleam implementation may be absent, and because we
            // think you should always specify types for external functions for
            // clarity + to avoid accidental mistakes.
            self.ensure_annotations_present(&arguments, return_annotation.as_ref(), location);
        }

        let has_body = !body.first().is_placeholder();
        let definition = FunctionDefinition {
            has_body,
            has_erlang_external: external_erlang.is_some(),
            has_javascript_external: external_javascript.is_some(),
        };

        let typed_args = arguments
            .into_iter()
            .zip(&prereg_args_types)
            .map(|(a, t)| a.set_type(t.clone()))
            .collect_vec();

        // We have already registered the function in the `register_value_from_function`
        // method, but here we must set this as the current function again, so that anything
        // we reference in the body of it can be tracked properly in the call graph.
        environment.references.set_current_node(name.clone());

        // Infer the type using the preregistered args + return types as a starting point
        let result = environment.in_new_scope(&mut self.problems, |environment, problems| {
            let mut expr_typer = ExprTyper::new(environment, definition, problems);
            expr_typer.hydrator = self
                .hydrators
                .remove(&name)
                .expect("Could not find hydrator for fn");

            let (args, body) = expr_typer.infer_fn_with_known_types(
                typed_args.clone(),
                body,
                Some(prereg_return_type.clone()),
            )?;
            let args_types = args.iter().map(|a| a.type_.clone()).collect();
            let type_ = fn_(args_types, body.last().type_());
            Ok((
                type_,
                body,
                expr_typer.implementations,
                expr_typer.minimum_required_version,
                expr_typer.purity,
            ))
        });

        // If we could not successfully infer the type etc information of the
        // function then register the error and continue anaylsis using the best
        // information that we have, so we can still learn about the rest of the
        // module.
        let (type_, body, implementations, required_version, purity) = match result {
            Ok((type_, body, implementations, required_version, purity)) => {
                (type_, body, implementations, required_version, purity)
            }
            Err(error) => {
                self.problems.error(error);
                let type_ = preregistered_type.clone();
                let body = Vec1::new(Statement::Expression(TypedExpr::Invalid {
                    type_: prereg_return_type.clone(),
                    location: SrcSpan {
                        start: body_location.end,
                        end: body_location.end,
                    },
                }));
                let implementations = Implementations::supporting_all();
                (
                    type_,
                    body,
                    implementations,
                    Version::new(1, 0, 0),
                    Purity::Impure,
                )
            }
        };

        if required_version > self.minimum_required_version {
            self.minimum_required_version = required_version;
        }

        match publicity {
            Publicity::Private
            | Publicity::Public
            | Publicity::Internal {
                attribute_location: None,
            } => (),

            Publicity::Internal {
                attribute_location: Some(location),
            } => self.track_feature_usage(FeatureKind::InternalAnnotation, location),
        }

        if let Some((module, _, location)) = &external_javascript {
            if module.contains('@') {
                self.track_feature_usage(FeatureKind::AtInJavascriptModules, *location)
            }
        }

        // Assert that the inferred type matches the type of any recursive call
        if let Err(error) = unify(preregistered_type.clone(), type_) {
            self.problems.error(convert_unify_error(error, location));
        }

        // Ensure that the current target has an implementation for the function.
        // This is done at the expression level while inferring the function body, but we do it again
        // here as externally implemented functions may not have a Gleam body.
        //
        // We don't emit this error if there is no implementation, as this would
        // have already emitted an error above.
        if has_implementation
            && publicity.is_importable()
            && environment.target_support.is_enforced()
            && !implementations.supports(target)
            // We don't emit this error if there is a body
            // since this would be caught at the statement level
            && !has_body
        {
            self.problems.error(Error::UnsupportedPublicFunctionTarget {
                name: name.clone(),
                target,
                location,
            });
        }

        let variant = ValueConstructorVariant::ModuleFn {
            documentation: doc.as_ref().map(|(_, doc)| doc.clone()),
            name: name.clone(),
            external_erlang: external_erlang
                .as_ref()
                .map(|(m, f, _)| (m.clone(), f.clone())),
            external_javascript: external_javascript
                .as_ref()
                .map(|(m, f, _)| (m.clone(), f.clone())),
            field_map,
            module: environment.current_module.clone(),
            arity: typed_args.len(),
            location,
            implementations,
            purity,
        };

        environment.insert_variable(
            name.clone(),
            variant,
            preregistered_type.clone(),
            publicity,
            deprecation.clone(),
        );

        environment.references.register_value_reference(
            environment.current_module.clone(),
            name.clone(),
            &name,
            name_location,
            ReferenceKind::Definition,
        );

        Definition::Function(Function {
            documentation: doc,
            location,
            name: Some((name_location, name)),
            publicity,
            deprecation,
            arguments: typed_args,
            end_position: end_location,
            return_annotation,
            return_type: preregistered_type
                .return_type()
                .expect("Could not find return type for fn"),
            body,
            external_erlang,
            external_javascript,
            implementations,
            purity,
        })
    }

    fn assert_valid_javascript_external(
        &mut self,
        function_name: &EcoString,
        external_javascript: Option<&(EcoString, EcoString, SrcSpan)>,
        location: SrcSpan,
    ) {
        use regex::Regex;

        static MODULE: OnceLock<Regex> = OnceLock::new();
        static FUNCTION: OnceLock<Regex> = OnceLock::new();

        let (module, function) = match external_javascript {
            None => return,
            Some((module, function, _location)) => (module, function),
        };
        if !MODULE
            .get_or_init(|| Regex::new("^[@a-zA-Z0-9\\./:_-]+$").expect("regex"))
            .is_match(module)
        {
            self.problems.error(Error::InvalidExternalJavascriptModule {
                location,
                module: module.clone(),
                name: function_name.clone(),
            });
        }
        if !FUNCTION
            .get_or_init(|| Regex::new("^[a-zA-Z_][a-zA-Z0-9_]*$").expect("regex"))
            .is_match(function)
        {
            self.problems
                .error(Error::InvalidExternalJavascriptFunction {
                    location,
                    function: function.clone(),
                    name: function_name.clone(),
                });
        }
    }

    fn ensure_annotations_present(
        &mut self,
        arguments: &[UntypedArg],
        return_annotation: Option<&TypeAst>,
        location: SrcSpan,
    ) {
        for arg in arguments {
            if arg.annotation.is_none() {
                self.problems.error(Error::ExternalMissingAnnotation {
                    location: arg.location,
                    kind: MissingAnnotation::Parameter,
                });
            }
        }
        if return_annotation.is_none() {
            self.problems.error(Error::ExternalMissingAnnotation {
                location,
                kind: MissingAnnotation::Return,
            });
        }
    }

    fn ensure_function_has_an_implementation(
        &mut self,
        body: &Vec1<UntypedStatement>,
        external_erlang: &Option<(EcoString, EcoString, SrcSpan)>,
        external_javascript: &Option<(EcoString, EcoString, SrcSpan)>,
        location: SrcSpan,
    ) -> bool {
        match (external_erlang, external_javascript) {
            (None, None) if body.first().is_placeholder() => {
                self.problems.error(Error::NoImplementation { location });
                false
            }
            _ => true,
        }
    }

    fn analyse_import(
        &mut self,
        i: UntypedImport,
        environment: &Environment<'_>,
    ) -> Option<TypedDefinition> {
        let Import {
            documentation,
            location,
            module,
            as_name,
            unqualified_values,
            unqualified_types,
            ..
        } = i;
        // Find imported module
        let Some(module_info) = environment.importable_modules.get(&module) else {
            // Here the module being imported doesn't exist. We don't emit an
            // error here as the `Importer` that was run earlier will have
            // already emitted an error for this.
            return None;
        };

        // Modules should belong to a package that is a direct dependency of the
        // current package to be imported.
        // Upgrade this to an error in future.
        if module_info.package != GLEAM_CORE_PACKAGE_NAME
            && module_info.package != self.package_config.name
            && !self.direct_dependencies.contains_key(&module_info.package)
        {
            self.warnings.emit(Warning::TransitiveDependencyImported {
                location,
                module: module_info.name.clone(),
                package: module_info.package.clone(),
            })
        }

        Some(Definition::Import(Import {
            documentation,
            location,
            module,
            as_name,
            unqualified_values,
            unqualified_types,
            package: module_info.package.clone(),
        }))
    }

    fn analyse_custom_type(
        &mut self,
        t: UntypedCustomType,
        environment: &mut Environment<'_>,
    ) -> Option<TypedDefinition> {
        match self.do_analyse_custom_type(t, environment) {
            Ok(t) => Some(t),
            Err(error) => {
                self.problems.error(error);
                None
            }
        }
    }

    // TODO: split this into a new class.
    fn do_analyse_custom_type(
        &mut self,
        t: UntypedCustomType,
        environment: &mut Environment<'_>,
    ) -> Result<TypedDefinition, Error> {
        self.register_values_from_custom_type(
            &t,
            environment,
            &t.parameters.iter().map(|(_, name)| name).collect_vec(),
        )?;

        let CustomType {
            documentation: doc,
            location,
            end_position,
            publicity,
            opaque,
            name,
            name_location,
            parameters,
            constructors,
            deprecation,
            ..
        } = t;

        match publicity {
            Publicity::Private
            | Publicity::Public
            | Publicity::Internal {
                attribute_location: None,
            } => (),

            Publicity::Internal {
                attribute_location: Some(location),
            } => self.track_feature_usage(FeatureKind::InternalAnnotation, location),
        }

        let constructors: Vec<RecordConstructor<Arc<Type>>> = constructors
            .into_iter()
            .map(
                |RecordConstructor {
                     location,
                     name_location,
                     name,
                     arguments: args,
                     documentation,
                     deprecation: constructor_deprecation,
                 }| {
                    self.check_name_case(name_location, &name, Named::CustomTypeVariant);
                    if constructor_deprecation.is_deprecated() {
                        self.track_feature_usage(
                            FeatureKind::VariantWithDeprecatedAnnotation,
                            location,
                        );
                    }

                    let preregistered_fn = environment
                        .get_variable(&name)
                        .expect("Could not find preregistered type for function");
                    let preregistered_type = preregistered_fn.type_.clone();

                    let args = match preregistered_type.fn_types() {
                        Some((args_types, _return_type)) => args
                            .into_iter()
                            .zip(&args_types)
                            .map(|(argument, t)| {
                                if let Some((location, label)) = &argument.label {
                                    self.check_name_case(*location, label, Named::Label);
                                }

                                RecordConstructorArg {
                                    label: argument.label,
                                    ast: argument.ast,
                                    location: argument.location,
                                    type_: t.clone(),
                                    doc: argument.doc,
                                }
                            })
                            .collect(),
                        _ => {
                            vec![]
                        }
                    };

                    RecordConstructor {
                        location,
                        name_location,
                        name,
                        arguments: args,
                        documentation,
                        deprecation: constructor_deprecation,
                    }
                },
            )
            .collect();
        let typed_parameters = environment
            .get_type_constructor(&None, &name)
            .expect("Could not find preregistered type constructor ")
            .parameters
            .clone();

        // Check if all constructors are deprecated if so error.
        if !constructors.is_empty()
            && constructors
                .iter()
                .all(|record| record.deprecation.is_deprecated())
        {
            self.problems
                .error(Error::AllVariantsDeprecated { location });
        }

        // If any constructor record/varient is deprecated while
        // the type is deprecated as a whole that is considered an error.
        if deprecation.is_deprecated()
            && !constructors.is_empty()
            && constructors
                .iter()
                .any(|record| record.deprecation.is_deprecated())
        {
            // Report error on all variants attibuted with deprecated
            constructors
                .iter()
                .filter(|record| record.deprecation.is_deprecated())
                .for_each(|record| {
                    self.problems
                        .error(Error::DeprecatedVariantOnDeprecatedType {
                            location: record.location,
                        });
                });
        }

        Ok(Definition::CustomType(CustomType {
            documentation: doc,
            location,
            end_position,
            publicity,
            opaque,
            name,
            name_location,
            parameters,
            constructors,
            typed_parameters,
            deprecation,
        }))
    }

    fn register_values_from_custom_type(
        &mut self,
        t: &UntypedCustomType,
        environment: &mut Environment<'_>,
        type_parameters: &[&EcoString],
    ) -> Result<(), Error> {
        let CustomType {
            publicity,
            opaque,
            name,
            constructors,
            deprecation,
            ..
        } = t;

        let mut hydrator = self
            .hydrators
            .remove(name)
            .expect("Could not find hydrator for register_values custom type");
        hydrator.disallow_new_type_variables();
        let type_ = environment
            .module_types
            .get(name)
            .expect("Type for custom type not found in register_values")
            .type_
            .clone();

        let mut constructors_data = vec![];

        let mut index = 0;
        for constructor in constructors.iter() {
            if let Err(error) = assert_unique_name(
                &mut self.value_names,
                &constructor.name,
                constructor.location,
            ) {
                self.problems.error(error);
                continue;
            }

            // If the constructor belongs to an opaque type then it's going to be
            // considered as private.
            let value_constructor_publicity = if *opaque {
                Publicity::Private
            } else {
                *publicity
            };

            environment.references.register_value(
                constructor.name.clone(),
                EntityKind::Constructor,
                constructor.location,
                value_constructor_publicity,
            );

            environment
                .references
                .register_type_reference_in_call_graph(name.clone());

            let mut field_map_builder = FieldMapBuilder::new(constructor.arguments.len() as u32);
            let mut args_types = Vec::with_capacity(constructor.arguments.len());
            let mut fields = Vec::with_capacity(constructor.arguments.len());

            for RecordConstructorArg {
                label,
                ast,
                location,
                ..
            } in constructor.arguments.iter()
            {
                // Build a type from the annotation AST
                let t = match hydrator.type_from_ast(ast, environment, &mut self.problems) {
                    Ok(t) => t,
                    Err(e) => {
                        self.problems.error(e);
                        continue;
                    }
                };

                fields.push(TypeValueConstructorField {
                    type_: t.clone(),
                    label: label.as_ref().map(|(_location, label)| label.clone()),
                });

                // Register the type for this parameter
                args_types.push(t);

                let (label_location, label) = match label {
                    Some((location, label)) => (*location, Some(label)),
                    None => (*location, None),
                };

                // Register the label for this parameter
                if let Err(error) = field_map_builder.add(label, label_location) {
                    self.problems.error(error);
                }
            }
            let field_map = field_map_builder.finish();
            // Insert constructor function into module scope
            let mut type_ = type_.deref().clone();
            type_.set_custom_type_variant(index as u16);
            let type_ = match constructor.arguments.len() {
                0 => Arc::new(type_),
                _ => fn_(args_types.clone(), Arc::new(type_)),
            };
            let constructor_info = ValueConstructorVariant::Record {
                documentation: constructor
                    .documentation
                    .as_ref()
                    .map(|(_, doc)| doc.clone()),
                variants_count: constructors.len() as u16,
                name: constructor.name.clone(),
                arity: constructor.arguments.len() as u16,
                field_map: field_map.clone(),
                location: constructor.location,
                module: self.module_name.clone(),
                variant_index: index as u16,
            };
            index += 1;

            // If the whole custom type is deprecated all of its varints are too.
            // Otherwise just the varint(s) attributed as deprecated are.
            let deprecate_constructor = if deprecation.is_deprecated() {
                deprecation
            } else {
                &constructor.deprecation
            };

            environment.insert_module_value(
                constructor.name.clone(),
                ValueConstructor {
                    publicity: value_constructor_publicity,
                    deprecation: deprecate_constructor.clone(),
                    type_: type_.clone(),
                    variant: constructor_info.clone(),
                },
            );

            environment.references.register_value_reference(
                environment.current_module.clone(),
                constructor.name.clone(),
                &constructor.name,
                constructor.name_location,
                ReferenceKind::Definition,
            );

            constructors_data.push(TypeValueConstructor {
                name: constructor.name.clone(),
                parameters: fields,
                documentation: constructor
                    .documentation
                    .as_ref()
                    .map(|(_, documentation)| documentation.clone()),
            });
            environment.insert_variable(
                constructor.name.clone(),
                constructor_info,
                type_,
                value_constructor_publicity,
                deprecate_constructor.clone(),
            );

            environment.names.named_constructor_in_scope(
                environment.current_module.clone(),
                constructor.name.clone(),
                constructor.name.clone(),
            );
        }

        let Accessors {
            shared_accessors,
            variant_specific_accessors,
        } = custom_type_accessors(&constructors_data)?;

        let map = AccessorsMap {
            publicity: if *opaque {
                Publicity::Private
            } else {
                *publicity
            },
            shared_accessors,
            // TODO: improve the ownership here so that we can use the
            // `return_type_constructor` below rather than looking it up twice.
            type_: type_.clone(),
            variant_specific_accessors,
        };
        environment.insert_accessors(name.clone(), map);

        let opaque = if *opaque {
            Opaque::Opaque
        } else {
            Opaque::NotOpaque
        };
        // Now record the constructors for the type.
        environment.insert_type_to_constructors(
            name.clone(),
            TypeVariantConstructors::new(constructors_data, type_parameters, opaque, hydrator),
        );

        Ok(())
    }

    fn register_types_from_custom_type(
        &mut self,
        t: &UntypedCustomType,
        environment: &mut Environment<'a>,
    ) -> Result<(), Error> {
        let CustomType {
            name,
            name_location,
            publicity,
            parameters,
            location,
            deprecation,
            opaque,
            constructors,
            documentation,
            ..
        } = t;
        // We exit early here as we don't yet have a good way to handle the two
        // duplicate definitions in the later pass of the analyser which
        // register the constructor values for the types. The latter would end up
        // overwriting the former, but here in type registering we keep the
        // former. I think we want to really keep the former both times.
        // The fact we can't straightforwardly do this indicated to me that we
        // could improve our approach here somewhat.
        environment.assert_unique_type_name(name, *location)?;

        self.check_name_case(*name_location, name, Named::Type);

        let mut hydrator = Hydrator::new();
        let parameters = self.make_type_vars(parameters, &mut hydrator, environment);

        hydrator.clear_ridgid_type_names();

        // We check is the type comes from an internal module and restrict its
        // publicity.
        let publicity = match publicity {
            // It's important we only restrict the publicity of public types.
            Publicity::Public if self.package_config.is_internal_module(&self.module_name) => {
                Publicity::Internal {
                    attribute_location: None,
                }
            }
            // If a type is private we don't want to make it internal just because
            // it comes from an internal module, so in that case the publicity is
            // left unchanged.
            Publicity::Public | Publicity::Private | Publicity::Internal { .. } => *publicity,
        };

        let type_ = Arc::new(Type::Named {
            publicity,
            package: environment.current_package.clone(),
            module: self.module_name.to_owned(),
            name: name.clone(),
            args: parameters.clone(),
            inferred_variant: None,
        });
        let _ = self.hydrators.insert(name.clone(), hydrator);
        environment
            .insert_type_constructor(
                name.clone(),
                TypeConstructor {
                    origin: *location,
                    module: self.module_name.clone(),
                    deprecation: deprecation.clone(),
                    parameters,
                    publicity,
                    type_,
                    documentation: documentation.as_ref().map(|(_, doc)| doc.clone()),
                },
            )
            .expect("name uniqueness checked above");

        environment.names.named_type_in_scope(
            environment.current_module.clone(),
            name.clone(),
            name.clone(),
        );

        environment
            .references
            .register_type(name.clone(), EntityKind::Type, *location, publicity);

        environment.references.register_type_reference(
            environment.current_module.clone(),
            name.clone(),
            name,
            *name_location,
            ReferenceKind::Definition,
        );

        if *opaque && constructors.is_empty() {
            self.problems.warning(Warning::OpaqueExternalType {
                location: *location,
            });
        }

        Ok(())
    }

    fn register_type_alias(&mut self, t: &UntypedTypeAlias, environment: &mut Environment<'_>) {
        let TypeAlias {
            location,
            publicity,
            parameters: args,
            alias: name,
            name_location,
            type_ast: resolved_type,
            deprecation,
            type_: _,
            documentation,
        } = t;

        // A type alias must not have the same name as any other type in the module.
        if let Err(error) = environment.assert_unique_type_name(name, *location) {
            self.problems.error(error);
            // A type already exists with the name so we cannot continue and
            // register this new type with the same name.
            return;
        }

        self.check_name_case(*name_location, name, Named::TypeAlias);

        environment
            .references
            .register_type(name.clone(), EntityKind::Type, *location, *publicity);

        // Use the hydrator to convert the AST into a type, erroring if the AST was invalid
        // in some fashion.
        let mut hydrator = Hydrator::new();
        let parameters = self.make_type_vars(args, &mut hydrator, environment);
        let arity = parameters.len();
        let tryblock = || {
            hydrator.disallow_new_type_variables();
            let type_ = hydrator.type_from_ast(resolved_type, environment, &mut self.problems)?;

            environment
                .names
                .type_in_scope(name.clone(), type_.as_ref(), &parameters);

            // Insert the alias so that it can be used by other code.
            environment.insert_type_constructor(
                name.clone(),
                TypeConstructor {
                    origin: *location,
                    module: self.module_name.clone(),
                    parameters,
                    type_: type_.clone(),
                    deprecation: deprecation.clone(),
                    publicity: *publicity,
                    documentation: documentation.as_ref().map(|(_, doc)| doc.clone()),
                },
            )?;

            environment.insert_type_alias(
                name.clone(),
                TypeAliasConstructor {
                    origin: *location,
                    module: self.module_name.clone(),
                    type_,
                    publicity: *publicity,
                    deprecation: deprecation.clone(),
                    documentation: documentation.as_ref().map(|(_, doc)| doc.clone()),
                    arity,
                },
            )?;

            if let Some(name) = hydrator.unused_type_variables().next() {
                return Err(Error::UnusedTypeAliasParameter {
                    location: *location,
                    name: name.clone(),
                });
            }

            Ok(())
        };
        let result = tryblock();
        self.record_if_error(result);
    }

    fn make_type_vars(
        &mut self,
        args: &[SpannedString],
        hydrator: &mut Hydrator,
        environment: &mut Environment<'_>,
    ) -> Vec<Arc<Type>> {
        args.iter()
            .map(|(location, name)| {
                self.check_name_case(*location, name, Named::TypeVariable);
                match hydrator.add_type_variable(name, environment) {
                    Ok(t) => t,
                    Err(t) => {
                        self.problems.error(Error::DuplicateTypeParameter {
                            location: *location,
                            name: name.clone(),
                        });
                        t
                    }
                }
            })
            .collect()
    }

    fn record_if_error(&mut self, result: Result<(), Error>) {
        if let Err(error) = result {
            self.problems.error(error);
        }
    }

    fn register_value_from_function(
        &mut self,
        f: &UntypedFunction,
        environment: &mut Environment<'_>,
    ) {
        let Function {
            name,
            arguments: args,
            location,
            return_annotation,
            publicity,
            documentation,
            external_erlang,
            external_javascript,
            deprecation,
            end_position: _,
            body: _,
            return_type: _,
            implementations,
            purity,
        } = f;
        let (name_location, name) = name.as_ref().expect("A module's function must be named");

        self.check_name_case(*name_location, name, Named::Function);

        environment.references.register_value(
            name.clone(),
            EntityKind::Function,
            *location,
            *publicity,
        );

        let mut builder = FieldMapBuilder::new(args.len() as u32);
        for Arg {
            names, location, ..
        } in args.iter()
        {
            check_argument_names(names, &mut self.problems);

            if let Err(error) = builder.add(names.get_label(), *location) {
                self.problems.error(error);
            }
        }
        let field_map = builder.finish();
        let mut hydrator = Hydrator::new();

        // When external implementations are present then the type annotations
        // must be given in full, so we disallow holes in the annotations.
        hydrator.permit_holes(external_erlang.is_none() && external_javascript.is_none());

        let arg_types = args
            .iter()
            .map(|arg| {
                match hydrator.type_from_option_ast(
                    &arg.annotation,
                    environment,
                    &mut self.problems,
                ) {
                    Ok(type_) => type_,
                    Err(error) => {
                        self.problems.error(error);
                        environment.new_unbound_var()
                    }
                }
            })
            .collect();

        let return_type =
            match hydrator.type_from_option_ast(return_annotation, environment, &mut self.problems)
            {
                Ok(type_) => type_,
                Err(error) => {
                    self.problems.error(error);
                    environment.new_unbound_var()
                }
            };

        let type_ = fn_(arg_types, return_type);
        let _ = self.hydrators.insert(name.clone(), hydrator);

        let variant = ValueConstructorVariant::ModuleFn {
            documentation: documentation.as_ref().map(|(_, doc)| doc.clone()),
            name: name.clone(),
            field_map,
            external_erlang: external_erlang
                .as_ref()
                .map(|(m, f, _)| (m.clone(), f.clone())),
            external_javascript: external_javascript
                .as_ref()
                .map(|(m, f, _)| (m.clone(), f.clone())),
            module: environment.current_module.clone(),
            arity: args.len(),
            location: *location,
            implementations: *implementations,
            purity: *purity,
        };
        environment.insert_variable(
            name.clone(),
            variant,
            type_,
            *publicity,
            deprecation.clone(),
        );
    }

    fn check_for_type_leaks(&mut self, value: &ValueConstructor) {
        // A private value doesn't export anything so it can't leak anything.
        if value.publicity.is_private() {
            return;
        }

        // If a private or internal value references a private type
        if let Some(leaked) = value.type_.find_private_type() {
            self.problems.error(Error::PrivateTypeLeak {
                location: value.variant.definition_location(),
                leaked,
            });
        }
    }

    fn check_name_case(&mut self, location: SrcSpan, name: &EcoString, kind: Named) {
        if let Err(error) = check_name_case(location, name, kind) {
            self.problems.error(error);
        }
    }

    fn track_feature_usage(&mut self, feature_kind: FeatureKind, location: SrcSpan) {
        let minimum_required_version = feature_kind.required_version();

        // Then if the required version is not in the specified version for the
        // range we emit a warning highlighting the usage of the feature.
        if let Some(gleam_version) = &self.package_config.gleam_version {
            if let Some(lowest_allowed_version) = gleam_version.lowest_version() {
                // There is a version in the specified range that is lower than
                // the one required by this feature! This means that the
                // specified range is wrong and would allow someone to run a
                // compiler that is too old to know of this feature.
                if minimum_required_version > lowest_allowed_version {
                    self.problems
                        .warning(Warning::FeatureRequiresHigherGleamVersion {
                            location,
                            feature_kind,
                            minimum_required_version: minimum_required_version.clone(),
                            wrongfully_allowed_version: lowest_allowed_version,
                        })
                }
            }
        }

        if minimum_required_version > self.minimum_required_version {
            self.minimum_required_version = minimum_required_version;
        }
    }
}

fn optionally_push<T>(vector: &mut Vec<T>, item: Option<T>) {
    if let Some(item) = item {
        vector.push(item)
    }
}

fn validate_module_name(name: &EcoString) -> Result<(), Error> {
    if is_prelude_module(name) {
        return Err(Error::ReservedModuleName { name: name.clone() });
    };
    for segment in name.split('/') {
        if crate::parse::lexer::str_to_keyword(segment).is_some() {
            return Err(Error::KeywordInModuleName {
                name: name.clone(),
                keyword: segment.into(),
            });
        }
    }
    Ok(())
}

fn target_function_implementation<'a>(
    target: Target,
    external_erlang: &'a Option<(EcoString, EcoString, SrcSpan)>,
    external_javascript: &'a Option<(EcoString, EcoString, SrcSpan)>,
) -> &'a Option<(EcoString, EcoString, SrcSpan)> {
    match target {
        Target::Erlang => external_erlang,
        Target::JavaScript => external_javascript,
    }
}

fn analyse_type_alias(t: UntypedTypeAlias, environment: &mut Environment<'_>) -> TypedDefinition {
    let TypeAlias {
        documentation: doc,
        location,
        publicity,
        alias,
        name_location,
        parameters: args,
        type_ast: resolved_type,
        deprecation,
        ..
    } = t;

    // There could be no type alias registered if it was invalid in some way.
    // analysis aims to be fault tolerant to get the best possible feedback for
    // the programmer in the language server, so the analyser gets here even
    // though there was previously errors.
    let type_ = match environment.get_type_constructor(&None, &alias) {
        Ok(constructor) => constructor.type_.clone(),
        Err(_) => environment.new_generic_var(),
    };
    Definition::TypeAlias(TypeAlias {
        documentation: doc,
        location,
        publicity,
        alias,
        name_location,
        parameters: args,
        type_ast: resolved_type,
        type_,
        deprecation,
    })
}

pub fn infer_bit_array_option<UntypedValue, TypedValue, Typer>(
    segment_option: BitArrayOption<UntypedValue>,
    mut type_check: Typer,
) -> Result<BitArrayOption<TypedValue>, Error>
where
    Typer: FnMut(UntypedValue, Arc<Type>) -> Result<TypedValue, Error>,
{
    match segment_option {
        BitArrayOption::Size {
            value,
            location,
            short_form,
            ..
        } => {
            let value = type_check(*value, int())?;
            Ok(BitArrayOption::Size {
                location,
                short_form,
                value: Box::new(value),
            })
        }

        BitArrayOption::Unit { location, value } => Ok(BitArrayOption::Unit { location, value }),

        BitArrayOption::Bytes { location } => Ok(BitArrayOption::Bytes { location }),
        BitArrayOption::Int { location } => Ok(BitArrayOption::Int { location }),
        BitArrayOption::Float { location } => Ok(BitArrayOption::Float { location }),
        BitArrayOption::Bits { location } => Ok(BitArrayOption::Bits { location }),
        BitArrayOption::Utf8 { location } => Ok(BitArrayOption::Utf8 { location }),
        BitArrayOption::Utf16 { location } => Ok(BitArrayOption::Utf16 { location }),
        BitArrayOption::Utf32 { location } => Ok(BitArrayOption::Utf32 { location }),
        BitArrayOption::Utf8Codepoint { location } => {
            Ok(BitArrayOption::Utf8Codepoint { location })
        }
        BitArrayOption::Utf16Codepoint { location } => {
            Ok(BitArrayOption::Utf16Codepoint { location })
        }
        BitArrayOption::Utf32Codepoint { location } => {
            Ok(BitArrayOption::Utf32Codepoint { location })
        }
        BitArrayOption::Signed { location } => Ok(BitArrayOption::Signed { location }),
        BitArrayOption::Unsigned { location } => Ok(BitArrayOption::Unsigned { location }),
        BitArrayOption::Big { location } => Ok(BitArrayOption::Big { location }),
        BitArrayOption::Little { location } => Ok(BitArrayOption::Little { location }),
        BitArrayOption::Native { location } => Ok(BitArrayOption::Native { location }),
    }
}

fn generalise_statement(
    s: TypedDefinition,
    module_name: &EcoString,
    environment: &mut Environment<'_>,
) -> TypedDefinition {
    match s {
        Definition::Function(function) => generalise_function(function, environment, module_name),
        Definition::ModuleConstant(constant) => {
            generalise_module_constant(constant, environment, module_name)
        }
        statement @ (Definition::TypeAlias(TypeAlias { .. })
        | Definition::CustomType(CustomType { .. })
        | Definition::Import(Import { .. })) => statement,
    }
}

fn generalise_module_constant(
    constant: ModuleConstant<Arc<Type>, EcoString>,
    environment: &mut Environment<'_>,
    module_name: &EcoString,
) -> TypedDefinition {
    let ModuleConstant {
        documentation: doc,
        location,
        name,
        name_location,
        annotation,
        publicity,
        value,
        type_,
        deprecation,
        implementations,
    } = constant;
    let type_ = type_.clone();
    let type_ = type_::generalise(type_);
    let variant = ValueConstructorVariant::ModuleConstant {
        documentation: doc.as_ref().map(|(_, doc)| doc.clone()),
        location,
        literal: *value.clone(),
        module: module_name.clone(),
        implementations,
        name: name.clone(),
    };
    environment.insert_variable(
        name.clone(),
        variant.clone(),
        type_.clone(),
        publicity,
        deprecation.clone(),
    );

    environment.insert_module_value(
        name.clone(),
        ValueConstructor {
            publicity,
            variant,
            deprecation: deprecation.clone(),
            type_: type_.clone(),
        },
    );

    Definition::ModuleConstant(ModuleConstant {
        documentation: doc,
        location,
        name,
        name_location,
        annotation,
        publicity,
        value,
        type_,
        deprecation,
        implementations,
    })
}

fn generalise_function(
    function: TypedFunction,
    environment: &mut Environment<'_>,
    module_name: &EcoString,
) -> TypedDefinition {
    let Function {
        documentation: doc,
        location,
        name,
        publicity,
        deprecation,
        arguments: args,
        body,
        return_annotation,
        end_position: end_location,
        return_type,
        external_erlang,
        external_javascript,
        implementations,
        purity,
    } = function;

    let (name_location, name) = name.expect("Function in a definition must be named");

    // Lookup the inferred function information
    let function = environment
        .get_variable(&name)
        .expect("Could not find preregistered type for function");
    let field_map = function.field_map().cloned();
    let type_ = function.type_.clone();

    let type_ = type_::generalise(type_);

    // Insert the function into the module's interface
    let variant = ValueConstructorVariant::ModuleFn {
        documentation: doc.as_ref().map(|(_, doc)| doc.clone()),
        name: name.clone(),
        field_map,
        external_erlang: external_erlang
            .as_ref()
            .map(|(m, f, _)| (m.clone(), f.clone())),
        external_javascript: external_javascript
            .as_ref()
            .map(|(m, f, _)| (m.clone(), f.clone())),
        module: module_name.clone(),
        arity: args.len(),
        location,
        implementations,
        purity,
    };
    environment.insert_variable(
        name.clone(),
        variant.clone(),
        type_.clone(),
        publicity,
        deprecation.clone(),
    );
    environment.insert_module_value(
        name.clone(),
        ValueConstructor {
            publicity,
            deprecation: deprecation.clone(),
            type_,
            variant,
        },
    );

    Definition::Function(Function {
        documentation: doc,
        location,
        name: Some((name_location, name)),
        publicity,
        deprecation,
        arguments: args,
        end_position: end_location,
        return_annotation,
        return_type,
        body,
        external_erlang,
        external_javascript,
        implementations,
        purity,
    })
}

fn assert_unique_name(
    names: &mut HashMap<EcoString, SrcSpan>,
    name: &EcoString,
    location: SrcSpan,
) -> Result<(), Error> {
    match names.insert(name.clone(), location) {
        Some(previous_location) => Err(Error::DuplicateName {
            location_a: location,
            location_b: previous_location,
            name: name.clone(),
        }),
        None => Ok(()),
    }
}

struct Accessors {
    shared_accessors: HashMap<EcoString, RecordAccessor>,
    variant_specific_accessors: Vec<HashMap<EcoString, RecordAccessor>>,
}

fn custom_type_accessors(constructors: &[TypeValueConstructor]) -> Result<Accessors, Error> {
    let args = get_compatible_record_fields(constructors);

    let mut shared_accessors = HashMap::with_capacity(args.len());

    for (index, label, type_) in args {
        let _ = shared_accessors.insert(
            label.clone(),
            RecordAccessor {
                index: index as u64,
                label: label.clone(),
                type_: type_.clone(),
            },
        );
    }

    let mut variant_specific_accessors: Vec<HashMap<EcoString, RecordAccessor>> =
        Vec::with_capacity(constructors.len());

    for constructor in constructors {
        let mut fields = HashMap::with_capacity(constructor.parameters.len());

        for (index, parameter) in constructor.parameters.iter().enumerate() {
            let Some(label) = &parameter.label else {
                continue;
            };

            let _ = fields.insert(
                label.clone(),
                RecordAccessor {
                    index: index as u64,
                    label: label.clone(),
                    type_: parameter.type_.clone(),
                },
            );
        }
        variant_specific_accessors.push(fields);
    }

    Ok(Accessors {
        shared_accessors,
        variant_specific_accessors,
    })
}

/// Returns the fields that have the same label and type across all variants of
/// the given type.
fn get_compatible_record_fields(
    constructors: &[TypeValueConstructor],
) -> Vec<(usize, &EcoString, &Arc<Type>)> {
    let mut compatible = vec![];

    let first = match constructors.first() {
        Some(first) => first,
        None => return compatible,
    };

    'next_argument: for (index, first_parameter) in first.parameters.iter().enumerate() {
        // Fields without labels do not have accessors
        let first_label = match first_parameter.label.as_ref() {
            Some(label) => label,
            None => continue 'next_argument,
        };

        // Check each variant to see if they have an field in the same position
        // with the same label and the same type
        for constructor in constructors.iter().skip(1) {
            // The field must exist in all variants
            let parameter = match constructor.parameters.get(index) {
                Some(argument) => argument,
                None => continue 'next_argument,
            };

            // The labels must be the same
            if parameter
                .label
                .as_ref()
                .is_none_or(|arg_label| arg_label != first_label)
            {
                continue 'next_argument;
            }

            // The types must be the same
            if !parameter.type_.same_as(&first_parameter.type_) {
                continue 'next_argument;
            }
        }

        // The previous loop did not find any incompatible fields in the other
        // variants so this field is compatible across variants and we should
        // generate an accessor for it.
        compatible.push((index, first_label, &first_parameter.type_))
    }

    compatible
}

/// Given a type, return a list of all the types it depends on
fn get_type_dependencies(type_: &TypeAst) -> Vec<EcoString> {
    let mut deps = Vec::with_capacity(1);

    match type_ {
        TypeAst::Var(TypeAstVar { .. }) => (),
        TypeAst::Hole(TypeAstHole { .. }) => (),
        TypeAst::Constructor(TypeAstConstructor {
            name,
            arguments,
            module,
            ..
        }) => {
            deps.push(match module {
                Some((module, _)) => format!("{name}.{module}").into(),
                None => name.clone(),
            });

            for arg in arguments {
                deps.extend(get_type_dependencies(arg))
            }
        }
        TypeAst::Fn(TypeAstFn {
            arguments, return_, ..
        }) => {
            for arg in arguments {
                deps.extend(get_type_dependencies(arg))
            }
            deps.extend(get_type_dependencies(return_))
        }
        TypeAst::Tuple(TypeAstTuple { elements, .. }) => {
            for element in elements {
                deps.extend(get_type_dependencies(element))
            }
        }
    }

    deps
}

fn sorted_type_aliases(aliases: &Vec<UntypedTypeAlias>) -> Result<Vec<&UntypedTypeAlias>, Error> {
    let mut deps: Vec<(EcoString, Vec<EcoString>)> = Vec::with_capacity(aliases.len());

    for alias in aliases {
        deps.push((alias.alias.clone(), get_type_dependencies(&alias.type_ast)))
    }

    let sorted_deps = dep_tree::toposort_deps(deps).map_err(|err| {
        let dep_tree::Error::Cycle(cycle) = err;

        let last = cycle.last().expect("Cycle should not be empty");
        let alias = aliases
            .iter()
            .find(|alias| alias.alias == *last)
            .expect("Could not find alias for cycle");

        Error::RecursiveTypeAlias {
            cycle,
            location: alias.location,
        }
    })?;

    Ok(aliases
        .iter()
        .sorted_by_key(|alias| sorted_deps.iter().position(|x| x == &alias.alias))
        .collect())
}
