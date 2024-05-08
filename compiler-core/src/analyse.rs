mod imports;
#[cfg(test)]
mod tests;

use crate::{
    ast::{
        self, BitArrayOption, CustomType, Definition, DefinitionLocation, Function,
        GroupedStatements, Import, ModuleConstant, Publicity, RecordConstructor,
        RecordConstructorArg, SrcSpan, TypeAlias, TypeAst, TypeAstConstructor, TypeAstFn,
        TypeAstHole, TypeAstTuple, TypeAstVar, TypedDefinition, TypedFunction, TypedModule,
        UntypedArg, UntypedFunction, UntypedModule, UntypedStatement,
    },
    build::{Origin, Target},
    call_graph::{into_dependency_order, CallGraphNode},
    config::PackageConfig,
    dep_tree,
    line_numbers::LineNumbers,
    type_::{
        self,
        environment::*,
        error::{convert_unify_error, Error, MissingAnnotation},
        expression::{ExprTyper, FunctionDefinition},
        fields::{FieldMap, FieldMapBuilder},
        hydrator::Hydrator,
        prelude::*,
        AccessorsMap, Deprecation, ModuleInterface, PatternConstructor, RecordAccessor, Type,
        TypeConstructor, TypeValueConstructor, TypeValueConstructorField, TypeVariantConstructors,
        ValueConstructor, ValueConstructorVariant,
    },
    uid::UniqueIdGenerator,
    warning::TypeWarningEmitter,
    GLEAM_CORE_PACKAGE_NAME,
};
use camino::Utf8PathBuf;
use ecow::EcoString;
use itertools::Itertools;
use std::{
    collections::HashMap,
    sync::{Arc, OnceLock},
};
use vec1::Vec1;

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
    pub fn definition_location(&self) -> Option<DefinitionLocation<'_>> {
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

#[derive(Debug)]
pub struct InferenceFailure {
    // Right now this is an option because we don't complete the module typing on error
    // Once we add proper type holes and always return a module this should not be an option
    pub ast: Option<TypedModule>,
    // All the inference errors that occurred during type inference
    pub errors: Vec1<Error>,
}

impl From<Error> for InferenceFailure {
    // Used to create InferenceFailure from a singe error.
    // Should not be needed once we start actually collecting all the errors.
    fn from(error: Error) -> Self {
        InferenceFailure {
            ast: None,
            errors: vec1::vec1![error],
        }
    }
}

#[derive(Debug)]
pub struct ModuleAnalyzer<'a, A> {
    pub target: Target,
    pub ids: &'a UniqueIdGenerator,
    pub origin: Origin,
    pub importable_modules: &'a im::HashMap<EcoString, ModuleInterface>,
    pub warnings: &'a TypeWarningEmitter,
    pub direct_dependencies: &'a HashMap<EcoString, A>,
    pub target_support: TargetSupport,
    pub package_config: &'a PackageConfig,
}

impl<'a, A> ModuleAnalyzer<'a, A> {
    // TODO: refactor this XL function to be easier to scan, the find details
    // being in helper methods.
    //
    /// Crawl the AST, annotating each node with the inferred type or
    /// returning an error.
    ///
    pub fn infer_module(
        &self,
        mut module: UntypedModule,
        line_numbers: LineNumbers,
        src_path: Utf8PathBuf,
    ) -> Result<TypedModule, InferenceFailure> {
        let mut errors = Vec::new();
        let name = module.name.clone();
        let documentation = std::mem::take(&mut module.documentation);
        let package = self.package_config.name.clone();
        let env = Environment::new(
            self.ids.clone(),
            package.clone(),
            name.clone(),
            self.target,
            self.importable_modules,
            self.warnings,
            self.target_support,
        );
        validate_module_name(&name)?;

        let mut type_names = HashMap::with_capacity(module.definitions.len());
        let mut value_names = HashMap::with_capacity(module.definitions.len());
        let mut hydrators = HashMap::with_capacity(module.definitions.len());

        let statements = GroupedStatements::new(module.into_iter_statements(self.target));
        let statements_count = statements.len();

        // Register any modules, types, and values being imported
        // We process imports first so that anything imported can be referenced
        // anywhere in the module.
        let mut env = imports::Importer::run(self.origin, env, &statements.imports)?;

        // Register types so they can be used in constructors and functions
        // earlier in the module.
        for t in &statements.custom_types {
            register_types_from_custom_type(
                t,
                &mut type_names,
                &mut env,
                &name,
                self.package_config,
                &mut hydrators,
            )?;
        }
        // TODO: Extract a Type alias class to perform this.
        let sorted_aliases = sorted_type_aliases(&statements.type_aliases)?;
        for t in sorted_aliases {
            register_type_alias(t, &mut type_names, &mut env, &name)?;
        }

        // Register values so they can be used in functions earlier in the module.
        for c in &statements.constants {
            assert_unique_name(&mut value_names, &c.name, c.location)?;
        }
        for f in &statements.functions {
            register_value_from_function(f, &mut value_names, &mut env, &mut hydrators, &name)?;
        }
        for t in &statements.custom_types {
            register_values_from_custom_type(
                t,
                &mut hydrators,
                &mut env,
                &mut value_names,
                &name,
                &t.parameters,
            )?;
        }

        // Infer the types of each statement in the module
        let mut typed_statements = Vec::with_capacity(statements_count);
        for i in statements.imports {
            let statement = record_imported_items_for_use_detection(
                i,
                package.as_str(),
                self.direct_dependencies,
                self.warnings,
                &env,
            )?;
            typed_statements.push(statement);
        }
        for t in statements.custom_types {
            let statement = infer_custom_type(t, &mut env)?;
            typed_statements.push(statement);
        }
        for t in statements.type_aliases {
            let statement = insert_type_alias(t, &mut env)?;
            typed_statements.push(statement);
        }

        // Sort functions and constants into dependency order for inference. Definitions that do
        // not depend on other definitions are inferred first, then ones that depend
        // on those, etc.
        let definition_groups = into_dependency_order(statements.functions, statements.constants)?;
        let mut working_group = vec![];

        for group in definition_groups {
            // A group may have multiple functions that depend on each other through
            // mutual recursion.

            for definition in group {
                match definition {
                    CallGraphNode::Function(f) => {
                        let statement =
                            infer_function(f, &mut env, &mut hydrators, &name, &mut errors);
                        match statement {
                            Ok(statement) => working_group.push(statement),
                            Err(e) => {
                                // TODO: We do this to maintain any constant related errors within the function
                                // Once function inference is continuable this entire match will be removed
                                let mut errs = Vec1::new(e);
                                errs.append(&mut errors);
                                errs.sort_by_key(|e| e.start_location());
                                return Err(InferenceFailure {
                                    ast: None,
                                    errors: errs,
                                });
                            }
                        }
                    }
                    CallGraphNode::ModuleConstant(c) => {
                        let statement = infer_module_constant(c, &mut env, &name, &mut errors);
                        working_group.push(statement);
                    }
                }
            }

            // Now that the entire group has been inferred, generalise their types.
            for inferred in working_group.drain(..) {
                let statement = generalise_statement(inferred, &name, &mut env);
                typed_statements.push(statement);
            }
        }

        // Generate warnings for unused items
        let unused_imports = env.convert_unused_to_warnings();

        // Remove imported types and values to create the public interface
        // Private types and values are retained so they can be used in the language
        // server, but are filtered out when type checking to prevent using private
        // items.
        env.module_types.retain(|_, info| info.module == name);
        env.accessors
            .retain(|_, accessors| accessors.publicity.is_importable());

        // Ensure no exported values have private types in their type signature
        for value in env.module_values.values() {
            if value.publicity.is_private() {
                continue;
            }
            if let Some(leaked) = value.type_.find_private_type() {
                errors.push(Error::PrivateTypeLeak {
                    location: value.variant.definition_location(),
                    leaked,
                });
            }

            // We also want to make sure that no public type exposes internal ones
            // in their type signature.
            if !value.publicity.is_public() {
                continue;
            }
            // We also don't want to raise a warning if we're inside an internal
            // module ourselves, the type wouldn't actually be publicly exposed.
            if self.package_config.is_internal_module(name.as_str()) {
                continue;
            }
        }

        let Environment {
            module_types: types,
            module_types_constructors: types_constructors,
            module_values: values,
            todo_encountered: contains_todo,
            accessors,
            ..
        } = env;

        let is_internal = self.package_config.is_internal_module(name.as_str());

        // Sort the errors by location so that they are easier to debug.
        errors.sort_by_key(|e| e.start_location());

        let ast = ast::Module {
            documentation,
            name: name.clone(),
            definitions: typed_statements,
            type_info: ModuleInterface {
                name,
                types,
                types_value_constructors: types_constructors,
                values,
                accessors,
                origin: self.origin,
                package: self.package_config.name.clone(),
                is_internal,
                unused_imports,
                contains_todo,
                line_numbers,
                src_path,
            },
        };

        match Vec1::try_from_vec(errors) {
            Err(_) => Ok(ast),
            Ok(errors) => Err(InferenceFailure {
                ast: Some(ast),
                errors,
            }),
        }
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

fn register_type_alias(
    t: &TypeAlias<()>,
    names: &mut HashMap<EcoString, SrcSpan>,
    environment: &mut Environment<'_>,
    module: &EcoString,
) -> Result<(), Error> {
    let TypeAlias {
        location,
        publicity,
        parameters: args,
        alias: name,
        type_ast: resolved_type,
        deprecation,
        type_: _,
        documentation,
    } = t;

    // A type alias must not have the same name as any other type in the module.
    assert_unique_type_name(names, name, *location)?;

    // Use the hydrator to convert the AST into a type, erroring if the AST was invalid
    // in some fashion.
    let mut hydrator = Hydrator::new();
    let parameters = make_type_vars(args, location, &mut hydrator, environment)?;
    hydrator.disallow_new_type_variables();
    let typ = hydrator.type_from_ast(resolved_type, environment)?;

    // Insert the alias so that it can be used by other code.
    environment.insert_type_constructor(
        name.clone(),
        TypeConstructor {
            origin: *location,
            module: module.clone(),
            parameters,
            typ,
            deprecation: deprecation.clone(),
            publicity: *publicity,
            documentation: documentation.clone(),
        },
    )?;

    if let Some(name) = hydrator.unused_type_variables().next() {
        return Err(Error::UnusedTypeAliasParameter {
            location: *location,
            name: name.clone(),
        });
    }

    // Register the type for detection of dead code.
    if publicity.is_private() {
        environment.init_usage(name.clone(), EntityKind::PrivateType, *location);
    };
    Ok(())
}

fn register_types_from_custom_type<'a>(
    t: &CustomType<()>,
    names: &mut HashMap<EcoString, SrcSpan>,
    environment: &mut Environment<'a>,
    module: &'a EcoString,
    config: &PackageConfig,
    hydrators: &mut HashMap<EcoString, Hydrator>,
) -> Result<(), Error> {
    let CustomType {
        name,
        publicity,
        parameters,
        location,
        deprecation,
        opaque,
        constructors,
        documentation,
        ..
    } = t;
    assert_unique_type_name(names, name, *location)?;
    let mut hydrator = Hydrator::new();
    let parameters = make_type_vars(parameters, location, &mut hydrator, environment)?;

    hydrator.clear_ridgid_type_names();

    // We check is the type comes from an internal module and restrict its
    // publicity.
    let publicity = match publicity {
        // It's important we only restrict the publicity of public types.
        Publicity::Public if config.is_internal_module(module) => Publicity::Internal,
        // If a type is private we don't want to make it internal just because
        // it comes from an internal module, so in that case the publicity is
        // left unchanged.
        Publicity::Public | Publicity::Private | Publicity::Internal => *publicity,
    };

    let typ = Arc::new(Type::Named {
        publicity,
        package: environment.current_package.clone(),
        module: module.to_owned(),
        name: name.clone(),
        args: parameters.clone(),
    });
    let _ = hydrators.insert(name.clone(), hydrator);
    environment.insert_type_constructor(
        name.clone(),
        TypeConstructor {
            origin: *location,
            module: module.clone(),
            deprecation: deprecation.clone(),
            parameters,
            publicity,
            typ,
            documentation: documentation.clone(),
        },
    )?;

    if *opaque && constructors.is_empty() {
        environment
            .warnings
            .emit(type_::Warning::OpaqueExternalType {
                location: *location,
            });
    }

    if publicity.is_private() {
        environment.init_usage(name.clone(), EntityKind::PrivateType, *location);
    };
    Ok(())
}

fn register_values_from_custom_type(
    t: &CustomType<()>,
    hydrators: &mut HashMap<EcoString, Hydrator>,
    environment: &mut Environment<'_>,
    names: &mut HashMap<EcoString, SrcSpan>,
    module_name: &EcoString,
    type_parameters: &[EcoString],
) -> Result<(), Error> {
    let CustomType {
        location,
        publicity,
        opaque,
        name,
        constructors,
        deprecation,
        ..
    } = t;

    let mut hydrator = hydrators
        .remove(name)
        .expect("Could not find hydrator for register_values custom type");
    hydrator.disallow_new_type_variables();
    let typ = environment
        .module_types
        .get(name)
        .expect("Type for custom type not found in register_values")
        .typ
        .clone();
    if let Some(accessors) = custom_type_accessors(constructors, &mut hydrator, environment)? {
        let map = AccessorsMap {
            publicity: if *opaque {
                Publicity::Private
            } else {
                *publicity
            },
            accessors,
            // TODO: improve the ownership here so that we can use the
            // `return_type_constructor` below rather than looking it up twice.
            type_: typ.clone(),
        };
        environment.insert_accessors(name.clone(), map)
    }

    let mut constructors_data = vec![];

    for (index, constructor) in constructors.iter().enumerate() {
        assert_unique_name(names, &constructor.name, constructor.location)?;

        let mut field_map = FieldMap::new(constructor.arguments.len() as u32);
        let mut args_types = Vec::with_capacity(constructor.arguments.len());
        let mut fields = Vec::with_capacity(constructor.arguments.len());

        for (i, RecordConstructorArg { label, ast, .. }) in constructor.arguments.iter().enumerate()
        {
            // Build a type from the annotation AST
            let t = hydrator.type_from_ast(ast, environment)?;

            fields.push(TypeValueConstructorField { type_: t.clone() });

            // Register the type for this parameter
            args_types.push(t);

            // Register the label for this parameter, if there is one
            if let Some(label) = label {
                field_map
                    .insert(label.clone(), i as u32)
                    .map_err(|_| Error::DuplicateField {
                        label: label.clone(),
                        location: *location,
                    })?;
            }
        }
        let field_map = field_map.into_option();
        // Insert constructor function into module scope
        let typ = match constructor.arguments.len() {
            0 => typ.clone(),
            _ => fn_(args_types.clone(), typ.clone()),
        };
        let constructor_info = ValueConstructorVariant::Record {
            documentation: constructor.documentation.clone(),
            constructors_count: constructors.len() as u16,
            name: constructor.name.clone(),
            arity: constructor.arguments.len() as u16,
            field_map: field_map.clone(),
            location: constructor.location,
            module: module_name.clone(),
            constructor_index: index as u16,
        };

        // If the contructor belongs to an opaque type then it's going to be
        // considered as private.
        let value_constructor_publicity = if *opaque {
            Publicity::Private
        } else {
            *publicity
        };

        environment.insert_module_value(
            constructor.name.clone(),
            ValueConstructor {
                publicity: value_constructor_publicity,
                deprecation: deprecation.clone(),
                type_: typ.clone(),
                variant: constructor_info.clone(),
            },
        );

        if value_constructor_publicity.is_private() {
            environment.init_usage(
                constructor.name.clone(),
                EntityKind::PrivateTypeConstructor(name.clone()),
                constructor.location,
            );
        }

        constructors_data.push(TypeValueConstructor {
            name: constructor.name.clone(),
            parameters: fields,
        });
        environment.insert_variable(
            constructor.name.clone(),
            constructor_info,
            typ,
            value_constructor_publicity,
            deprecation.clone(),
        );
    }

    // Now record the constructors for the type.
    environment.insert_type_to_constructors(
        name.clone(),
        TypeVariantConstructors::new(constructors_data, type_parameters, hydrator),
    );

    Ok(())
}

fn register_value_from_function(
    f: &UntypedFunction,
    names: &mut HashMap<EcoString, SrcSpan>,
    environment: &mut Environment<'_>,
    hydrators: &mut HashMap<EcoString, Hydrator>,
    module_name: &EcoString,
) -> Result<(), Error> {
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
    } = f;
    assert_unique_name(names, name, *location)?;
    assert_valid_javascript_external(name, external_javascript.as_ref(), *location)?;

    let mut builder = FieldMapBuilder::new(args.len() as u32);
    for arg in args.iter() {
        builder.add(arg.names.get_label(), arg.location)?;
    }
    let field_map = builder.finish();
    let mut hydrator = Hydrator::new();

    // When external implementations are present then the type annotations
    // must be given in full, so we disallow holes in the annotations.
    hydrator.permit_holes(external_erlang.is_none() && external_javascript.is_none());

    let arg_types = args
        .iter()
        .map(|arg| hydrator.type_from_option_ast(&arg.annotation, environment))
        .try_collect()?;
    let return_type = hydrator.type_from_option_ast(return_annotation, environment)?;
    let typ = fn_(arg_types, return_type);
    let _ = hydrators.insert(name.clone(), hydrator);

    let external =
        target_function_implementation(environment.target, external_erlang, external_javascript);
    let (impl_module, impl_function) = implementation_names(external, module_name, name);
    let variant = ValueConstructorVariant::ModuleFn {
        documentation: documentation.clone(),
        name: impl_function,
        field_map,
        module: impl_module,
        arity: args.len(),
        location: *location,
        implementations: *implementations,
    };
    environment.insert_variable(name.clone(), variant, typ, *publicity, deprecation.clone());
    if publicity.is_private() {
        environment.init_usage(name.clone(), EntityKind::PrivateFunction, *location);
    };
    Ok(())
}

fn assert_valid_javascript_external(
    function_name: &EcoString,
    external_javascript: Option<&(EcoString, EcoString)>,
    location: SrcSpan,
) -> Result<(), Error> {
    use regex::Regex;

    static MODULE: OnceLock<Regex> = OnceLock::new();
    static FUNCTION: OnceLock<Regex> = OnceLock::new();

    let (module, function) = match external_javascript {
        None => return Ok(()),
        Some(external) => external,
    };
    if !MODULE
        .get_or_init(|| Regex::new("^[a-zA-Z0-9\\./:_-]+$").expect("regex"))
        .is_match(module)
    {
        return Err(Error::InvalidExternalJavascriptModule {
            location,
            module: module.clone(),
            name: function_name.clone(),
        });
    }
    if !FUNCTION
        .get_or_init(|| Regex::new("^[a-zA-Z_][a-zA-Z0-9_]*$").expect("regex"))
        .is_match(function)
    {
        return Err(Error::InvalidExternalJavascriptFunction {
            location,
            function: function.clone(),
            name: function_name.clone(),
        });
    }
    Ok(())
}

fn infer_function(
    f: UntypedFunction,
    environment: &mut Environment<'_>,
    hydrators: &mut HashMap<EcoString, Hydrator>,
    module_name: &EcoString,
    errors: &mut Vec<Error>,
) -> Result<TypedDefinition, Error> {
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
    } = f;
    let target = environment.target;
    let preregistered_fn = environment
        .get_variable(&name)
        .expect("Could not find preregistered type for function");
    let field_map = preregistered_fn.field_map().cloned();
    let preregistered_type = preregistered_fn.type_.clone();
    let (args_types, return_type) = preregistered_type
        .fn_types()
        .expect("Preregistered type for fn was not a fn");

    // Find the external implementation for the current target, if one has been given.
    let external = target_function_implementation(target, &external_erlang, &external_javascript);
    let (impl_module, impl_function) = implementation_names(external, module_name, &name);

    // The function must have at least one implementation somewhere.
    ensure_function_has_an_implementation(&body, &external_erlang, &external_javascript, location)?;

    if external.is_some() {
        // There was an external implementation, so type annotations are
        // mandatory as the Gleam implementation may be absent, and because we
        // think you should always specify types for external functions for
        // clarity + to avoid accidental mistakes.
        ensure_annotations_present(&arguments, return_annotation.as_ref(), location)?;
    }

    let definition = FunctionDefinition {
        has_body: !body.first().is_placeholder(),
        has_erlang_external: external_erlang.is_some(),
        has_javascript_external: external_javascript.is_some(),
    };

    // Infer the type using the preregistered args + return types as a starting point
    let (type_, args, body, implementations) = environment.in_new_scope(|environment| {
        let args_types = arguments
            .into_iter()
            .zip(&args_types)
            .map(|(a, t)| a.set_type(t.clone()))
            .collect();
        let mut expr_typer = ExprTyper::new(environment, definition, errors);
        expr_typer.hydrator = hydrators
            .remove(&name)
            .expect("Could not find hydrator for fn");

        let (args, body) =
            expr_typer.infer_fn_with_known_types(args_types, body, Some(return_type))?;
        let args_types = args.iter().map(|a| a.type_.clone()).collect();
        let typ = fn_(args_types, body.last().type_());
        Ok((typ, args, body, expr_typer.implementations))
    })?;

    // Assert that the inferred type matches the type of any recursive call
    unify(preregistered_type, type_.clone()).map_err(|e| convert_unify_error(e, location))?;

    // Ensure that the current target has an implementation for the function.
    // This is done at the expression level while inferring the function body, but we do it again
    // here as externally implemented functions may not have a Gleam body.
    if publicity.is_importable()
        && environment.target_support.is_enforced()
        && !implementations.supports(target)
    {
        return Err(Error::UnsupportedPublicFunctionTarget {
            name: name.clone(),
            target,
            location,
        });
    }

    let variant = ValueConstructorVariant::ModuleFn {
        documentation: doc.clone(),
        name: impl_function,
        field_map,
        module: impl_module,
        arity: args.len(),
        location,
        implementations,
    };

    environment.insert_variable(
        name.clone(),
        variant,
        type_.clone(),
        publicity,
        deprecation.clone(),
    );

    Ok(Definition::Function(Function {
        documentation: doc,
        location,
        name,
        publicity,
        deprecation,
        arguments: args,
        end_position: end_location,
        return_annotation,
        return_type: type_
            .return_type()
            .expect("Could not find return type for fn"),
        body,
        external_erlang,
        external_javascript,
        implementations,
    }))
}

/// Returns the module name and function name of the implementation of a
/// function. If the function is implemented as a Gleam function then it is the
/// same as the name of the module and function. If the function has an external
/// implementation then it is the name of the external module and function.
fn implementation_names(
    external: &Option<(EcoString, EcoString)>,
    module_name: &EcoString,
    name: &EcoString,
) -> (EcoString, EcoString) {
    match external {
        None => (module_name.clone(), name.clone()),
        Some((m, f)) => (m.clone(), f.clone()),
    }
}

fn target_function_implementation<'a>(
    target: Target,
    external_erlang: &'a Option<(EcoString, EcoString)>,
    external_javascript: &'a Option<(EcoString, EcoString)>,
) -> &'a Option<(EcoString, EcoString)> {
    match target {
        Target::Erlang => external_erlang,
        Target::JavaScript => external_javascript,
    }
}

fn ensure_function_has_an_implementation(
    body: &Vec1<UntypedStatement>,
    external_erlang: &Option<(EcoString, EcoString)>,
    external_javascript: &Option<(EcoString, EcoString)>,
    location: SrcSpan,
) -> Result<(), Error> {
    match (external_erlang, external_javascript) {
        (None, None) if body.first().is_placeholder() => Err(Error::NoImplementation { location }),
        _ => Ok(()),
    }
}

fn ensure_annotations_present(
    arguments: &[UntypedArg],
    return_annotation: Option<&TypeAst>,
    location: SrcSpan,
) -> Result<(), Error> {
    for arg in arguments {
        if arg.annotation.is_none() {
            return Err(Error::ExternalMissingAnnotation {
                location: arg.location,
                kind: MissingAnnotation::Parameter,
            });
        }
    }
    if return_annotation.is_none() {
        return Err(Error::ExternalMissingAnnotation {
            location,
            kind: MissingAnnotation::Return,
        });
    }
    Ok(())
}

fn insert_type_alias(
    t: TypeAlias<()>,
    environment: &mut Environment<'_>,
) -> Result<TypedDefinition, Error> {
    let TypeAlias {
        documentation: doc,
        location,
        publicity,
        alias,
        parameters: args,
        type_ast: resolved_type,
        deprecation,
        ..
    } = t;
    let typ = environment
        .get_type_constructor(&None, &alias)
        .expect("Could not find existing type for type alias")
        .typ
        .clone();
    Ok(Definition::TypeAlias(TypeAlias {
        documentation: doc,
        location,
        publicity,
        alias,
        parameters: args,
        type_ast: resolved_type,
        type_: typ,
        deprecation,
    }))
}

fn infer_custom_type(
    t: CustomType<()>,
    environment: &mut Environment<'_>,
) -> Result<TypedDefinition, Error> {
    let CustomType {
        documentation: doc,
        location,
        end_position,
        publicity,
        opaque,
        name,
        parameters,
        constructors,
        deprecation,
        ..
    } = t;
    let constructors = constructors
        .into_iter()
        .map(
            |RecordConstructor {
                 location,
                 name,
                 arguments: args,
                 documentation,
             }| {
                let preregistered_fn = environment
                    .get_variable(&name)
                    .expect("Could not find preregistered type for function");
                let preregistered_type = preregistered_fn.type_.clone();

                let args = if let Some((args_types, _return_type)) = preregistered_type.fn_types() {
                    args.into_iter()
                        .zip(&args_types)
                        .map(
                            |(
                                RecordConstructorArg {
                                    label,
                                    ast,
                                    location,
                                    ..
                                },
                                t,
                            )| {
                                RecordConstructorArg {
                                    label,
                                    ast,
                                    location,
                                    type_: t.clone(),
                                    doc: None,
                                }
                            },
                        )
                        .collect()
                } else {
                    vec![]
                };

                RecordConstructor {
                    location,
                    name,
                    arguments: args,
                    documentation,
                }
            },
        )
        .collect();
    let typed_parameters = environment
        .get_type_constructor(&None, &name)
        .expect("Could not find preregistered type constructor ")
        .parameters
        .clone();

    Ok(Definition::CustomType(CustomType {
        documentation: doc,
        location,
        end_position,
        publicity,
        opaque,
        name,
        parameters,
        constructors,
        typed_parameters,
        deprecation,
    }))
}

fn record_imported_items_for_use_detection<A>(
    i: Import<()>,
    current_package: &str,
    direct_dependencies: &HashMap<EcoString, A>,
    warnings: &TypeWarningEmitter,
    environment: &Environment<'_>,
) -> Result<TypedDefinition, Error> {
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
    let module_info =
        environment
            .importable_modules
            .get(&module)
            .ok_or_else(|| Error::UnknownModule {
                location,
                name: module.clone(),
                imported_modules: environment.imported_modules.keys().cloned().collect(),
            })?;

    // Modules should belong to a package that is a direct dependency of the
    // current package to be imported.
    // Upgrade this to an error in future.
    if module_info.package != GLEAM_CORE_PACKAGE_NAME
        && module_info.package != current_package
        && !direct_dependencies.contains_key(&module_info.package)
    {
        warnings.emit(type_::Warning::TransitiveDependencyImported {
            location,
            module: module_info.name.clone(),
            package: module_info.package.clone(),
        })
    }

    Ok(Definition::Import(Import {
        documentation,
        location,
        module,
        as_name,
        unqualified_values,
        unqualified_types,
        package: module_info.package.clone(),
    }))
}

fn infer_module_constant(
    c: ModuleConstant<(), ()>,
    environment: &mut Environment<'_>,
    module_name: &EcoString,
    errors: &mut Vec<Error>,
) -> TypedDefinition {
    let ModuleConstant {
        documentation: doc,
        location,
        name,
        annotation,
        publicity,
        value,
        deprecation,
        ..
    } = c;

    let mut expr_typer = ExprTyper::new(
        environment,
        FunctionDefinition {
            has_body: true,
            has_erlang_external: false,
            has_javascript_external: false,
        },
        errors,
    );
    let typed_expr = expr_typer.infer_const(&annotation, *value);
    let type_ = typed_expr.type_();
    let implementations = expr_typer.implementations;

    let variant = ValueConstructor {
        publicity,
        deprecation: deprecation.clone(),
        variant: ValueConstructorVariant::ModuleConstant {
            documentation: doc.clone(),
            location,
            literal: typed_expr.clone(),
            module: module_name.clone(),
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

    if publicity.is_private() {
        environment.init_usage(name.clone(), EntityKind::PrivateConstant, location);
    }

    Definition::ModuleConstant(ModuleConstant {
        documentation: doc,
        location,
        name,
        annotation,
        publicity,
        value: Box::new(typed_expr),
        type_,
        deprecation,
        implementations,
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
        annotation,
        publicity,
        value,
        type_,
        deprecation,
        implementations,
    } = constant;
    let typ = type_.clone();
    let type_ = type_::generalise(typ);
    let variant = ValueConstructorVariant::ModuleConstant {
        documentation: doc.clone(),
        location,
        literal: *value.clone(),
        module: module_name.clone(),
        implementations,
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
    } = function;

    // Lookup the inferred function information
    let function = environment
        .get_variable(&name)
        .expect("Could not find preregistered type for function");
    let field_map = function.field_map().cloned();
    let typ = function.type_.clone();

    let type_ = type_::generalise(typ);

    // Insert the function into the module's interface
    let external =
        target_function_implementation(environment.target, &external_erlang, &external_javascript);
    let (impl_module, impl_function) = implementation_names(external, module_name, &name);

    let variant = ValueConstructorVariant::ModuleFn {
        documentation: doc.clone(),
        name: impl_function,
        field_map,
        module: impl_module,
        arity: args.len(),
        location,
        implementations,
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
        name,
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
    })
}

fn make_type_vars(
    args: &[EcoString],
    location: &SrcSpan,
    hydrator: &mut Hydrator,
    environment: &mut Environment<'_>,
) -> Result<Vec<Arc<Type>>, Error> {
    args.iter()
        .map(|name| {
            hydrator.add_type_variable(name, environment).map_err(|()| {
                Error::DuplicateTypeParameter {
                    location: *location,
                    name: name.clone(),
                }
            })
        })
        .collect::<Result<_, _>>()
}

fn assert_unique_type_name(
    names: &mut HashMap<EcoString, SrcSpan>,
    name: &EcoString,
    location: SrcSpan,
) -> Result<(), Error> {
    match names.insert(name.clone(), location) {
        Some(previous_location) => Err(Error::DuplicateTypeName {
            name: name.clone(),
            previous_location,
            location,
        }),
        None => Ok(()),
    }
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

fn custom_type_accessors<A>(
    constructors: &[RecordConstructor<A>],
    hydrator: &mut Hydrator,
    environment: &mut Environment<'_>,
) -> Result<Option<HashMap<EcoString, RecordAccessor>>, Error> {
    let args = get_compatible_record_fields(constructors);

    let mut fields = HashMap::with_capacity(args.len());
    hydrator.disallow_new_type_variables();
    for (index, label, ast) in args {
        let typ = hydrator.type_from_ast(ast, environment)?;
        let _ = fields.insert(
            label.clone(),
            RecordAccessor {
                index: index as u64,
                label: label.clone(),
                type_: typ,
            },
        );
    }
    Ok(Some(fields))
}

/// Returns the fields that have the same label and type across all variants of
/// the given type.
fn get_compatible_record_fields<A>(
    constructors: &[RecordConstructor<A>],
) -> Vec<(usize, &EcoString, &TypeAst)> {
    let mut compatible = vec![];

    let first = match constructors.first() {
        Some(first) => first,
        None => return compatible,
    };

    'next_argument: for (index, first_argument) in first.arguments.iter().enumerate() {
        // Fields without labels do not have accessors
        let label = match first_argument.label.as_ref() {
            Some(label) => label,
            None => continue 'next_argument,
        };

        // Check each variant to see if they have an field in the same position
        // with the same label and the same type
        for constructor in constructors.iter().skip(1) {
            // The field must exist in all variants
            let argument = match constructor.arguments.get(index) {
                Some(argument) => argument,
                None => continue 'next_argument,
            };

            // The labels must be the same
            if argument.label != first_argument.label {
                continue 'next_argument;
            }

            // The types must be the same
            if !argument.ast.is_logically_equal(&first_argument.ast) {
                continue 'next_argument;
            }
        }

        // The previous loop did not find any incompatible fields in the other
        // variants so this field is compatible across variants and we should
        // generate an accessor for it.
        compatible.push((index, label, &first_argument.ast))
    }

    compatible
}

/// Given a type, return a list of all the types it depends on
fn get_type_dependencies(typ: &TypeAst) -> Vec<EcoString> {
    let mut deps = Vec::with_capacity(1);

    match typ {
        TypeAst::Var(TypeAstVar { .. }) => (),
        TypeAst::Hole(TypeAstHole { .. }) => (),
        TypeAst::Constructor(TypeAstConstructor {
            name,
            arguments,
            module,
            ..
        }) => {
            deps.push(match module {
                Some(module) => format!("{}.{}", name, module).into(),
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
        TypeAst::Tuple(TypeAstTuple { elems, .. }) => {
            for elem in elems {
                deps.extend(get_type_dependencies(elem))
            }
        }
    }

    deps
}

fn sorted_type_aliases(aliases: &Vec<TypeAlias<()>>) -> Result<Vec<&TypeAlias<()>>, Error> {
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
