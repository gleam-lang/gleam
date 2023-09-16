#[cfg(test)]
mod tests;

use crate::ast::{UntypedArg, UntypedStatement};
use crate::type_::error::MissingAnnotation;
use crate::type_::Deprecation;
use crate::{
    ast::{
        self, BitStringSegmentOption, CustomType, Definition, DefinitionLocation, Function,
        GroupedStatements, Import, Layer, ModuleConstant, RecordConstructor, RecordConstructorArg,
        SrcSpan, TypeAlias, TypeAst, TypedDefinition, TypedModule, UnqualifiedImport,
        UntypedModule,
    },
    build::{Origin, Target},
    call_graph::into_dependency_order,
    type_::{
        self,
        environment::*,
        error::{convert_unify_error, Error},
        expression::ExprTyper,
        fields::{FieldMap, FieldMapBuilder},
        hydrator::Hydrator,
        prelude::*,
        AccessorsMap, ModuleInterface, PatternConstructor, RecordAccessor, Type, TypeConstructor,
        ValueConstructor, ValueConstructorVariant,
    },
    uid::UniqueIdGenerator,
    warning::TypeWarningEmitter,
};
use crate::{dep_tree, GLEAM_CORE_PACKAGE_NAME};
use itertools::Itertools;
use smol_str::SmolStr;
use std::{collections::HashMap, sync::Arc};
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

// TODO: This takes too many arguments.
#[allow(clippy::too_many_arguments)]
/// Crawl the AST, annotating each node with the inferred type or
/// returning an error.
///
pub fn infer_module<A>(
    target: Target,
    ids: &UniqueIdGenerator,
    mut module: UntypedModule,
    origin: Origin,
    package: &SmolStr,
    modules: &im::HashMap<SmolStr, ModuleInterface>,
    warnings: &TypeWarningEmitter,
    direct_dependencies: &HashMap<SmolStr, A>,
) -> Result<TypedModule, Error> {
    let name = module.name.clone();
    let documentation = std::mem::take(&mut module.documentation);
    let mut env = Environment::new(ids.clone(), &name, target, modules, warnings);
    validate_module_name(&name)?;

    let mut type_names = HashMap::with_capacity(module.definitions.len());
    let mut value_names = HashMap::with_capacity(module.definitions.len());
    let mut hydrators = HashMap::with_capacity(module.definitions.len());

    let statements = GroupedStatements::new(module.into_iter_statements(target));
    let statements_count = statements.len();

    // Register any modules, types, and values being imported
    // We process imports first so that anything imported can be referenced
    // anywhere in the module.
    // TODO: Extract an ImportRegistrar class to perform this.
    for s in &statements.imports {
        register_import(s, &name, origin, &mut env)?;
    }

    // Register types so they can be used in constructors and functions
    // earlier in the module.
    for t in &statements.custom_types {
        register_types_from_custom_type(t, &mut type_names, &mut env, &name, &mut hydrators)?;
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
        register_values_from_custom_type(t, &mut hydrators, &mut env, &mut value_names, &name)?;
    }

    // Infer the types of each statement in the module
    let mut typed_statements = Vec::with_capacity(statements_count);
    for i in statements.imports {
        let statement = record_imported_items_for_use_detection(
            i,
            package,
            direct_dependencies,
            warnings,
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
    for c in statements.constants {
        let statement = infer_module_constant(c, &mut env, &name)?;
        typed_statements.push(statement);
    }

    // Sort the functions into dependency order for inference. Functions that do
    // not depend on other functions are inferred first, then ones that depend
    // on those, etc.
    let function_groups = into_dependency_order(statements.functions)?;
    let mut working_group = vec![];

    for group in function_groups {
        // A group may have multiple functions that depend on each other through
        // mutual recursion.

        for function in group {
            let inferred = infer_function(function, &mut env, &mut hydrators, &name)?;
            working_group.push(inferred);
        }

        // Now that the entire group has been inferred, generalise their types.
        for inferred in working_group.drain(..) {
            let statement = generalise_statement(inferred, &name, &mut env);
            typed_statements.push(statement);
        }
    }

    // Generate warnings for unused items
    env.convert_unused_to_warnings();

    // Remove imported types and values to create the public interface
    // Private types and values are retained so they can be used in the language
    // server, but are filtered out when type checking to prevent using private
    // items.
    env.module_types.retain(|_, info| info.module == name);
    env.accessors.retain(|_, accessors| accessors.public);

    // Ensure no exported values have private types in their type signature
    for value in env.module_values.values() {
        if !value.public {
            continue;
        }
        if let Some(leaked) = value.type_.find_private_type() {
            return Err(Error::PrivateTypeLeak {
                location: value.variant.definition_location(),
                leaked,
            });
        }
    }

    let Environment {
        module_types: types,
        module_types_constructors: types_constructors,
        module_values: values,
        accessors,
        ..
    } = env;

    Ok(ast::Module {
        documentation,
        name: name.clone(),
        definitions: typed_statements,
        type_info: ModuleInterface {
            name,
            types,
            types_constructors,
            values,
            accessors,
            origin,
            package: package.clone(),
        },
    })
}

pub fn register_import(
    import: &Import<()>,
    current_module: &str,
    origin: Origin,
    environment: &mut Environment<'_>,
) -> Result<(), Error> {
    // Determine local alias of imported module
    let module_name = import.used_name();

    let Import {
        module,
        unqualified,
        location,
        discarded,
        ..
    } = import;
    let name = module.clone();
    // Find imported module
    let module_info =
        environment
            .importable_modules
            .get(&name)
            .ok_or_else(|| Error::UnknownModule {
                location: *location,
                name: name.clone(),
                imported_modules: environment.imported_modules.keys().cloned().collect(),
            })?;

    // Modules in `src/` cannot import modules from `test/`
    if origin.is_src() && !module_info.origin.is_src() {
        return Err(Error::SrcImportingTest {
            location: *location,
            src_module: current_module.into(),
            test_module: name,
        });
    }

    // Insert unqualified imports into scope
    for UnqualifiedImport {
        name,
        location,
        as_name,
        ..
    } in unqualified
    {
        let mut type_imported = false;
        let mut value_imported = false;
        let mut variant = None;

        let imported_name = as_name.as_ref().unwrap_or(name);

        // Check if value already was imported
        if let Some(previous) = environment.unqualified_imported_names.get(imported_name) {
            return Err(Error::DuplicateImport {
                location: *location,
                previous_location: *previous,
                name: name.clone(),
            });
        }

        // Register the name as imported so it can't be imported a
        // second time in future
        let _ = environment
            .unqualified_imported_names
            .insert(imported_name.clone(), *location);

        // Register the unqualified import if it is a value
        if let Some(value) = module_info.get_public_value(name) {
            environment.insert_variable(
                imported_name.clone(),
                value.variant.clone(),
                value.type_.clone(),
                true,
                Deprecation::NotDeprecated,
            );
            variant = Some(&value.variant);
            value_imported = true;
        }

        // Register the unqualified import if it is a type constructor
        if let Some(typ) = module_info.get_public_type(name) {
            let typ_info = TypeConstructor {
                origin: *location,
                ..typ.clone()
            };
            match environment.insert_type_constructor(imported_name.clone(), typ_info) {
                Ok(_) => (),
                Err(e) => return Err(e),
            };

            type_imported = true;
        }

        if value_imported && type_imported {
            environment.init_usage(
                imported_name.clone(),
                EntityKind::ImportedTypeAndConstructor,
                *location,
            );
        } else if type_imported {
            let _ = environment.imported_types.insert(imported_name.clone());
            environment.init_usage(imported_name.clone(), EntityKind::ImportedType, *location);
        } else if value_imported {
            match variant {
                Some(&ValueConstructorVariant::Record { .. }) => environment.init_usage(
                    imported_name.clone(),
                    EntityKind::ImportedConstructor,
                    *location,
                ),
                _ => environment.init_usage(
                    imported_name.clone(),
                    EntityKind::ImportedValue,
                    *location,
                ),
            };
        } else if !value_imported {
            // Error if no type or value was found with that name
            return Err(Error::UnknownModuleField {
                location: *location,
                name: name.clone(),
                module_name: module.clone(),
                value_constructors: module_info.public_value_names(),
                type_constructors: module_info.public_type_names(),
            });
        }
    }

    if unqualified.is_empty() {
        // When the module has no unqualified imports, we track its usage
        // so we can warn if not used by the end of the type checking
        if !discarded {
            let _ = environment
                .unused_modules
                .insert(module_name.clone(), *location);
        }
    }

    if !discarded {
        // Check if a module was already imported with this name
        if let Some((previous_location, _)) = environment.imported_modules.get(&module_name) {
            return Err(Error::DuplicateImport {
                location: *location,
                previous_location: *previous_location,
                name: module_name.clone(),
            });
        }

        // Register the name as imported so it can't be imported a
        // second time in future
        let _ = environment
            .unqualified_imported_names
            .insert(module_name.clone(), *location);

        // Insert imported module into scope
        let _ = environment
            .imported_modules
            .insert(module_name, (*location, module_info));
    }

    Ok(())
}

fn validate_module_name(name: &SmolStr) -> Result<(), Error> {
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
    names: &mut HashMap<SmolStr, SrcSpan>,
    environment: &mut Environment<'_>,
    module: &SmolStr,
) -> Result<(), Error> {
    let TypeAlias {
        location,
        public,
        parameters: args,
        alias: name,
        type_ast: resolved_type,
        ..
    } = t;
    assert_unique_type_name(names, name, *location)?;
    let mut hydrator = Hydrator::new();
    let parameters = make_type_vars(args, location, &mut hydrator, environment)?;
    hydrator.disallow_new_type_variables();
    let typ = hydrator.type_from_ast(resolved_type, environment)?;
    environment.insert_type_constructor(
        name.clone(),
        TypeConstructor {
            origin: *location,
            module: module.clone(),
            public: *public,
            parameters,
            typ,
        },
    )?;
    if !public {
        environment.init_usage(name.clone(), EntityKind::PrivateType, *location);
    };
    Ok(())
}

fn register_types_from_custom_type<'a>(
    t: &CustomType<()>,
    names: &mut HashMap<SmolStr, SrcSpan>,
    environment: &mut Environment<'a>,
    module: &'a SmolStr,
    hydrators: &mut HashMap<SmolStr, Hydrator>,
) -> Result<(), Error> {
    let CustomType {
        name,
        public,
        parameters,
        location,
        constructors,
        ..
    } = t;
    assert_unique_type_name(names, name, *location)?;
    let mut hydrator = Hydrator::new();
    let parameters = make_type_vars(parameters, location, &mut hydrator, environment)?;

    hydrator.clear_ridgid_type_names();

    let typ = Arc::new(Type::App {
        public: *public,
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
            public: *public,
            parameters,
            typ,
        },
    )?;
    let constructor_names = constructors.iter().map(|c| c.name.clone()).collect();
    environment.insert_type_to_constructors(name.clone(), constructor_names);
    if !public {
        environment.init_usage(name.clone(), EntityKind::PrivateType, *location);
    };
    Ok(())
}

fn register_values_from_custom_type(
    t: &CustomType<()>,
    hydrators: &mut HashMap<SmolStr, Hydrator>,
    environment: &mut Environment<'_>,
    names: &mut HashMap<SmolStr, SrcSpan>,
    module_name: &SmolStr,
) -> Result<(), Error> {
    let CustomType {
        location,
        public,
        opaque,
        name,
        constructors,
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
            public: (*public && !*opaque),
            accessors,
            // TODO: improve the ownership here so that we can use the
            // `return_type_constructor` below rather than looking it up twice.
            type_: typ.clone(),
        };
        environment.insert_accessors(name.clone(), map)
    }
    for constructor in constructors {
        assert_unique_name(names, &constructor.name, constructor.location)?;

        let mut field_map = FieldMap::new(constructor.arguments.len() as u32);
        let mut args_types = Vec::with_capacity(constructor.arguments.len());
        for (i, RecordConstructorArg { label, ast, .. }) in constructor.arguments.iter().enumerate()
        {
            let t = hydrator.type_from_ast(ast, environment)?;
            args_types.push(t);
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
            _ => fn_(args_types, typ.clone()),
        };
        let constructor_info = ValueConstructorVariant::Record {
            documentation: constructor.documentation.clone(),
            constructors_count: constructors.len() as u16,
            name: constructor.name.clone(),
            arity: constructor.arguments.len() as u16,
            field_map: field_map.clone(),
            location: constructor.location,
            module: module_name.clone(),
        };

        environment.insert_module_value(
            constructor.name.clone(),
            ValueConstructor {
                deprecation: Deprecation::NotDeprecated,
                public: *public && !opaque,
                type_: typ.clone(),
                variant: constructor_info.clone(),
            },
        );

        if !public {
            environment.init_usage(
                constructor.name.clone(),
                EntityKind::PrivateTypeConstructor(name.clone()),
                constructor.location,
            );
        }

        environment.insert_variable(
            constructor.name.clone(),
            constructor_info,
            typ,
            *public,
            Deprecation::NotDeprecated,
        );
    }
    Ok(())
}

fn register_value_from_function(
    f: &Function<(), ast::UntypedExpr>,
    names: &mut HashMap<SmolStr, SrcSpan>,
    environment: &mut Environment<'_>,
    hydrators: &mut HashMap<SmolStr, Hydrator>,
    module_name: &SmolStr,
) -> Result<(), Error> {
    let Function {
        name,
        arguments: args,
        location,
        return_annotation,
        public,
        documentation,
        external_erlang,
        external_javascript,
        deprecation,
        end_position: _,
        body: _,
        return_type: _,
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
    };
    environment.insert_variable(name.clone(), variant, typ, *public, deprecation.clone());
    if !public {
        environment.init_usage(name.clone(), EntityKind::PrivateFunction, *location);
    };
    Ok(())
}

fn assert_valid_javascript_external(
    function_name: &SmolStr,
    external_javascript: Option<&(SmolStr, SmolStr)>,
    location: SrcSpan,
) -> Result<(), Error> {
    use regex::Regex;
    lazy_static::lazy_static! {
        static ref MODULE: Regex = Regex::new("^[a-zA-Z0-9\\./:_-]+$").expect("regex");
        static ref FUNCTION: Regex = Regex::new("^[a-zA-Z_][a-zA-Z0-9_]*$").expect("regex");
    }

    let (module, function) = match external_javascript {
        None => return Ok(()),
        Some(external) => external,
    };
    if !MODULE.is_match(module) {
        return Err(Error::InvalidExternalJavascriptModule {
            location,
            module: module.clone(),
            name: function_name.clone(),
        });
    }
    if !FUNCTION.is_match(function) {
        return Err(Error::InvalidExternalJavascriptFunction {
            location,
            function: function.clone(),
            name: function_name.clone(),
        });
    }
    Ok(())
}

fn infer_function(
    f: Function<(), ast::UntypedExpr>,
    environment: &mut Environment<'_>,
    hydrators: &mut HashMap<SmolStr, Hydrator>,
    module_name: &SmolStr,
) -> Result<TypedDefinition, Error> {
    let Function {
        documentation: doc,
        location,
        name,
        public,
        arguments,
        body,
        return_annotation,
        end_position: end_location,
        deprecation,
        external_erlang,
        external_javascript,
        return_type: (),
    } = f;
    let preregistered_fn = environment
        .get_variable(&name)
        .expect("Could not find preregistered type for function");
    let field_map = preregistered_fn.field_map().cloned();
    let preregistered_type = preregistered_fn.type_.clone();
    let (args_types, return_type) = preregistered_type
        .fn_types()
        .expect("Preregistered type for fn was not a fn");

    // Find the external implementation for the current target, if one has been given.
    let external =
        target_function_implementation(environment.target, &external_erlang, &external_javascript);
    let (impl_module, impl_function) = implementation_names(external, module_name, &name);

    if external.is_some() {
        // There was an external implementation, so type annotations are
        // mandatory as the Gleam implementation may be absent, and because we
        // think you should always specify types for external functions for
        // clarity + to avoid accidental mistakes.
        ensure_annotations_present(&arguments, return_annotation.as_ref(), location)?;
    } else {
        // There was no external implementation, so a Gleam one must be given.
        ensure_body_given(&body, location)?;
    }

    // Infer the type using the preregistered args + return types as a starting point
    let (type_, args, body) = environment.in_new_scope(|environment| {
        let args_types = arguments
            .into_iter()
            .zip(&args_types)
            .map(|(a, t)| a.set_type(t.clone()))
            .collect();
        let mut expr_typer = ExprTyper::new(environment);
        expr_typer.hydrator = hydrators
            .remove(&name)
            .expect("Could not find hydrator for fn");

        let (args, body) =
            expr_typer.infer_fn_with_known_types(args_types, body, Some(return_type))?;
        let args_types = args.iter().map(|a| a.type_.clone()).collect();
        let typ = fn_(args_types, body.last().type_());
        Ok((typ, args, body))
    })?;

    // Assert that the inferred type matches the type of any recursive call
    unify(preregistered_type, type_.clone()).map_err(|e| convert_unify_error(e, location))?;

    let variant = ValueConstructorVariant::ModuleFn {
        documentation: doc.clone(),
        name: impl_function,
        field_map,
        module: impl_module,
        arity: args.len(),
        location,
    };
    environment.insert_variable(
        name.clone(),
        variant,
        type_.clone(),
        public,
        deprecation.clone(),
    );

    Ok(Definition::Function(Function {
        documentation: doc,
        location,
        name,
        public,
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
    }))
}

/// Returns the the module name and function name of the implementation of a
/// function. If the function is implemented as a Gleam function then it is the
/// same as the name of the module and function. If the function has an external
/// implementation then it is the name of the external module and function.
fn implementation_names(
    external: &Option<(SmolStr, SmolStr)>,
    module_name: &SmolStr,
    name: &SmolStr,
) -> (SmolStr, SmolStr) {
    match external {
        None => (module_name.clone(), name.clone()),
        Some((m, f)) => (m.clone(), f.clone()),
    }
}

fn target_function_implementation<'a>(
    target: Target,
    external_erlang: &'a Option<(SmolStr, SmolStr)>,
    external_javascript: &'a Option<(SmolStr, SmolStr)>,
) -> &'a Option<(SmolStr, SmolStr)> {
    match target {
        Target::Erlang => external_erlang,
        Target::JavaScript => external_javascript,
    }
}

fn ensure_body_given(body: &Vec1<UntypedStatement>, location: SrcSpan) -> Result<(), Error> {
    if body.first().is_placeholder() {
        Err(Error::NoImplementation { location })
    } else {
        Ok(())
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
        public,
        alias,
        parameters: args,
        type_ast: resolved_type,
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
        public,
        alias,
        parameters: args,
        type_ast: resolved_type,
        type_: typ,
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
        public,
        opaque,
        name,
        parameters,
        constructors,
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
        public,
        opaque,
        name,
        parameters,
        constructors,
        typed_parameters,
    }))
}

fn record_imported_items_for_use_detection<A>(
    i: Import<()>,
    current_package: &str,
    direct_dependencies: &HashMap<SmolStr, A>,
    warnings: &TypeWarningEmitter,
    environment: &Environment<'_>,
) -> Result<TypedDefinition, Error> {
    let Import {
        documentation,
        location,
        module,
        as_name,
        mut unqualified,
        discarded,
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
    // Record any imports that are types only as this information is
    // needed to prevent types being imported in generated JavaScript
    for import in unqualified.iter_mut() {
        if environment.imported_types.contains(import.variable_name()) {
            import.layer = Layer::Type;
        }
    }

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
        unqualified,
        discarded,
        package: module_info.package.clone(),
    }))
}

fn infer_module_constant(
    c: ModuleConstant<(), ()>,
    environment: &mut Environment<'_>,
    module_name: &SmolStr,
) -> Result<TypedDefinition, Error> {
    let ModuleConstant {
        documentation: doc,
        location,
        name,
        annotation,
        public,
        value,
        ..
    } = c;
    let typed_expr = ExprTyper::new(environment).infer_const(&annotation, *value)?;
    let type_ = typed_expr.type_();
    let variant = ValueConstructor {
        public,
        deprecation: Deprecation::NotDeprecated,
        variant: ValueConstructorVariant::ModuleConstant {
            documentation: doc.clone(),
            location,
            literal: typed_expr.clone(),
            module: module_name.clone(),
        },
        type_: type_.clone(),
    };

    environment.insert_variable(
        name.clone(),
        variant.variant.clone(),
        type_.clone(),
        public,
        Deprecation::NotDeprecated,
    );
    environment.insert_module_value(name.clone(), variant);

    if !public {
        environment.init_usage(name.clone(), EntityKind::PrivateConstant, location);
    }

    Ok(Definition::ModuleConstant(ModuleConstant {
        documentation: doc,
        location,
        name,
        annotation,
        public,
        value: Box::new(typed_expr),
        type_,
    }))
}

pub fn infer_bit_string_segment_option<UntypedValue, TypedValue, Typer>(
    segment_option: BitStringSegmentOption<UntypedValue>,
    mut type_check: Typer,
) -> Result<BitStringSegmentOption<TypedValue>, Error>
where
    Typer: FnMut(UntypedValue, Arc<Type>) -> Result<TypedValue, Error>,
{
    match segment_option {
        BitStringSegmentOption::Size {
            value,
            location,
            short_form,
            ..
        } => {
            let value = type_check(*value, int())?;
            Ok(BitStringSegmentOption::Size {
                location,
                short_form,
                value: Box::new(value),
            })
        }

        BitStringSegmentOption::Unit { location, value } => {
            Ok(BitStringSegmentOption::Unit { location, value })
        }

        BitStringSegmentOption::Binary { location } => {
            Ok(BitStringSegmentOption::Binary { location })
        }
        BitStringSegmentOption::Int { location } => Ok(BitStringSegmentOption::Int { location }),
        BitStringSegmentOption::Float { location } => {
            Ok(BitStringSegmentOption::Float { location })
        }
        BitStringSegmentOption::BitString { location } => {
            Ok(BitStringSegmentOption::BitString { location })
        }
        BitStringSegmentOption::Utf8 { location } => Ok(BitStringSegmentOption::Utf8 { location }),
        BitStringSegmentOption::Utf16 { location } => {
            Ok(BitStringSegmentOption::Utf16 { location })
        }
        BitStringSegmentOption::Utf32 { location } => {
            Ok(BitStringSegmentOption::Utf32 { location })
        }
        BitStringSegmentOption::Utf8Codepoint { location } => {
            Ok(BitStringSegmentOption::Utf8Codepoint { location })
        }
        BitStringSegmentOption::Utf16Codepoint { location } => {
            Ok(BitStringSegmentOption::Utf16Codepoint { location })
        }
        BitStringSegmentOption::Utf32Codepoint { location } => {
            Ok(BitStringSegmentOption::Utf32Codepoint { location })
        }
        BitStringSegmentOption::Signed { location } => {
            Ok(BitStringSegmentOption::Signed { location })
        }
        BitStringSegmentOption::Unsigned { location } => {
            Ok(BitStringSegmentOption::Unsigned { location })
        }
        BitStringSegmentOption::Big { location } => Ok(BitStringSegmentOption::Big { location }),
        BitStringSegmentOption::Little { location } => {
            Ok(BitStringSegmentOption::Little { location })
        }
        BitStringSegmentOption::Native { location } => {
            Ok(BitStringSegmentOption::Native { location })
        }
    }
}

fn generalise_statement(
    s: TypedDefinition,
    module_name: &SmolStr,
    environment: &mut Environment<'_>,
) -> TypedDefinition {
    match s {
        Definition::Function(function) => generalise_function(function, environment, module_name),

        statement @ (Definition::TypeAlias(TypeAlias { .. })
        | Definition::CustomType(CustomType { .. })
        | Definition::Import(Import { .. })
        | Definition::ModuleConstant(ModuleConstant { .. })) => statement,
    }
}

fn generalise_function(
    function: Function<Arc<Type>, ast::TypedExpr>,
    environment: &mut Environment<'_>,
    module_name: &SmolStr,
) -> TypedDefinition {
    let Function {
        documentation: doc,
        location,
        name,
        public,
        deprecation,
        arguments: args,
        body,
        return_annotation,
        end_position: end_location,
        return_type,
        external_erlang,
        external_javascript,
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
    };
    environment.insert_variable(
        name.clone(),
        variant.clone(),
        type_.clone(),
        public,
        deprecation.clone(),
    );
    environment.insert_module_value(
        name.clone(),
        ValueConstructor {
            public,
            deprecation: deprecation.clone(),
            type_,
            variant,
        },
    );

    Definition::Function(Function {
        documentation: doc,
        location,
        name,
        public,
        deprecation,
        arguments: args,
        end_position: end_location,
        return_annotation,
        return_type,
        body,
        external_erlang,
        external_javascript,
    })
}

fn make_type_vars(
    args: &[SmolStr],
    location: &SrcSpan,
    hydrator: &mut Hydrator,
    environment: &mut Environment<'_>,
) -> Result<Vec<Arc<Type>>, Error> {
    args.iter()
        .map(|arg| TypeAst::Var {
            location: *location,
            name: arg.clone(),
        })
        .map(|ast| hydrator.type_from_ast(&ast, environment))
        .try_collect()
}

fn assert_unique_type_name(
    names: &mut HashMap<SmolStr, SrcSpan>,
    name: &SmolStr,
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
    names: &mut HashMap<SmolStr, SrcSpan>,
    name: &SmolStr,
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
) -> Result<Option<HashMap<SmolStr, RecordAccessor>>, Error> {
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
) -> Vec<(usize, &SmolStr, &TypeAst)> {
    let mut compatible = vec![];

    let first = match constructors.get(0) {
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
fn get_type_dependencies(typ: &TypeAst) -> Vec<SmolStr> {
    let mut deps = Vec::with_capacity(1);

    match typ {
        TypeAst::Var { .. } => (),
        TypeAst::Hole { .. } => (),
        TypeAst::Constructor {
            name,
            arguments,
            module,
            ..
        } => {
            deps.push(match module {
                Some(module) => format!("{}.{}", name, module).into(),
                None => name.clone(),
            });

            for arg in arguments {
                deps.extend(get_type_dependencies(arg))
            }
        }
        TypeAst::Fn {
            arguments, return_, ..
        } => {
            for arg in arguments {
                deps.extend(get_type_dependencies(arg))
            }
            deps.extend(get_type_dependencies(return_))
        }
        TypeAst::Tuple { elems, .. } => {
            for elem in elems {
                deps.extend(get_type_dependencies(elem))
            }
        }
    }

    deps
}

fn sorted_type_aliases(aliases: &Vec<TypeAlias<()>>) -> Result<Vec<&TypeAlias<()>>, Error> {
    let mut deps: Vec<(SmolStr, Vec<SmolStr>)> = Vec::with_capacity(aliases.len());

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
