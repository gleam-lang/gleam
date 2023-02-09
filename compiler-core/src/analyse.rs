#[cfg(test)]
mod tests;

use crate::{
    ast::{
        self, BitStringSegmentOption, ExternalFunction, Function, Layer, RecordConstructor,
        RecordConstructorArg, SrcSpan, Statement, TypeAst, TypedModule, TypedStatement,
        UnqualifiedImport, UntypedModule, UntypedStatement,
    },
    build::{Origin, Target},
    type_::{
        self,
        environment::*,
        error::{convert_unify_error, Error, Warning},
        expression::ExprTyper,
        fields::{FieldMap, FieldMapBuilder},
        hydrator::Hydrator,
        prelude::*,
        AccessorsMap, Module, RecordAccessor, Type, TypeConstructor, ValueConstructor,
        ValueConstructorVariant,
    },
    uid::UniqueIdGenerator,
};
use itertools::Itertools;
use smol_str::SmolStr;
use std::{collections::HashMap, sync::Arc};

// TODO: This takes too many arguments.
/// Crawl the AST, annotating each node with the inferred type or
/// returning an error.
///
pub fn infer_module(
    target: Target,
    ids: &UniqueIdGenerator,
    mut module: UntypedModule,
    origin: Origin,
    package: &SmolStr,
    modules: &im::HashMap<SmolStr, Module>,
    warnings: &mut Vec<Warning>,
) -> Result<TypedModule, Error> {
    let name = module.name.clone();
    let documentation = std::mem::take(&mut module.documentation);
    let mut env = Environment::new(ids.clone(), &name, modules, warnings);
    validate_module_name(&name)?;

    let mut type_names = HashMap::with_capacity(module.statements.len());
    let mut value_names = HashMap::with_capacity(module.statements.len());
    let mut hydrators = HashMap::with_capacity(module.statements.len());

    // Register any modules, types, and values being imported
    // We process imports first so that anything imported can be referenced
    // anywhere in the module.
    for s in module.iter_statements(target) {
        register_import(s, &name, origin, &mut env)?;
    }

    // Register types so they can be used in constructors and functions
    // earlier in the module.
    for s in module.iter_statements(target) {
        register_types(s, name.clone(), &mut hydrators, &mut type_names, &mut env)?;
    }

    // Register values so they can be used in functions earlier in the module.
    for s in module.iter_statements(target) {
        register_values(s, &name, &mut hydrators, &mut value_names, &mut env)?;
    }

    // Infer the types of each statement in the module
    // We first infer all the constants so they can be used in functions defined
    // anywhere in the module.
    let mut statements = Vec::with_capacity(module.statements.len());
    let mut consts = vec![];
    let mut not_consts = vec![];
    for statement in module.into_iter_statements(target) {
        match statement {
            Statement::Function(Function { .. })
            | Statement::TypeAlias { .. }
            | Statement::CustomType { .. }
            | Statement::ExternalFunction(ExternalFunction { .. })
            | Statement::ExternalType { .. }
            | Statement::Import { .. } => not_consts.push(statement),

            Statement::ModuleConstant { .. } => consts.push(statement),
        }
    }

    for statement in consts.into_iter().chain(not_consts) {
        let statement = infer_statement(statement, &name, &mut hydrators, &mut env)?;
        statements.push(statement);
    }

    // Generalise functions now that the entire module has been inferred
    let statements = statements
        .into_iter()
        .map(|s| generalise_statement(s, &name, &mut env))
        .collect();

    // Generate warnings for unused items
    env.convert_unused_to_warnings();

    // Remove private and imported types and values to create the public interface
    env.module_types
        .retain(|_, info| info.public && info.module == name);
    env.module_values.retain(|_, info| info.public);
    env.accessors.retain(|_, accessors| accessors.public);

    // Ensure no exported values have private types in their type signature
    for value in env.module_values.values() {
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
        statements,
        type_info: Module {
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
    s: &UntypedStatement,
    current_module: &str,
    origin: Origin,
    environment: &mut Environment<'_>,
) -> Result<(), Error> {
    match s {
        Statement::Import {
            module,
            as_name,
            unqualified,
            location,
            ..
        } => {
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

            if origin.is_src() && !module_info.origin.is_src() {
                return Err(Error::SrcImportingTest {
                    location: *location,
                    src_module: current_module.into(),
                    test_module: name,
                });
            }

            // Determine local alias of imported module
            let module_name = as_name
                .as_ref()
                .cloned()
                .or_else(|| module.split('/').last().map(|s| s.into()))
                .expect("Typer could not identify module name.");

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
                if let Some(value) = module_info.values.get(name) {
                    environment.insert_variable(
                        imported_name.clone(),
                        value.variant.clone(),
                        value.type_.clone(),
                        true,
                    );
                    variant = Some(&value.variant);
                    value_imported = true;
                }

                // Register the unqualified import if it is a type constructor
                if let Some(typ) = module_info.types.get(name) {
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
                    environment.init_usage(
                        imported_name.clone(),
                        EntityKind::ImportedType,
                        *location,
                    );
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
                        value_constructors: module_info.values.keys().cloned().collect(),
                        type_constructors: module_info.types.keys().cloned().collect(),
                    });
                }
            }

            if unqualified.is_empty() {
                // When the module has no unqualified imports, we track its usage
                // so we can warn if not used by the end of the type checking
                let _ = environment
                    .unused_modules
                    .insert(module_name.clone(), *location);
            }

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
            Ok(())
        }

        Statement::Function(Function { .. })
        | Statement::TypeAlias { .. }
        | Statement::CustomType { .. }
        | Statement::ExternalFunction(ExternalFunction { .. })
        | Statement::ExternalType { .. }
        | Statement::ModuleConstant { .. } => Ok(()),
    }
}

fn validate_module_name(name: &SmolStr) -> Result<(), Error> {
    if name == "gleam" {
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

/// Iterate over a module, registering any new types created by the module into the typer
pub fn register_types<'a>(
    statement: &'a UntypedStatement,
    module: SmolStr,
    hydrators: &mut HashMap<SmolStr, Hydrator>,
    names: &mut HashMap<&'a SmolStr, &'a SrcSpan>,
    environment: &mut Environment<'_>,
) -> Result<(), Error> {
    match statement {
        Statement::ExternalType {
            name,
            public,
            arguments: args,
            location,
            ..
        } => {
            assert_unique_type_name(names, name, location)?;

            // Build a type from the type AST
            let mut hydrator = Hydrator::new();
            let parameters = make_type_vars(args, location, &mut hydrator, environment)?;
            let typ = Arc::new(Type::App {
                public: *public,
                module: module.as_str().into(),
                name: name.clone(),
                args: parameters.clone(),
            });

            // Insert into the module scope
            environment.insert_type_constructor(
                name.clone(),
                TypeConstructor {
                    origin: *location,
                    module,
                    public: *public,
                    parameters,
                    typ,
                },
            )?;

            // Keep track of private types so we can tell if they are later unused
            if !public {
                environment.init_usage(name.clone(), EntityKind::PrivateType, *location);
            }
        }

        Statement::CustomType {
            name,
            public,
            parameters,
            location,
            constructors,
            ..
        } => {
            assert_unique_type_name(names, name, location)?;

            // Build a type from the type AST
            let mut hydrator = Hydrator::new();
            let parameters = make_type_vars(parameters, location, &mut hydrator, environment)?;
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
                    module,
                    public: *public,
                    parameters,
                    typ,
                },
            )?;

            let constructor_names = constructors.iter().map(|c| c.name.clone()).collect();
            environment.insert_type_to_constructors(name.clone(), constructor_names);

            // Keep track of private types so we can tell if they are later unused
            if !public {
                environment.init_usage(name.clone(), EntityKind::PrivateType, *location);
            }
        }

        Statement::TypeAlias {
            location,
            public,
            parameters: args,
            alias: name,
            type_ast: resolved_type,
            ..
        } => {
            assert_unique_type_name(names, name, location)?;

            // Register the paramerterised types
            let mut hydrator = Hydrator::new();
            let parameters = make_type_vars(args, location, &mut hydrator, environment)?;

            // Disallow creation of new types outside the paramerterised types
            hydrator.disallow_new_type_variables();

            // Create the type that the alias resolves to
            let typ = hydrator.type_from_ast(resolved_type, environment)?;
            environment.insert_type_constructor(
                name.clone(),
                TypeConstructor {
                    origin: *location,
                    module,
                    public: *public,
                    parameters,
                    typ,
                },
            )?;

            // Keep track of private types so we can tell if they are later unused
            if !public {
                environment.init_usage(name.clone(), EntityKind::PrivateType, *location);
            }
        }

        Statement::Function(Function { .. })
        | Statement::ExternalFunction(ExternalFunction { .. })
        | Statement::Import { .. }
        | Statement::ModuleConstant { .. } => (),
    }

    Ok(())
}

fn register_values<'a>(
    s: &'a UntypedStatement,
    module_name: &'a SmolStr,
    hydrators: &mut HashMap<SmolStr, Hydrator>,
    names: &mut HashMap<&'a SmolStr, &'a SrcSpan>,
    environment: &mut Environment<'_>,
) -> Result<(), Error> {
    match s {
        Statement::Function(Function {
            name,
            arguments: args,
            location,
            return_annotation,
            public,
            ..
        }) => {
            assert_unique_value_name(names, name, location)?;
            let _ = environment.ungeneralised_functions.insert(name.clone());

            // Create the field map so we can reorder labels for usage of this function
            let mut builder = FieldMapBuilder::new(args.len() as u32);
            for arg in args.iter() {
                builder.add(arg.names.get_label(), arg.location)?;
            }
            let field_map = builder.finish();

            // Construct type from annotations
            let mut hydrator = Hydrator::new();
            hydrator.permit_holes(true);
            let arg_types = args
                .iter()
                .map(|arg| hydrator.type_from_option_ast(&arg.annotation, environment))
                .try_collect()?;
            let return_type = hydrator.type_from_option_ast(return_annotation, environment)?;
            let typ = fn_(arg_types, return_type);

            // Keep track of which types we create from annotations so we can know
            // which generic types not to instantiate later when performing
            // inference of the function body.
            let _ = hydrators.insert(name.clone(), hydrator);

            // Insert the function into the environment
            environment.insert_variable(
                name.clone(),
                ValueConstructorVariant::ModuleFn {
                    name: name.clone(),
                    field_map,
                    module: module_name.clone(),
                    arity: args.len(),
                    location: *location,
                },
                typ,
                *public,
            );
            if !public {
                environment.init_usage(name.clone(), EntityKind::PrivateFunction, *location);
            }
        }

        Statement::ExternalFunction(ExternalFunction {
            location,
            name,
            public,
            arguments: args,
            return_: retrn,
            module,
            fun,
            ..
        }) => {
            assert_unique_value_name(names, name, location)?;

            // Construct type of function from AST
            let mut hydrator = Hydrator::new();
            let (typ, field_map) = environment.in_new_scope(|environment| {
                let return_type = hydrator.type_from_ast(retrn, environment)?;
                let mut args_types = Vec::with_capacity(args.len());
                let mut builder = FieldMapBuilder::new(args.len() as u32);

                for arg in args.iter() {
                    args_types.push(hydrator.type_from_ast(&arg.annotation, environment)?);
                    builder.add(arg.label.as_ref(), arg.location)?;
                }
                let typ = fn_(args_types, return_type);
                Ok((typ, builder.finish()))
            })?;

            // Insert function into module
            environment.insert_module_value(
                name.clone(),
                ValueConstructor {
                    public: *public,
                    type_: typ.clone(),
                    variant: ValueConstructorVariant::ModuleFn {
                        name: fun.clone(),
                        field_map: field_map.clone(),
                        module: module.clone(),
                        arity: args.len(),
                        location: *location,
                    },
                },
            );

            // Insert function into module's internal scope
            environment.insert_variable(
                name.clone(),
                ValueConstructorVariant::ModuleFn {
                    name: fun.clone(),
                    module: module.clone(),
                    arity: args.len(),
                    field_map,
                    location: *location,
                },
                typ,
                *public,
            );
            if !public {
                environment.init_usage(name.clone(), EntityKind::PrivateFunction, *location);
            }
        }

        Statement::CustomType {
            location,
            public,
            opaque,
            name,
            constructors,
            ..
        } => {
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

            // If the custom type only has a single constructor then we can access the
            // fields using the record.field syntax, so store any fields accessors.
            if let Some(accessors) =
                custom_type_accessors(constructors, &mut hydrator, environment)?
            {
                let map = AccessorsMap {
                    public: (*public && !*opaque),
                    accessors,
                    // TODO: improve the ownership here so that we can use the
                    // `return_type_constructor` below rather than looking it up twice.
                    type_: typ.clone(),
                };
                environment.insert_accessors(name.clone(), map)
            }

            // Check and register constructors
            for constructor in constructors {
                assert_unique_value_name(names, &constructor.name, &constructor.location)?;

                let mut field_map = FieldMap::new(constructor.arguments.len() as u32);
                let mut args_types = Vec::with_capacity(constructor.arguments.len());
                for (i, RecordConstructorArg { label, ast, .. }) in
                    constructor.arguments.iter().enumerate()
                {
                    let t = hydrator.type_from_ast(ast, environment)?;
                    args_types.push(t);
                    if let Some(label) = label {
                        field_map.insert(label.clone(), i as u32).map_err(|_| {
                            Error::DuplicateField {
                                label: label.clone(),
                                location: *location,
                            }
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
                    constructors_count: constructors.len() as u16,
                    name: constructor.name.clone(),
                    arity: constructor.arguments.len() as u16,
                    field_map: field_map.clone(),
                    location: constructor.location,
                    module: module_name.clone(),
                };

                if !opaque {
                    environment.insert_module_value(
                        constructor.name.clone(),
                        ValueConstructor {
                            public: *public,
                            type_: typ.clone(),
                            variant: constructor_info.clone(),
                        },
                    );
                }

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
                );
            }
        }

        Statement::ModuleConstant { name, location, .. } => {
            assert_unique_const_name(names, name, location)?;
        }

        Statement::Import { .. } | Statement::TypeAlias { .. } | Statement::ExternalType { .. } => {
        }
    }
    Ok(())
}

fn infer_statement(
    s: UntypedStatement,
    module_name: &SmolStr,
    hydrators: &mut HashMap<SmolStr, Hydrator>,
    environment: &mut Environment<'_>,
) -> Result<TypedStatement, Error> {
    match s {
        Statement::Function(Function {
            doc,
            location,
            name,
            public,
            arguments: args,
            body,
            return_annotation,
            end_position: end_location,
            ..
        }) => {
            let preregistered_fn = environment
                .get_variable(&name)
                .expect("Could not find preregistered type for function");
            let field_map = preregistered_fn.field_map().cloned();
            let preregistered_type = preregistered_fn.type_.clone();
            let (args_types, return_type) = preregistered_type
                .fn_types()
                .expect("Preregistered type for fn was not a fn");

            // Infer the type using the preregistered args + return types as a starting point
            let (typ, args, body, safe_to_generalise) =
                environment.in_new_scope(|environment| {
                    let args = args
                        .into_iter()
                        .zip(&args_types)
                        .map(|(a, t)| a.set_type(t.clone()))
                        .collect();
                    let mut expr_typer = ExprTyper::new(environment);
                    expr_typer.hydrator = hydrators
                        .remove(&name)
                        .expect("Could not find hydrator for fn");
                    let (args, body) =
                        expr_typer.infer_fn_with_known_types(args, body, Some(return_type))?;
                    let args_types = args.iter().map(|a| a.type_.clone()).collect();
                    let typ = fn_(args_types, body.type_());
                    let safe_to_generalise = !expr_typer.ungeneralised_function_used;
                    Ok((typ, args, body, safe_to_generalise))
                })?;

            // Assert that the inferred type matches the type of any recursive call
            unify(preregistered_type, typ.clone()).map_err(|e| convert_unify_error(e, location))?;

            // Generalise the function if safe to do so
            let typ = if safe_to_generalise {
                let _ = environment.ungeneralised_functions.remove(&name);
                let typ = type_::generalise(typ);
                environment.insert_variable(
                    name.clone(),
                    ValueConstructorVariant::ModuleFn {
                        name: name.clone(),
                        field_map,
                        module: module_name.clone(),
                        arity: args.len(),
                        location,
                    },
                    typ.clone(),
                    public,
                );
                typ
            } else {
                typ
            };

            Ok(Statement::Function(Function {
                doc,
                location,
                name,
                public,
                arguments: args,
                end_position: end_location,
                return_annotation,
                return_type: typ
                    .return_type()
                    .expect("Could not find return type for fn"),
                body,
            }))
        }

        Statement::ExternalFunction(ExternalFunction {
            doc,
            location,
            name,
            public,
            arguments: args,
            return_: retrn,
            module,
            fun,
            ..
        }) => {
            let preregistered_fn = environment
                .get_variable(&name)
                .expect("Could not find preregistered type value function");
            let preregistered_type = preregistered_fn.type_.clone();
            let (args_types, return_type) = preregistered_type
                .fn_types()
                .expect("Preregistered type for fn was not a fn");
            let args = args
                .into_iter()
                .zip(&args_types)
                .map(|(a, t)| a.set_type(t.clone()))
                .collect();
            Ok(Statement::ExternalFunction(ExternalFunction {
                return_type,
                doc,
                location,
                name,
                public,
                arguments: args,
                return_: retrn,
                module,
                fun,
            }))
        }

        Statement::TypeAlias {
            doc,
            location,
            public,
            alias,
            parameters: args,
            type_ast: resolved_type,
            ..
        } => {
            let typ = environment
                .get_type_constructor(&None, &alias)
                .expect("Could not find existing type for type alias")
                .typ
                .clone();
            Ok(Statement::TypeAlias {
                doc,
                location,
                public,
                alias,
                parameters: args,
                type_ast: resolved_type,
                type_: typ,
            })
        }

        Statement::CustomType {
            doc,
            location,
            public,
            opaque,
            name,
            parameters,
            constructors,
            ..
        } => {
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

                        let args = if let Some((args_types, _return_type)) =
                            preregistered_type.fn_types()
                        {
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

            Ok(Statement::CustomType {
                doc,
                location,
                public,
                opaque,
                name,
                parameters,
                constructors,
                typed_parameters,
            })
        }

        Statement::ExternalType {
            doc,
            location,
            public,
            name,
            arguments: args,
        } => {
            // Check contained types are valid
            let mut hydrator = Hydrator::new();
            for arg in &args {
                let var = TypeAst::Var {
                    location,
                    name: arg.clone(),
                };
                let _ = hydrator.type_from_ast(&var, environment)?;
            }
            Ok(Statement::ExternalType {
                doc,
                location,
                public,
                name,
                arguments: args,
            })
        }

        Statement::Import {
            location,
            module,
            as_name,
            mut unqualified,
            ..
        } => {
            // Find imported module
            let module_info = environment.importable_modules.get(&module).ok_or_else(|| {
                Error::UnknownModule {
                    location,
                    name: module.clone(),
                    imported_modules: environment.imported_modules.keys().cloned().collect(),
                }
            })?;
            // Record any imports that are types only as this information is
            // needed to prevent types being imported in generated JavaScript
            for import in unqualified.iter_mut() {
                if environment.imported_types.contains(import.variable_name()) {
                    import.layer = Layer::Type;
                }
            }
            Ok(Statement::Import {
                location,
                module,
                as_name,
                unqualified,
                package: module_info.package.clone(),
            })
        }

        Statement::ModuleConstant {
            doc,
            location,
            name,
            annotation,
            public,
            value,
            ..
        } => {
            let typed_expr = ExprTyper::new(environment).infer_const(&annotation, *value)?;
            let type_ = typed_expr.type_();
            let variant = ValueConstructor {
                public,
                variant: ValueConstructorVariant::ModuleConstant {
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
            );
            environment.insert_module_value(name.clone(), variant);

            if !public {
                environment.init_usage(name.clone(), EntityKind::PrivateConstant, location);
            }

            Ok(Statement::ModuleConstant {
                doc,
                location,
                name,
                annotation,
                public,
                value: Box::new(typed_expr),
                type_,
            })
        }
    }
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
    s: TypedStatement,
    module_name: &SmolStr,
    environment: &mut Environment<'_>,
) -> TypedStatement {
    match s {
        Statement::Function(Function {
            doc,
            location,
            name,
            public,
            arguments: args,
            body,
            return_annotation,
            end_position: end_location,
            return_type,
        }) => {
            // Lookup the inferred function information
            let function = environment
                .get_variable(&name)
                .expect("Could not find preregistered type for function");
            let field_map = function.field_map().cloned();
            let typ = function.type_.clone();

            // Generalise the function if not already done so
            let typ = if environment.ungeneralised_functions.remove(&name) {
                type_::generalise(typ)
            } else {
                typ
            };

            // Insert the function into the module's interface
            environment.insert_module_value(
                name.clone(),
                ValueConstructor {
                    public,
                    type_: typ,
                    variant: ValueConstructorVariant::ModuleFn {
                        name: name.clone(),
                        field_map,
                        module: module_name.clone(),
                        arity: args.len(),
                        location,
                    },
                },
            );

            Statement::Function(Function {
                doc,
                location,
                name,
                public,
                arguments: args,
                end_position: end_location,
                return_annotation,
                return_type,
                body,
            })
        }

        statement @ (Statement::TypeAlias { .. }
        | Statement::CustomType { .. }
        | Statement::ExternalFunction(ExternalFunction { .. })
        | Statement::ExternalType { .. }
        | Statement::Import { .. }
        | Statement::ModuleConstant { .. }) => statement,
    }
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

fn assert_unique_type_name<'a>(
    names: &mut HashMap<&'a SmolStr, &'a SrcSpan>,
    name: &'a SmolStr,
    location: &'a SrcSpan,
) -> Result<(), Error> {
    match names.insert(name, location) {
        Some(previous_location) => Err(Error::DuplicateTypeName {
            name: name.clone(),
            previous_location: *previous_location,
            location: *location,
        }),
        None => Ok(()),
    }
}

fn assert_unique_value_name<'a>(
    names: &mut HashMap<&'a SmolStr, &'a SrcSpan>,
    name: &'a SmolStr,
    location: &'a SrcSpan,
) -> Result<(), Error> {
    match names.insert(name, location) {
        Some(previous_location) => Err(Error::DuplicateName {
            name: name.clone(),
            previous_location: *previous_location,
            location: *location,
        }),
        None => Ok(()),
    }
}

fn assert_unique_const_name<'a>(
    names: &mut HashMap<&'a SmolStr, &'a SrcSpan>,
    name: &'a SmolStr,
    location: &'a SrcSpan,
) -> Result<(), Error> {
    match names.insert(name, location) {
        Some(previous_location) => Err(Error::DuplicateConstName {
            name: name.clone(),
            previous_location: *previous_location,
            location: *location,
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
