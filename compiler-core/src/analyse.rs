#[cfg(test)]
mod tests;

use crate::{
    ast::{
        self, BitStringSegmentOption, CustomType, DefinitionLocation, ExternalFunction,
        ExternalType, Function, GroupedStatements, Import, Layer, ModuleConstant, ModuleFunction,
        ModuleStatement, RecordConstructor, RecordConstructorArg, SrcSpan, TypeAlias, TypeAst,
        TypedModule, TypedModuleStatement, UnqualifiedImport, UntypedModule,
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
        AccessorsMap, Module, PatternConstructor, RecordAccessor, Type, TypeConstructor,
        ValueConstructor, ValueConstructorVariant,
    },
    uid::UniqueIdGenerator,
    warning::TypeWarningEmitter,
};
use itertools::Itertools;
use smol_str::SmolStr;
use std::{collections::HashMap, sync::Arc};

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
    warnings: &TypeWarningEmitter,
) -> Result<TypedModule, Error> {
    let name = module.name.clone();
    let documentation = std::mem::take(&mut module.documentation);
    let mut env = Environment::new(ids.clone(), &name, modules, warnings);
    validate_module_name(&name)?;

    let mut type_names = HashMap::with_capacity(module.statements.len());
    let mut value_names = HashMap::with_capacity(module.statements.len());
    let mut hydrators = HashMap::with_capacity(module.statements.len());

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
    // TODO: Extract a TypeRegistrar class to perform all this.
    for t in &statements.external_types {
        register_types_from_external_type(t, &mut type_names, &mut env, &name)?;
    }
    for t in &statements.custom_types {
        register_types_from_custom_type(t, &mut type_names, &mut env, &name, &mut hydrators)?;
    }
    // TODO: Extract a Type alias class to perform this.
    // TODO: Sort type aliases by dependency order so they don't have to be
    // ordered in the file
    for t in &statements.type_aliases {
        register_type_alias(t, &mut type_names, &mut env, &name)?;
    }

    // Register values so they can be used in functions earlier in the module.
    for c in &statements.constants {
        assert_unique_name(&mut value_names, &c.name, c.location)?;
    }
    for f in &statements.functions {
        register_value_from_function(f, &mut value_names, &mut env, &mut hydrators, &name)?;
    }
    for ef in &statements.external_functions {
        register_external_function(ef, &mut value_names, &mut env)?;
    }
    for t in &statements.custom_types {
        register_values_from_custom_type(t, &mut hydrators, &mut env, &mut value_names, &name)?;
    }

    // Infer the types of each statement in the module
    let mut typed_statements = Vec::with_capacity(statements_count);
    for i in statements.imports {
        let statement = record_imported_items_for_use_detection(i, &mut env)?;
        typed_statements.push(statement);
    }
    for t in statements.custom_types {
        let statement = infer_custom_type(t, &mut env)?;
        typed_statements.push(statement);
    }
    for t in statements.external_types {
        let statement = hydrate_external_type(t, &mut env)?;
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
    let functions = statements
        .functions
        .into_iter()
        .map(ModuleFunction::Internal);
    let external_functions = statements
        .external_functions
        .into_iter()
        .map(ModuleFunction::External);
    let functions = functions.chain(external_functions).collect_vec();
    let function_groups = into_dependency_order(functions)?;
    let mut working_group = vec![];

    for group in function_groups {
        // A group may have multiple functions that depend on each other through
        // mutual recursion.
        for function in group {
            let inferred = match function {
                ModuleFunction::Internal(f) => infer_function(f, &mut env, &mut hydrators, &name)?,
                ModuleFunction::External(f) => infer_external_function(f, &mut env)?,
            };
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

    // Remove private and imported types and values to create the public interface
    env.module_types
        .retain(|_, info| info.public && info.module == name);
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
        statements: typed_statements,
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

fn register_types_from_external_type(
    t: &ExternalType,
    names: &mut HashMap<SmolStr, SrcSpan>,
    environment: &mut Environment<'_>,
    module: &SmolStr,
) -> Result<(), Error> {
    let ExternalType {
        name,
        public,
        arguments: args,
        location,
        ..
    } = t;
    assert_unique_type_name(names, name, *location)?;
    let mut hydrator = Hydrator::new();
    let parameters = make_type_vars(args, location, &mut hydrator, environment)?;
    let typ = Arc::new(Type::App {
        public: *public,
        module: module.as_str().into(),
        name: name.clone(),
        args: parameters.clone(),
    });
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

        environment.insert_variable(constructor.name.clone(), constructor_info, typ, *public);
    }
    Ok(())
}

fn register_external_function(
    f: &ExternalFunction<()>,
    names: &mut HashMap<SmolStr, SrcSpan>,
    environment: &mut Environment<'_>,
) -> Result<(), Error> {
    let ExternalFunction {
        location,
        name,
        public,
        arguments: args,
        return_: retrn,
        module,
        fun,
        documentation,
        ..
    } = f;
    assert_unique_name(names, name, *location)?;
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
    environment.insert_module_value(
        name.clone(),
        ValueConstructor {
            public: *public,
            type_: typ.clone(),
            variant: ValueConstructorVariant::ModuleFn {
                documentation: documentation.clone(),
                name: fun.clone(),
                field_map: field_map.clone(),
                module: module.clone(),
                arity: args.len(),
                location: *location,
            },
        },
    );
    environment.insert_variable(
        name.clone(),
        ValueConstructorVariant::ModuleFn {
            documentation: documentation.clone(),
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
    };
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
        ..
    } = f;
    assert_unique_name(names, name, *location)?;
    let _ = environment.ungeneralised_functions.insert(name.clone());
    let mut builder = FieldMapBuilder::new(args.len() as u32);
    for arg in args.iter() {
        builder.add(arg.names.get_label(), arg.location)?;
    }
    let field_map = builder.finish();
    let mut hydrator = Hydrator::new();
    hydrator.permit_holes(true);
    let arg_types = args
        .iter()
        .map(|arg| hydrator.type_from_option_ast(&arg.annotation, environment))
        .try_collect()?;
    let return_type = hydrator.type_from_option_ast(return_annotation, environment)?;
    let typ = fn_(arg_types, return_type);
    let _ = hydrators.insert(name.clone(), hydrator);
    environment.insert_variable(
        name.clone(),
        ValueConstructorVariant::ModuleFn {
            documentation: documentation.clone(),
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
    };
    Ok(())
}

fn infer_function(
    f: Function<(), ast::UntypedExpr>,
    environment: &mut Environment<'_>,
    hydrators: &mut HashMap<SmolStr, Hydrator>,
    module_name: &SmolStr,
) -> Result<TypedModuleStatement, Error> {
    let Function {
        documentation: doc,
        location,
        name,
        public,
        arguments: args,
        body,
        return_annotation,
        end_position: end_location,
        ..
    } = f;
    let preregistered_fn = environment
        .get_variable(&name)
        .expect("Could not find preregistered type for function");
    let field_map = preregistered_fn.field_map().cloned();
    let preregistered_type = preregistered_fn.type_.clone();
    let (args_types, return_type) = preregistered_type
        .fn_types()
        .expect("Preregistered type for fn was not a fn");

    // Infer the type using the preregistered args + return types as a starting point
    let (type_, args, body, safe_to_generalise) = environment.in_new_scope(|environment| {
        let args = args
            .into_iter()
            .zip(&args_types)
            .map(|(a, t)| a.set_type(t.clone()))
            .collect();
        let mut expr_typer = ExprTyper::new(environment);
        expr_typer.hydrator = hydrators
            .remove(&name)
            .expect("Could not find hydrator for fn");
        let (args, body) = expr_typer.infer_fn_with_known_types(args, body, Some(return_type))?;
        let args_types = args.iter().map(|a| a.type_.clone()).collect();
        let typ = fn_(args_types, body.last().type_());
        let safe_to_generalise = !expr_typer.ungeneralised_function_used;
        Ok((typ, args, body, safe_to_generalise))
    })?;

    // Assert that the inferred type matches the type of any recursive call
    unify(preregistered_type, type_.clone()).map_err(|e| convert_unify_error(e, location))?;

    // Generalise the function if safe to do so
    let type_ = if safe_to_generalise {
        let _ = environment.ungeneralised_functions.remove(&name);
        let type_ = type_::generalise(type_);
        environment.insert_variable(
            name.clone(),
            ValueConstructorVariant::ModuleFn {
                documentation: doc.clone(),
                name: name.clone(),
                field_map,
                module: module_name.clone(),
                arity: args.len(),
                location,
            },
            type_.clone(),
            public,
        );
        type_
    } else {
        type_
    };

    Ok(ModuleStatement::Function(Function {
        documentation: doc,
        location,
        name,
        public,
        arguments: args,
        end_position: end_location,
        return_annotation,
        return_type: type_
            .return_type()
            .expect("Could not find return type for fn"),
        body,
    }))
}

fn infer_external_function(
    f: ExternalFunction<()>,
    environment: &mut Environment<'_>,
) -> Result<TypedModuleStatement, Error> {
    let ExternalFunction {
        documentation: doc,
        location,
        name,
        public,
        arguments: args,
        return_: retrn,
        module,
        fun,
        ..
    } = f;
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
    Ok(ModuleStatement::ExternalFunction(ExternalFunction {
        return_type,
        documentation: doc,
        location,
        name,
        public,
        arguments: args,
        return_: retrn,
        module,
        fun,
    }))
}

fn insert_type_alias(
    t: TypeAlias<()>,
    environment: &mut Environment<'_>,
) -> Result<TypedModuleStatement, Error> {
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
    Ok(ModuleStatement::TypeAlias(TypeAlias {
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
) -> Result<TypedModuleStatement, Error> {
    let CustomType {
        documentation: doc,
        location,
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

    Ok(ModuleStatement::CustomType(CustomType {
        documentation: doc,
        location,
        public,
        opaque,
        name,
        parameters,
        constructors,
        typed_parameters,
    }))
}

fn hydrate_external_type(
    t: ExternalType,
    environment: &mut Environment<'_>,
) -> Result<TypedModuleStatement, Error> {
    let ExternalType {
        documentation: doc,
        location,
        public,
        name,
        arguments: args,
    } = t;
    // Check contained types are valid
    let mut hydrator = Hydrator::new();
    for arg in &args {
        let var = TypeAst::Var {
            location,
            name: arg.clone(),
        };
        let _ = hydrator.type_from_ast(&var, environment)?;
    }
    Ok(ModuleStatement::ExternalType(ExternalType {
        documentation: doc,
        location,
        public,
        name,
        arguments: args,
    }))
}

fn record_imported_items_for_use_detection(
    i: Import<()>,
    environment: &mut Environment<'_>,
) -> Result<TypedModuleStatement, Error> {
    let Import {
        documentation,
        location,
        module,
        as_name,
        mut unqualified,
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
    Ok(ModuleStatement::Import(Import {
        documentation,
        location,
        module,
        as_name,
        unqualified,
        package: module_info.package.clone(),
    }))
}

fn infer_module_constant(
    c: ModuleConstant<(), ()>,
    environment: &mut Environment<'_>,
    module_name: &SmolStr,
) -> Result<TypedModuleStatement, Error> {
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
        variant: ValueConstructorVariant::ModuleConstant {
            documentation: doc.clone(),
            location,
            literal: typed_expr.clone(),
            module: module_name.clone(),
        },
        type_: type_.clone(),
    };

    environment.insert_variable(name.clone(), variant.variant.clone(), type_.clone(), public);
    environment.insert_module_value(name.clone(), variant);

    if !public {
        environment.init_usage(name.clone(), EntityKind::PrivateConstant, location);
    }

    Ok(ModuleStatement::ModuleConstant(ModuleConstant {
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
    s: TypedModuleStatement,
    module_name: &SmolStr,
    environment: &mut Environment<'_>,
) -> TypedModuleStatement {
    match s {
        ModuleStatement::Function(function) => {
            generalise_function(function, environment, module_name)
        }

        statement @ (ModuleStatement::TypeAlias(TypeAlias { .. })
        | ModuleStatement::CustomType(CustomType { .. })
        | ModuleStatement::ExternalFunction(ExternalFunction { .. })
        | ModuleStatement::ExternalType(ExternalType { .. })
        | ModuleStatement::Import(Import { .. })
        | ModuleStatement::ModuleConstant(ModuleConstant { .. })) => statement,
    }
}

fn generalise_function(
    function: Function<Arc<Type>, ast::TypedExpr>,
    environment: &mut Environment<'_>,
    module_name: &SmolStr,
) -> TypedModuleStatement {
    let Function {
        documentation: doc,
        location,
        name,
        public,
        arguments: args,
        body,
        return_annotation,
        end_position: end_location,
        return_type,
    } = function;

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
                documentation: doc.clone(),
                name: name.clone(),
                field_map,
                module: module_name.clone(),
                arity: args.len(),
                location,
            },
        },
    );

    ModuleStatement::Function(Function {
        documentation: doc,
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
