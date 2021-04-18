mod environment;
mod error;
mod expr;
mod fields;
mod hydrator;
mod pattern;
mod prelude;
pub mod pretty;
#[cfg(test)]
mod test_helpers;
#[cfg(test)]
mod tests;

pub use environment::*;
pub use error::{Error, UnifyErrorSituation, Warning};
pub use expr::*;
pub use fields::FieldMap;
pub use prelude::*;

use crate::{
    ast::{
        self, ArgNames, BitStringSegment, BitStringSegmentOption, CallArg, Constant, Pattern,
        RecordConstructor, RecordConstructorArg, SrcSpan, Statement, TypeAst, TypedConstant,
        TypedExpr, TypedModule, TypedPattern, TypedPatternBitStringSegment, TypedRecordUpdateArg,
        TypedStatement, UnqualifiedImport, UntypedModule, UntypedMultiPattern, UntypedPattern,
        UntypedRecordUpdateArg, UntypedStatement,
    },
    bit_string,
    build::Origin,
    error::GleamExpect,
};

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::ops::Deref;
use std::sync::Arc;

use error::*;
use hydrator::Hydrator;
use itertools::Itertools;

pub trait HasType {
    fn type_(&self) -> Arc<Type>;
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    App {
        public: bool,
        module: Vec<String>,
        name: String,
        args: Vec<Arc<Type>>,
    },

    Fn {
        args: Vec<Arc<Type>>,
        retrn: Arc<Type>,
    },

    Var {
        type_: Arc<RefCell<TypeVar>>,
    },

    Tuple {
        elems: Vec<Arc<Type>>,
    },
}

impl Type {
    pub fn is_result(&self) -> bool {
        matches!(self, Self::App { name, module, .. } if "Result" == name && module.is_empty())
    }

    pub fn is_unbound(&self) -> bool {
        matches!(self, Self::Var { type_: typ } if typ.borrow().is_unbound())
    }

    pub fn return_type(&self) -> Option<Arc<Self>> {
        match self {
            Self::Fn { retrn, .. } => Some(retrn.clone()),
            _ => None,
        }
    }

    pub fn fn_types(&self) -> Option<(Vec<Arc<Self>>, Arc<Self>)> {
        match self {
            Self::Fn { args, retrn, .. } => Some((args.clone(), retrn.clone())),
            _ => None,
        }
    }

    pub fn is_int(&self) -> bool {
        matches!(self, Self::App { module, name, .. } if "Int" == name && module.is_empty())
    }

    pub fn is_float(&self) -> bool {
        matches!(self, Self::App { module, name, .. } if "Float" == name && module.is_empty())
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Self::App { module, name, .. } if "String" == name && module.is_empty())
    }

    /// Get the args for the type if the type is a specific `Type::App`.
    /// Returns None if the type is not a `Type::App` or is an incorrect `Type:App`
    ///
    pub fn get_app_args(
        &self,
        public: bool,
        module: &[String],
        name: &str,
        arity: usize,
        environment: &mut Environment<'_, '_>,
    ) -> Option<Vec<Arc<Self>>> {
        match self {
            Self::App {
                module: m,
                name: n,
                args,
                ..
            } => {
                if module == m && name == n && args.len() == arity {
                    Some(args.clone())
                } else {
                    None
                }
            }

            Self::Var { type_: typ } => {
                let args: Vec<_> = match typ.borrow().deref() {
                    TypeVar::Link { type_: typ } => {
                        return typ.get_app_args(public, module, name, arity, environment);
                    }

                    TypeVar::Unbound { level, .. } => (0..arity)
                        .map(|_| environment.new_unbound_var(*level))
                        .collect(),

                    TypeVar::Generic { .. } => return None,
                };

                // TODO: use the real type here rather than making a copy
                *typ.borrow_mut() = TypeVar::Link {
                    type_: Arc::new(Self::App {
                        name: name.to_string(),
                        module: module.to_owned(),
                        args: args.clone(),
                        public,
                    }),
                };
                Some(args)
            }

            _ => None,
        }
    }

    pub fn find_private_type(&self) -> Option<Self> {
        match self {
            Self::App { public: false, .. } => Some(self.clone()),

            Self::App { args, .. } => args.iter().find_map(|t| t.find_private_type()),

            Self::Tuple { elems, .. } => elems.iter().find_map(|t| t.find_private_type()),

            Self::Fn { retrn, args, .. } => retrn
                .find_private_type()
                .or_else(|| args.iter().find_map(|t| t.find_private_type())),

            Self::Var { type_: typ, .. } => match typ.borrow().deref() {
                TypeVar::Unbound { .. } => None,

                TypeVar::Generic { .. } => None,

                TypeVar::Link { type_: typ, .. } => typ.find_private_type(),
            },
        }
    }

    pub fn fn_arity(&self) -> Option<usize> {
        match self {
            Self::Fn { args, .. } => Some(args.len()),
            _ => None,
        }
    }
}

pub fn collapse_links(t: Arc<Type>) -> Arc<Type> {
    if let Type::Var { type_: typ } = t.deref() {
        if let TypeVar::Link { type_: typ } = typ.borrow().deref() {
            return typ.clone();
        }
    }
    t
}

#[derive(Debug, PartialEq, Clone)]
pub struct AccessorsMap {
    pub public: bool,
    pub type_: Arc<Type>,
    pub accessors: HashMap<String, RecordAccessor>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RecordAccessor {
    // TODO: smaller int. Doesn't need to be this big
    pub index: u64,
    pub label: String,
    pub type_: Arc<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueConstructorVariant {
    /// A locally defined variable or function parameter
    LocalVariable,

    /// A module constant
    ModuleConstant {
        literal: Constant<Arc<Type>, String>,
    },

    /// A function belonging to the module
    ModuleFn {
        name: String,
        field_map: Option<FieldMap>,
        module: Vec<String>,
        arity: usize,
    },

    /// A constructor for a custom type
    Record {
        name: String,
        arity: usize,
        field_map: Option<FieldMap>,
    },
}

impl ValueConstructorVariant {
    fn to_module_value_constructor(&self) -> ModuleValueConstructor {
        match self {
            Self::Record { name, arity, .. } => ModuleValueConstructor::Record {
                name: name.clone(),
                arity: *arity,
            },

            Self::ModuleConstant { literal } => ModuleValueConstructor::Constant {
                literal: literal.clone(),
            },

            Self::LocalVariable { .. } | Self::ModuleFn { .. } => ModuleValueConstructor::Fn,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleValueConstructor {
    Record { name: String, arity: usize },
    Fn,
    Constant { literal: TypedConstant },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: Vec<String>,
    pub types: HashMap<String, TypeConstructor>,
    pub values: HashMap<String, ValueConstructor>,
    pub accessors: HashMap<String, AccessorsMap>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternConstructor {
    Record { name: String },
}

pub trait Typer {
    fn with_environment<T>(&mut self, f: impl FnOnce(&mut Environment<'_, '_>) -> T) -> T;

    fn new_unbound_var(&mut self, level: usize) -> Arc<Type> {
        self.with_environment(|e| e.new_unbound_var(level))
    }

    fn unify(&mut self, t1: Arc<Type>, t2: Arc<Type>) -> Result<(), UnifyError> {
        self.with_environment(|e| e.unify(t1, t2))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeVar {
    Unbound { id: usize, level: usize },
    Link { type_: Arc<Type> },
    Generic { id: usize },
}

impl TypeVar {
    pub fn is_unbound(&self) -> bool {
        matches!(self, Self::Unbound { .. })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeConstructor {
    pub public: bool,
    pub origin: SrcSpan,
    pub module: Vec<String>,
    pub parameters: Vec<Arc<Type>>,
    pub typ: Arc<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueConstructor {
    pub public: bool,
    pub origin: SrcSpan,
    pub variant: ValueConstructorVariant,
    pub type_: Arc<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAliasConstructor {
    pub public: bool,
    pub module: Vec<String>,
    pub type_: Type,
    pub arity: usize,
}

impl ValueConstructor {
    fn field_map(&self) -> Option<&FieldMap> {
        match &self.variant {
            ValueConstructorVariant::ModuleFn { field_map, .. }
            | ValueConstructorVariant::Record { field_map, .. } => field_map.as_ref(),
            _ => None,
        }
    }
}

/// Crawl the AST, annotating each node with the inferred type or
/// returning an error.
///
pub fn infer_module(
    uid: &mut usize,
    module: UntypedModule,
    modules: &HashMap<String, (Origin, Module)>,
    warnings: &mut Vec<Warning>,
) -> Result<TypedModule, Error> {
    let mut environment = Environment::new(uid, &module.name, modules, warnings);
    let module_name = &module.name;
    let mut type_names = HashMap::with_capacity(module.statements.len());
    let mut value_names = HashMap::with_capacity(module.statements.len());
    let mut hydrators = HashMap::with_capacity(module.statements.len());

    // Register any modules, types, and values being imported
    // We process imports first so that anything imported can be referenced
    // anywhere in the module.
    for s in &module.statements {
        register_import(s, &mut environment)?;
    }

    // Register types so they can be used in constructors and functions
    // earlier in the module.
    for s in &module.statements {
        register_types(
            s,
            module_name,
            &mut hydrators,
            &mut type_names,
            &mut environment,
        )?;
    }

    // Register values so they can be used in functions earlier in the module.
    for s in &module.statements {
        register_values(
            s,
            module_name,
            &mut hydrators,
            &mut value_names,
            &mut environment,
        )?;
    }

    // Infer the types of each statement in the module
    // We first infer all the constants so they can be used in functions defined
    // anywhere in the module.
    let mut statements = Vec::with_capacity(module.statements.len());
    let mut not_consts = vec![];
    for statement in module.statements {
        if matches!(statement, Statement::ModuleConstant { .. }) {
            let statement =
                infer_statement(statement, module_name, &mut hydrators, &mut environment)?;
            statements.push(statement);
        } else {
            not_consts.push(statement)
        }
    }

    for statement in not_consts {
        let statement = infer_statement(statement, module_name, &mut hydrators, &mut environment)?;
        statements.push(statement);
    }

    // Generalise functions now that the entire module has been inferred
    let statements = statements
        .into_iter()
        .map(|s| generalise_statement(s, module_name, &mut environment))
        .try_collect()?;

    // Generate warnings for unused items
    environment.convert_unused_to_warnings();

    // Remove private and imported types and values to create the public interface
    environment
        .module_types
        .retain(|_, info| info.public && &info.module == module_name);
    environment.module_values.retain(|_, info| info.public);
    environment
        .accessors
        .retain(|_, accessors| accessors.public);

    // Ensure no exported values have private types in their type signature
    for value in environment.module_values.values() {
        if let Some(leaked) = value.type_.find_private_type() {
            return Err(Error::PrivateTypeLeak {
                location: value.origin,
                leaked,
            });
        }
    }

    let Environment {
        module_types: types,
        module_values: values,
        accessors,
        ..
    } = environment;

    Ok(ast::Module {
        documentation: module.documentation,
        name: module.name.clone(),
        statements,
        type_info: Module {
            name: module.name,
            types,
            values,
            accessors,
        },
    })
}

fn assert_unique_value_name<'a>(
    names: &mut HashMap<&'a str, &'a SrcSpan>,
    name: &'a str,
    location: &'a SrcSpan,
) -> Result<(), Error> {
    match names.insert(name, location) {
        Some(previous_location) => Err(Error::DuplicateName {
            name: name.to_string(),
            previous_location: *previous_location,
            location: *location,
        }),
        None => Ok(()),
    }
}

fn assert_unique_type_name<'a>(
    names: &mut HashMap<&'a str, &'a SrcSpan>,
    name: &'a str,
    location: &'a SrcSpan,
) -> Result<(), Error> {
    match names.insert(name, location) {
        Some(previous_location) => Err(Error::DuplicateTypeName {
            name: name.to_string(),
            previous_location: *previous_location,
            location: *location,
        }),
        None => Ok(()),
    }
}

fn register_values<'a>(
    s: &'a UntypedStatement,
    module_name: &[String],
    hydrators: &mut HashMap<String, Hydrator>,
    names: &mut HashMap<&'a str, &'a SrcSpan>,
    environment: &mut Environment<'_, '_>,
) -> Result<(), Error> {
    match s {
        Statement::Fn {
            name,
            arguments: args,
            location,
            return_annotation,
            public,
            ..
        } => {
            assert_unique_value_name(names, name, location)?;
            let _ = environment.ungeneralised_functions.insert(name.to_string());

            // Create the field map so we can reorder labels for usage of this function
            let mut field_map = FieldMap::new(args.len());
            for (i, arg) in args.iter().enumerate() {
                if let ArgNames::NamedLabelled { label, .. } = &arg.names {
                    field_map
                        .insert(label.clone(), i)
                        .map_err(|_| Error::DuplicateField {
                            label: label.to_string(),
                            location: *location,
                        })?;
                }
            }
            let field_map = field_map.into_option();

            // Construct type from annotations
            let mut hydrator = Hydrator::new();
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
                    module: module_name.to_vec(),
                    arity: args.len(),
                },
                typ,
                *location,
            );
            if !public {
                environment.init_usage(name.clone(), EntityKind::PrivateFunction, *location);
            }
        }

        Statement::ExternalFn {
            location,
            name,
            public,
            arguments: args,
            return_: retrn,
            module,
            fun,
            ..
        } => {
            assert_unique_value_name(names, name, location)?;

            // Construct type of function from AST
            let mut hydrator = Hydrator::new();
            let (typ, field_map) = environment.in_new_scope(|environment| {
                let return_type = hydrator.type_from_ast(retrn, environment)?;
                let mut args_types = Vec::with_capacity(args.len());
                let mut field_map = FieldMap::new(args.len());
                for (i, arg) in args.iter().enumerate() {
                    let t = hydrator.type_from_ast(&arg.annotation, environment)?;
                    args_types.push(t);
                    if let Some(label) = &arg.label {
                        field_map
                            .insert(label.clone(), i)
                            .map_err(|_| Error::DuplicateField {
                                label: label.to_string(),
                                location: *location,
                            })?;
                    }
                }
                let field_map = field_map.into_option();
                let typ = fn_(args_types, return_type);

                Ok((typ, field_map))
            })?;

            // Insert function into module
            environment.insert_module_value(
                name,
                ValueConstructor {
                    public: *public,
                    type_: typ.clone(),
                    origin: *location,
                    variant: ValueConstructorVariant::ModuleFn {
                        name: fun.clone(),
                        field_map: field_map.clone(),
                        module: vec![module.clone()],
                        arity: args.len(),
                    },
                },
            );

            // Insert function into module's internal scope
            environment.insert_variable(
                name.clone(),
                ValueConstructorVariant::ModuleFn {
                    name: fun.clone(),
                    module: vec![module.clone()],
                    arity: args.len(),
                    field_map,
                },
                typ,
                *location,
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
                .gleam_expect("Could not find hydrator for register_values custom type");
            hydrator.disallow_new_type_variables();

            let typ = environment
                .module_types
                .get(name)
                .gleam_expect("Type for custom type not found in register_values")
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
                environment.insert_accessors(name, map)
            }

            // Check and register constructors
            for constructor in constructors {
                assert_unique_value_name(names, &constructor.name, &constructor.location)?;

                let mut field_map = FieldMap::new(constructor.arguments.len());
                let mut args_types = Vec::with_capacity(constructor.arguments.len());
                for (i, RecordConstructorArg { label, ast, .. }) in
                    constructor.arguments.iter().enumerate()
                {
                    let t = hydrator.type_from_ast(ast, environment)?;
                    args_types.push(t);
                    if let Some(label) = label {
                        field_map
                            .insert(label.clone(), i)
                            .map_err(|_| Error::DuplicateField {
                                label: label.to_string(),
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

                if !opaque {
                    environment.insert_module_value(
                        &constructor.name,
                        ValueConstructor {
                            public: *public,
                            type_: typ.clone(),
                            origin: constructor.location,
                            variant: ValueConstructorVariant::Record {
                                name: constructor.name.clone(),
                                arity: constructor.arguments.len(),
                                field_map: field_map.clone(),
                            },
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
                    ValueConstructorVariant::Record {
                        name: constructor.name.clone(),
                        arity: constructor.arguments.len(),
                        field_map,
                    },
                    typ,
                    constructor.location,
                );
            }
        }

        _ => (),
    }
    Ok(())
}

fn generalise_statement(
    s: TypedStatement,
    module_name: &[String],
    environment: &mut Environment<'_, '_>,
) -> Result<TypedStatement, Error> {
    match s {
        Statement::Fn {
            doc,
            location,
            name,
            public,
            arguments: args,
            body,
            return_annotation,
            end_location,
            return_type,
        } => {
            // Lookup the inferred function information
            let function = environment
                .get_variable(&name)
                .gleam_expect("Could not find preregistered type for function");
            let field_map = function.field_map().cloned();
            let typ = function.type_.clone();

            // Generalise the function if not already done so
            let typ = if environment.ungeneralised_functions.remove(&name) {
                let level = 1;
                generalise(typ, level)
            } else {
                typ
            };

            // Insert the function into the module's interface
            environment.insert_module_value(
                &name,
                ValueConstructor {
                    public,
                    origin: location,
                    type_: typ,
                    variant: ValueConstructorVariant::ModuleFn {
                        name: name.clone(),
                        field_map,
                        module: module_name.to_vec(),
                        arity: args.len(),
                    },
                },
            );

            Ok(Statement::Fn {
                doc,
                location,
                name,
                public,
                arguments: args,
                end_location,
                return_annotation,
                return_type,
                body,
            })
        }

        statement => Ok(statement),
    }
}

fn infer_statement(
    s: UntypedStatement,
    module_name: &[String],
    hydrators: &mut HashMap<String, Hydrator>,
    environment: &mut Environment<'_, '_>,
) -> Result<TypedStatement, Error> {
    match s {
        Statement::Fn {
            doc,
            location,
            name,
            public,
            arguments: args,
            body,
            return_annotation,
            end_location,
            ..
        } => {
            let preregistered_fn = environment
                .get_variable(&name)
                .gleam_expect("Could not find preregistered type for function");
            let field_map = preregistered_fn.field_map().cloned();
            let preregistered_type = preregistered_fn.type_.clone();
            let (args_types, return_type) = preregistered_type
                .fn_types()
                .gleam_expect("Preregistered type for fn was not a fn");

            // Infer the type using the preregistered args + return types as a starting point
            let (typ, args, body, safe_to_generalise) =
                environment.in_new_scope(|environment| {
                    let args = args
                        .into_iter()
                        .zip(args_types.iter())
                        .map(|(a, t)| a.set_type(t.clone()))
                        .collect();
                    let mut expr_typer = ExprTyper::new(environment);
                    expr_typer.hydrator = hydrators
                        .remove(&name)
                        .gleam_expect("Could not find hydrator for fn");
                    let (args, body) =
                        expr_typer.infer_fn_with_known_types(args, body, Some(return_type))?;
                    let args_types = args.iter().map(|a| a.type_.clone()).collect();
                    let typ = fn_(args_types, body.type_());
                    let safe_to_generalise = !expr_typer.ungeneralised_function_used;
                    Ok((typ, args, body, safe_to_generalise))
                })?;

            // Assert that the inferred type matches the type of any recursive call
            environment
                .unify(preregistered_type, typ.clone())
                .map_err(|e| convert_unify_error(e, location))?;

            // Generalise the function if safe to do so
            let typ = if safe_to_generalise {
                let _ = environment.ungeneralised_functions.remove(&name);
                let typ = generalise(typ, 0);
                environment.insert_variable(
                    name.clone(),
                    ValueConstructorVariant::ModuleFn {
                        name: name.clone(),
                        field_map,
                        module: module_name.to_vec(),
                        arity: args.len(),
                    },
                    typ.clone(),
                    location,
                );
                typ
            } else {
                typ
            };

            Ok(Statement::Fn {
                doc,
                location,
                name,
                public,
                arguments: args,
                end_location,
                return_annotation,
                return_type: typ
                    .return_type()
                    .gleam_expect("Could not find return type for fn"),
                body,
            })
        }

        Statement::ExternalFn {
            doc,
            location,
            name,
            public,
            arguments: args,
            return_: retrn,
            module,
            fun,
            ..
        } => {
            let preregistered_fn = environment
                .get_variable(&name)
                .gleam_expect("Could not find preregistered type for function");
            let preregistered_type = preregistered_fn.type_.clone();
            let (args_types, return_type) = preregistered_type
                .fn_types()
                .gleam_expect("Preregistered type for fn was not a fn");
            let args = args
                .into_iter()
                .zip(args_types.iter())
                .map(|(a, t)| a.set_type(t.clone()))
                .collect();
            Ok(Statement::ExternalFn {
                return_type,
                doc,
                location,
                name,
                public,
                arguments: args,
                return_: retrn,
                module,
                fun,
            })
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
                .gleam_expect("Could not find existing type for type alias")
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
                            .gleam_expect("Could not find preregistered type for function");
                        let preregistered_type = preregistered_fn.type_.clone();

                        let args = if let Some((args_types, _return_type)) =
                            preregistered_type.fn_types()
                        {
                            args.into_iter()
                                .zip(args_types.iter())
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
                .gleam_expect("Could not find preregistered type constructor ")
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
                    name: arg.to_string(),
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
            unqualified,
        } => Ok(Statement::Import {
            location,
            module,
            as_name,
            unqualified,
        }),

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

            environment.insert_module_value(
                &name,
                ValueConstructor {
                    public,
                    origin: location,
                    variant: ValueConstructorVariant::ModuleConstant {
                        literal: typed_expr.clone(),
                    },
                    type_: type_.clone(),
                },
            );

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

fn infer_bit_string_segment_option<UntypedValue, TypedValue, Typer>(
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

pub type TypedCallArg = CallArg<TypedExpr>;

fn assert_no_labelled_arguments<A>(args: &[CallArg<A>]) -> Result<(), Error> {
    for arg in args {
        if let Some(label) = &arg.label {
            return Err(Error::UnexpectedLabelledArg {
                location: arg.location,
                label: label.to_string(),
            });
        }
    }
    Ok(())
}

/// This function makes sure that the type variable being unified
/// doesn't occur within the type it is being unified with. This
/// prevents the algorithm from inferring recursive types, which
/// could cause naively-implemented type checking to diverge.
/// While traversing the type tree, this function also takes care
/// of updating the levels of the type variables appearing within
/// the type, thus ensuring the type will be correctly generalized.
///
fn update_levels(typ: Arc<Type>, own_level: usize, own_id: usize) -> Result<(), UnifyError> {
    if let Type::Var { type_: typ } = typ.deref() {
        let new_value = match typ.borrow().deref() {
            TypeVar::Link { type_: typ, .. } => {
                return update_levels(typ.clone(), own_level, own_id)
            }

            TypeVar::Unbound { id, level } => {
                if id == &own_id {
                    return Err(UnifyError::RecursiveType);
                } else if *level > own_level {
                    Some(TypeVar::Unbound {
                        id: *id,
                        level: own_level,
                    })
                } else {
                    return Ok(());
                }
            }

            TypeVar::Generic { .. } => return Ok(()),
        };

        if let Some(t) = new_value {
            *typ.borrow_mut() = t;
        }
        return Ok(());
    }

    match typ.deref() {
        Type::App { args, .. } => {
            for arg in args {
                update_levels(arg.clone(), own_level, own_id)?
            }
            Ok(())
        }

        Type::Fn { args, retrn } => {
            for arg in args {
                update_levels(arg.clone(), own_level, own_id)?;
            }
            update_levels(retrn.clone(), own_level, own_id)
        }

        Type::Tuple { elems, .. } => {
            for elem in elems {
                update_levels(elem.clone(), own_level, own_id)?
            }
            Ok(())
        }

        Type::Var { .. } => unreachable!(),
    }
}

fn match_fun_type(
    typ: Arc<Type>,
    arity: usize,
    environment: &mut Environment<'_, '_>,
) -> Result<(Vec<Arc<Type>>, Arc<Type>), MatchFunTypeError> {
    if let Type::Var { type_: typ } = typ.deref() {
        let new_value = match typ.borrow().deref() {
            TypeVar::Link { type_: typ, .. } => {
                return match_fun_type(typ.clone(), arity, environment)
            }

            TypeVar::Unbound { level, .. } => {
                let args: Vec<_> = (0..arity)
                    .map(|_| environment.new_unbound_var(*level))
                    .collect();
                let retrn = environment.new_unbound_var(*level);
                Some((args, retrn))
            }

            TypeVar::Generic { .. } => None,
        };

        if let Some((args, retrn)) = new_value {
            *typ.borrow_mut() = TypeVar::Link {
                type_: fn_(args.clone(), retrn.clone()),
            };
            return Ok((args, retrn));
        }
    }

    if let Type::Fn { args, retrn } = typ.deref() {
        return if args.len() != arity {
            Err(MatchFunTypeError::IncorrectArity {
                expected: args.len(),
                given: arity,
            })
        } else {
            Ok((args.clone(), retrn.clone()))
        };
    }

    Err(MatchFunTypeError::NotFn { typ })
}

/// Takes a level and a type and turns all type variables within the type that have
/// level higher than the input level into generalized (polymorphic) type variables.
///
fn generalise(t: Arc<Type>, ctx_level: usize) -> Arc<Type> {
    match t.deref() {
        Type::Var { type_: typ } => {
            let new_var = match typ.borrow().deref() {
                TypeVar::Unbound { id, level } => {
                    let id = *id;
                    if *level > ctx_level {
                        return Arc::new(Type::Var {
                            type_: Arc::new(RefCell::new(TypeVar::Generic { id })),
                        });
                    } else {
                        Some(TypeVar::Unbound { id, level: *level })
                    }
                }

                TypeVar::Link { type_: typ } => return generalise(typ.clone(), ctx_level),

                TypeVar::Generic { .. } => None,
            };

            if let Some(v) = new_var {
                *typ.borrow_mut() = v;
            }
            Arc::new(Type::Var { type_: typ.clone() })
        }

        Type::App {
            public,
            module,
            name,
            args,
        } => {
            let args = args
                .iter()
                .map(|t| generalise(t.clone(), ctx_level))
                .collect();
            Arc::new(Type::App {
                public: *public,
                module: module.clone(),
                name: name.clone(),
                args,
            })
        }

        Type::Fn { args, retrn } => fn_(
            args.iter()
                .map(|t| generalise(t.clone(), ctx_level))
                .collect(),
            generalise(retrn.clone(), ctx_level),
        ),

        Type::Tuple { elems } => tuple(
            elems
                .iter()
                .map(|t| generalise(t.clone(), ctx_level))
                .collect(),
        ),
    }
}

fn make_type_vars(
    args: &[String],
    location: &SrcSpan,
    hydrator: &mut Hydrator,
    environment: &mut Environment<'_, '_>,
) -> Result<Vec<Arc<Type>>, Error> {
    args.iter()
        .map(|arg| TypeAst::Var {
            location: *location,
            name: arg.to_string(),
        })
        .map(|ast| hydrator.type_from_ast(&ast, environment))
        .try_collect()
}

fn custom_type_accessors<A>(
    constructors: &[RecordConstructor<A>],
    hydrator: &mut Hydrator,
    environment: &mut Environment<'_, '_>,
) -> Result<Option<HashMap<String, RecordAccessor>>, Error> {
    // Get the constructor for this custom type.
    let args = match constructors {
        [constructor] if !constructor.arguments.is_empty() => &constructor.arguments,
        // If there is not exactly 1 constructor we return as we cannot
        // build any constructors.
        _ => return Ok(None),
    };

    let mut fields = HashMap::with_capacity(args.len());
    hydrator.disallow_new_type_variables();
    for (index, RecordConstructorArg { label, ast, .. }) in args.iter().enumerate() {
        if let Some(label) = label {
            let typ = hydrator.type_from_ast(ast, environment)?;
            let _ = fields.insert(
                label.to_string(),
                RecordAccessor {
                    index: index as u64,
                    label: label.to_string(),
                    type_: typ,
                },
            );
        }
    }
    Ok(Some(fields))
}

/// Iterate over a module, registering any new types created by the module into the typer
pub fn register_types<'a>(
    statement: &'a UntypedStatement,
    module: &[String],
    hydrators: &mut HashMap<String, Hydrator>,
    names: &mut HashMap<&'a str, &'a SrcSpan>,
    environment: &mut Environment<'_, '_>,
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
                module: module.to_owned(),
                name: name.clone(),
                args: parameters.clone(),
            });

            // Insert into the module scope
            environment.insert_type_constructor(
                name.clone(),
                TypeConstructor {
                    origin: *location,
                    module: module.to_owned(),
                    public: *public,
                    parameters,
                    typ,
                },
            )?;

            // Keep track of private types so we can tell if they are later unused
            if !public {
                let _ = environment.init_usage(name.clone(), EntityKind::PrivateType, *location);
            }
        }

        Statement::CustomType {
            name,
            public,
            parameters,
            location,
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
            let _ = hydrators.insert(name.to_string(), hydrator);

            environment.insert_type_constructor(
                name.clone(),
                TypeConstructor {
                    origin: *location,
                    module: module.to_owned(),
                    public: *public,
                    parameters,
                    typ,
                },
            )?;
            // Keep track of private types so we can tell if they are later unused
            if !public {
                let _ = environment.init_usage(name.clone(), EntityKind::PrivateType, *location);
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
                    module: module.to_owned(),
                    public: *public,
                    parameters,
                    typ,
                },
            )?;

            // Keep track of private types so we can tell if they are later unused
            if !public {
                let _ = environment.init_usage(name.clone(), EntityKind::PrivateType, *location);
            }
        }

        _ => {}
    }

    Ok(())
}

pub fn register_import(
    s: &UntypedStatement,
    environment: &mut Environment<'_, '_>,
) -> Result<(), Error> {
    match s {
        Statement::Import {
            module,
            as_name,
            unqualified,
            ..
        } => {
            // Find imported module
            let module_info = environment
                .importable_modules
                .get(&module.join("/"))
                .gleam_expect(
                    "Typer could not find a module being imported. 
Missing modules should be detected prior to type checking",
                );

            let (_, imported_module) = module_info;

            // Determine local alias of imported module
            let module_name = as_name
                .as_ref()
                .or_else(|| module.last())
                .gleam_expect("Typer could not identify module name.")
                .clone();

            // Insert unqualified imports into scope
            for UnqualifiedImport {
                name,
                location,
                as_name,
            } in unqualified
            {
                let mut type_imported = false;
                let mut value_imported = false;
                let mut variant = None;

                let imported_name = as_name.as_ref().unwrap_or(name);

                // Register the unqualified import if it is a value
                if let Some(value) = imported_module.values.get(name) {
                    environment.insert_variable(
                        imported_name.clone(),
                        value.variant.clone(),
                        value.type_.clone(),
                        *location,
                    );
                    variant = Some(&value.variant);
                    value_imported = true;
                }

                // Register the unqualified import if it is a type constructor
                if let Some(typ) = imported_module.types.get(name) {
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
                    let _ = environment.init_usage(
                        imported_name.to_string(),
                        EntityKind::ImportedTypeAndConstructor,
                        *location,
                    );
                } else if type_imported {
                    let _ = environment.init_usage(
                        imported_name.to_string(),
                        EntityKind::ImportedType,
                        *location,
                    );
                } else if value_imported {
                    let _ = match variant {
                        Some(&ValueConstructorVariant::Record { .. }) => environment.init_usage(
                            imported_name.to_string(),
                            EntityKind::ImportedConstructor,
                            *location,
                        ),
                        _ => environment.init_usage(
                            imported_name.to_string(),
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
                        value_constructors: imported_module
                            .values
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                        type_constructors: imported_module
                            .types
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                    });
                }
            }

            // Insert imported module into scope
            // TODO: use a refernce to the module to avoid copying
            let _ = environment
                .imported_modules
                .insert(module_name, module_info.clone());
            Ok(())
        }

        _ => Ok(()),
    }
}
