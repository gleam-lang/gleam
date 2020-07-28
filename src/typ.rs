mod environment;
mod error;
mod expr;
mod fields;
mod hydrator;
mod pattern;
mod prelude;
pub mod pretty;
#[cfg(test)]
mod tests;

pub use environment::*;
pub use error::{Error, Warning};
pub use expr::*;
pub use prelude::*;

use crate::{
    ast::{
        self, ArgNames, BitStringSegment, BitStringSegmentOption, CallArg, Constant, Pattern,
        RecordConstructor, SrcSpan, Statement, TypeAst, TypedArg, TypedConstant, TypedExpr,
        TypedModule, TypedPattern, TypedPatternBitStringSegment, TypedRecordUpdateArg,
        TypedStatement, UnqualifiedImport, UntypedArg, UntypedExpr, UntypedModule,
        UntypedMultiPattern, UntypedPattern, UntypedRecordUpdateArg, UntypedStatement,
    },
    bit_string::BinaryTypeSpecifier,
    build::Origin,
    error::GleamExpect,
};

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use error::*;
use fields::FieldMap;
use hydrator::Hydrator;

pub trait HasType {
    fn typ(&self) -> Arc<Type>;
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
        typ: Arc<RefCell<TypeVar>>,
    },

    Tuple {
        elems: Vec<Arc<Type>>,
    },
}

impl Type {
    pub fn is_result(&self) -> bool {
        match self {
            Type::App { name, module, .. } if "Result" == name && module.is_empty() => true,
            _ => false,
        }
    }

    pub fn is_unbound(&self) -> bool {
        match self {
            Type::Var { typ } => typ.borrow().is_unbound(),
            _ => false,
        }
    }

    /// Get the args for the type if the type is a specific Type::App.
    /// Returns None if the type is not a Type::App or is an incorrect Type:App
    ///
    pub fn get_app_args(
        &self,
        public: bool,
        module: &[String],
        name: &str,
        arity: usize,
        environment: &mut Environment,
    ) -> Option<Vec<Arc<Type>>> {
        match self {
            Type::App {
                module: m,
                name: n,
                args,
                ..
            } => {
                if *module == m[..] && name == n && args.len() == arity {
                    Some(args.clone())
                } else {
                    None
                }
            }

            Type::Var { typ } => {
                let args: Vec<_> = match &*typ.borrow() {
                    TypeVar::Link { typ } => {
                        return typ.get_app_args(public, module, name, arity, environment);
                    }

                    TypeVar::Unbound { level, .. } => (0..arity)
                        .map(|_| environment.new_unbound_var(*level))
                        .collect(),

                    TypeVar::Generic { .. } => return None,
                };

                // TODO: use the real type here rather than making a copy
                *typ.borrow_mut() = TypeVar::Link {
                    typ: Arc::new(Type::App {
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

    pub fn find_private_type(&self) -> Option<Type> {
        match self {
            Type::App { public: false, .. } => Some(self.clone()),

            Type::App { args, .. } => args.iter().find_map(|t| t.find_private_type()),

            Type::Tuple { elems, .. } => elems.iter().find_map(|t| t.find_private_type()),

            Type::Fn { retrn, args, .. } => retrn
                .find_private_type()
                .or_else(|| args.iter().find_map(|t| t.find_private_type())),

            Type::Var { typ, .. } => match &*typ.borrow() {
                TypeVar::Unbound { .. } => None,

                TypeVar::Generic { .. } => None,

                TypeVar::Link { typ, .. } => typ.find_private_type(),
            },
        }
    }

    pub fn fn_arity(&self) -> Option<usize> {
        match self {
            Type::Fn { args, .. } => Some(args.len()),
            _ => None,
        }
    }
}

pub fn collapse_links(t: Arc<Type>) -> Arc<Type> {
    if let Type::Var { typ } = &*t {
        if let TypeVar::Link { typ } = &*typ.borrow() {
            return typ.clone();
        }
    }
    t
}

#[derive(Debug, PartialEq, Clone)]
pub struct AccessorsMap {
    pub public: bool,
    pub typ: Arc<Type>,
    pub accessors: HashMap<String, RecordAccessor>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct RecordAccessor {
    pub index: u64,
    pub label: String,
    pub typ: Arc<Type>,
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
        field_map: Option<FieldMap>,
    },
}

impl ValueConstructorVariant {
    fn to_module_value_constructor(&self) -> ModuleValueConstructor {
        match self {
            ValueConstructorVariant::Record { name, field_map } => ModuleValueConstructor::Record {
                name: name.clone(),
                arity: field_map.as_ref().map_or(0, |fm| fm.arity),
            },

            ValueConstructorVariant::ModuleConstant { literal } => {
                ModuleValueConstructor::Constant {
                    literal: literal.clone(),
                }
            }

            ValueConstructorVariant::LocalVariable { .. }
            | ValueConstructorVariant::ModuleFn { .. } => ModuleValueConstructor::Fn,
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
    fn with_environment<T>(&mut self, f: impl FnOnce(&mut Environment) -> T) -> T;

    fn instantiate(
        &mut self,
        t: Arc<Type>,
        ctx_level: usize,
        ids: &mut im::HashMap<usize, Arc<Type>>,
    ) -> Arc<Type> {
        self.with_environment(|e| e.instantiate(t, ctx_level, ids))
    }

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
    Link { typ: Arc<Type> },
    Generic { id: usize },
}

impl TypeVar {
    pub fn is_unbound(&self) -> bool {
        match self {
            TypeVar::Unbound { .. } => true,
            _ => false,
        }
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
    pub typ: Arc<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAliasConstructor {
    pub public: bool,
    pub module: Vec<String>,
    pub typ: Type,
    pub arity: usize,
}

impl ValueConstructor {
    fn field_map(&self) -> Option<&FieldMap> {
        match self.variant {
            ValueConstructorVariant::ModuleFn { ref field_map, .. }
            | ValueConstructorVariant::Record { ref field_map, .. } => field_map.as_ref(),
            _ => None,
        }
    }
}

/// Crawl the AST, annotating each node with the inferred type or
/// returning an error.
///
pub fn infer_module(
    module: UntypedModule,
    modules: &HashMap<String, (Origin, Module)>,
    warnings: &mut Vec<Warning>,
) -> Result<TypedModule, Error> {
    let mut environment = Environment::new(module.name.as_slice(), modules, warnings);
    let module_name = &module.name;

    // Register any modules, types, and values being imported
    // We process imports first so that anything imported can be referenced
    // anywhere in the module.
    for s in module.statements.iter() {
        register_import(s, &mut environment)?;
    }

    // Register types so they can be used in constructors and functions
    // earlier in the module.
    for s in module.statements.iter() {
        register_types(s, module_name, &mut environment)?;
    }

    // Infer the types of each statement in the module
    let mut statements = Vec::with_capacity(module.statements.len());
    for statement in module.statements {
        let statement = infer_statement(statement, module_name, &mut environment)?;
        statements.push(statement);
    }

    // Remove private and imported types and values to create the public interface
    environment
        .module_types
        .retain(|_, info| info.public && &info.module == module_name);
    environment.module_values.retain(|_, info| info.public);
    environment
        .accessors
        .retain(|_, accessors| accessors.public);

    // Ensure no exported values have private types in their type signature
    for (_, value) in environment.module_values.iter() {
        if let Some(leaked) = value.typ.find_private_type() {
            return Err(Error::PrivateTypeLeak {
                location: value.origin.clone(),
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

fn infer_statement<'a>(
    s: UntypedStatement,
    module_name: &Vec<String>,
    environment: &'a mut Environment<'a>,
) -> Result<TypedStatement, Error> {
    match s {
        Statement::Fn {
            doc,
            location,
            name,
            public,
            args,
            body,
            return_annotation,
            end_location,
            ..
        } => {
            let level = 1;

            let mut field_map = FieldMap::new(args.len());
            for (i, arg) in args.iter().enumerate() {
                if let ArgNames::NamedLabelled { label, .. } = &arg.names {
                    field_map
                        .insert(label.clone(), i)
                        .map_err(|_| Error::DuplicateField {
                            label: label.to_string(),
                            location: location.clone(),
                        })?;
                }
            }
            let field_map = field_map.into_option();

            // Register a var for the function so that it can call itself recursively
            let rec = environment.new_unbound_var(level + 1);
            environment.insert_variable(
                name.clone(),
                ValueConstructorVariant::ModuleFn {
                    name: name.clone(),
                    field_map: field_map.clone(),
                    module: module_name.clone(),
                    arity: args.len(),
                },
                rec.clone(),
            );

            // Infer the type
            let (typ, args, body) = environment.in_new_scope(|environment| {
                do_infer_fn(args, body, return_annotation, environment)
            })?;

            // Assert that the inferred type matches the type of any recursive call
            environment
                .unify(rec, typ.clone())
                .map_err(|e| convert_unify_error(e, &location))?;
            let typ = generalise(typ, level);

            // Insert the function into the module's interface
            environment.insert_module_value(
                &name,
                ValueConstructor {
                    public,
                    origin: location.clone(),
                    typ: typ.clone(),
                    variant: ValueConstructorVariant::ModuleFn {
                        name: name.clone(),
                        field_map: field_map.clone(),
                        module: module_name.clone(),
                        arity: args.len(),
                    },
                },
            )?;

            // Insert the function into the environment
            environment.insert_variable(
                name.clone(),
                ValueConstructorVariant::ModuleFn {
                    name: name.clone(),
                    field_map,
                    module: module_name.clone(),
                    arity: args.len(),
                },
                typ,
            );

            let statement: TypedStatement = Statement::Fn {
                doc,
                location,
                name,
                public,
                args,
                end_location,
                return_annotation,
                return_type: body.typ(),
                body,
            };

            Ok(statement)
        }

        Statement::ExternalFn {
            doc,
            location,
            name,
            public,
            args,
            retrn,
            module,
            fun,
            ..
        } => {
            let mut hydrator = Hydrator::new();
            // Construct type of function from AST
            let (return_type, typ, field_map) = environment.in_new_scope(|environment| {
                let return_type = hydrator.type_from_ast(&retrn, environment)?;
                let mut args_types = Vec::with_capacity(args.len());
                let mut field_map = FieldMap::new(args.len());
                for (i, arg) in args.iter().enumerate() {
                    let t = hydrator.type_from_ast(&arg.typ, environment)?;
                    args_types.push(t);
                    if let Some(label) = &arg.label {
                        field_map
                            .insert(label.clone(), i)
                            .map_err(|_| Error::DuplicateField {
                                label: label.to_string(),
                                location: location.clone(),
                            })?;
                    }
                }
                let field_map = field_map.into_option();
                let typ = fn_(args_types, return_type.clone());

                Ok((return_type, typ, field_map))
            })?;

            // Insert function into module
            environment.insert_module_value(
                &name,
                ValueConstructor {
                    public,
                    typ: typ.clone(),
                    origin: location.clone(),
                    variant: ValueConstructorVariant::ModuleFn {
                        name: fun.clone(),
                        field_map: field_map.clone(),
                        module: vec![module.clone()],
                        arity: args.len(),
                    },
                },
            )?;

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
            );
            Ok(Statement::ExternalFn {
                return_type,
                doc,
                location,
                name,
                public,
                args,
                retrn,
                module,
                fun,
            })
        }

        Statement::TypeAlias {
            doc,
            location,
            public,
            alias,
            args,
            resolved_type,
            ..
        } => {
            let typ = environment
                .get_type_constructor(&None, alias.as_str())
                .gleam_expect("Could not find existing type for type alias")
                .typ
                .clone();
            Ok(Statement::TypeAlias {
                doc,
                location,
                public,
                alias,
                args,
                resolved_type,
                typ,
            })
        }

        Statement::CustomType {
            doc,
            location,
            public,
            opaque,
            name,
            args,
            constructors,
        } => {
            let mut hydrator = Hydrator::new();
            hydrator.disallow_new_type_variables();

            // This custom type was inserted into the module types in the `register_types`
            // pass, so we can expect this type to exist already.
            let return_type_constructor = environment
                .module_types
                .get(&name)
                .gleam_expect("Type for custom type not found on constructor infer pass");

            // Insert the parameter types (previously created in `register_types`) into the
            // type environment so that the constructor can reference them.
            for (typ, name) in return_type_constructor.parameters.iter().zip(args.iter()) {
                hydrator.register_type_as_created(name.clone(), typ.clone());
            }

            let retrn = return_type_constructor.typ.clone();

            // Check and register constructors
            for constructor in constructors.iter() {
                let mut field_map = FieldMap::new(constructor.args.len());
                let mut args_types = Vec::with_capacity(constructor.args.len());
                for (i, (label, arg, ..)) in constructor.args.iter().enumerate() {
                    let t = hydrator.type_from_ast(&arg, environment)?;
                    args_types.push(t);
                    if let Some(label) = label {
                        field_map
                            .insert(label.clone(), i)
                            .map_err(|_| Error::DuplicateField {
                                label: label.to_string(),
                                location: location.clone(),
                            })?;
                    }
                }
                let field_map = field_map.into_option();
                // Insert constructor function into module scope
                let typ = match constructor.args.len() {
                    0 => retrn.clone(),
                    _ => fn_(args_types, retrn.clone()),
                };

                if !opaque {
                    environment.insert_module_value(
                        &constructor.name,
                        ValueConstructor {
                            public,
                            typ: typ.clone(),
                            origin: constructor.location.clone(),
                            variant: ValueConstructorVariant::Record {
                                name: constructor.name.clone(),
                                field_map: field_map.clone(),
                            },
                        },
                    )?;
                }

                environment.insert_variable(
                    constructor.name.clone(),
                    ValueConstructorVariant::Record {
                        name: constructor.name.clone(),
                        field_map,
                    },
                    typ,
                );
            }
            Ok(Statement::CustomType {
                doc,
                location,
                public,
                opaque,
                name,
                args,
                constructors,
            })
        }

        Statement::ExternalType {
            doc,
            location,
            public,
            name,
            args,
        } => {
            // Check contained types are valid
            let mut hydrator = Hydrator::new();
            for arg in args.iter() {
                let var = TypeAst::Var {
                    location: location.clone(),
                    name: arg.to_string(),
                };
                hydrator.type_from_ast(&var, environment)?;
            }
            Ok(Statement::ExternalType {
                doc,
                location,
                public,
                name,
                args,
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
            let typ = typed_expr.typ();

            environment.insert_module_value(
                &name,
                ValueConstructor {
                    public,
                    origin: location.clone(),
                    variant: ValueConstructorVariant::ModuleConstant {
                        literal: typed_expr.clone(),
                    },
                    typ: typ.clone(),
                },
            )?;

            Ok(Statement::ModuleConstant {
                doc,
                location,
                name,
                annotation,
                public,
                value: Box::new(typed_expr),
                typ,
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
        BitStringSegmentOption::Invalid {
            label, location, ..
        } => Err(Error::InvalidBinarySegmentOption { label, location }),

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

        BitStringSegmentOption::Unit {
            value,
            location,
            short_form,
            ..
        } => {
            let value = type_check(*value, int())?;

            Ok(BitStringSegmentOption::Unit {
                location,
                short_form,
                value: Box::new(value),
            })
        }

        BitStringSegmentOption::Binary { location } => {
            Ok(BitStringSegmentOption::Binary { location })
        }
        BitStringSegmentOption::Integer { location } => {
            Ok(BitStringSegmentOption::Integer { location })
        }
        BitStringSegmentOption::Float { location } => {
            Ok(BitStringSegmentOption::Float { location })
        }
        BitStringSegmentOption::BitString { location } => {
            Ok(BitStringSegmentOption::BitString { location })
        }
        BitStringSegmentOption::UTF8 { location } => Ok(BitStringSegmentOption::UTF8 { location }),
        BitStringSegmentOption::UTF16 { location } => {
            Ok(BitStringSegmentOption::UTF16 { location })
        }
        BitStringSegmentOption::UTF32 { location } => {
            Ok(BitStringSegmentOption::UTF32 { location })
        }
        BitStringSegmentOption::UTF8Codepoint { location } => {
            Ok(BitStringSegmentOption::UTF8Codepoint { location })
        }
        BitStringSegmentOption::UTF16Codepoint { location } => {
            Ok(BitStringSegmentOption::UTF16Codepoint { location })
        }
        BitStringSegmentOption::UTF32Codepoint { location } => {
            Ok(BitStringSegmentOption::UTF32Codepoint { location })
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
                location: arg.location.clone(),
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
    if let Type::Var { typ } = &*typ {
        let new_value = match &*typ.borrow() {
            TypeVar::Link { typ, .. } => return update_levels(typ.clone(), own_level, own_id),

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

    match &*typ {
        Type::App { args, .. } => {
            for arg in args.iter() {
                update_levels(arg.clone(), own_level, own_id)?
            }
            Ok(())
        }

        Type::Fn { args, retrn } => {
            for arg in args.iter() {
                update_levels(arg.clone(), own_level, own_id)?;
            }
            update_levels(retrn.clone(), own_level, own_id)
        }

        Type::Tuple { elems, .. } => {
            for elem in elems.iter() {
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
    environment: &mut Environment,
) -> Result<(Vec<Arc<Type>>, Arc<Type>), MatchFunTypeError> {
    if let Type::Var { typ } = &*typ {
        let new_value = match &*typ.borrow() {
            TypeVar::Link { typ, .. } => return match_fun_type(typ.clone(), arity, environment),

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
                typ: fn_(args.clone(), retrn.clone()),
            };
            return Ok((args, retrn));
        }
    }

    if let Type::Fn { args, retrn } = &*typ {
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
    match &*t {
        Type::Var { typ } => {
            let new_var = match &*typ.borrow() {
                TypeVar::Unbound { id, level } => {
                    let id = *id;
                    if *level > ctx_level {
                        return Arc::new(Type::Var {
                            typ: Arc::new(RefCell::new(TypeVar::Generic { id })),
                        });
                    } else {
                        Some(TypeVar::Unbound { id, level: *level })
                    }
                }

                TypeVar::Link { typ } => return generalise(typ.clone(), ctx_level),

                TypeVar::Generic { .. } => None,
            };

            if let Some(v) = new_var {
                *typ.borrow_mut() = v;
            }
            Arc::new(Type::Var { typ: typ.clone() })
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
    environment: &mut Environment,
) -> Result<Vec<Arc<Type>>, Error> {
    args.iter()
        .map(|arg| TypeAst::Var {
            location: location.clone(),
            name: arg.to_string(),
        })
        .map(|ast| hydrator.type_from_ast(&ast, environment))
        .collect::<Result<_, _>>()
}

fn custom_type_accessors(
    constructors: &[RecordConstructor],
    hydrator: &mut Hydrator,
    environment: &mut Environment,
) -> Result<Option<HashMap<String, RecordAccessor>>, Error> {
    // Get the constructor for this custom type.
    let args = match constructors {
        [constructor] if !constructor.args.is_empty() => &constructor.args,
        // If there is not exactly 1 constructor we return as we cannot
        // build any constructors.
        _ => return Ok(None),
    };

    let mut fields = HashMap::with_capacity(args.len());
    hydrator.disallow_new_type_variables();
    for (index, (label, arg, ..)) in args.iter().enumerate() {
        if let Some(label) = label {
            let typ = hydrator.type_from_ast(arg, environment)?;
            fields.insert(
                label.to_string(),
                RecordAccessor {
                    index: index as u64,
                    label: label.to_string(),
                    typ,
                },
            );
        }
    }
    Ok(Some(fields))
}

/// Iterate over a module, registering any new types created by the module into the typer
pub fn register_types(
    statement: &UntypedStatement,
    module: &[String],
    environment: &mut Environment,
) -> Result<(), Error> {
    match statement {
        Statement::ExternalType {
            name,
            public,
            args,
            location,
            ..
        } => {
            let hydrator = Hydrator::new();
            let parameters = make_type_vars(args, location, &mut hydrator, environment)?;
            let typ = Arc::new(Type::App {
                public: *public,
                module: module.to_owned(),
                name: name.clone(),
                args: parameters.clone(),
            });

            environment.insert_type_constructor(
                name.clone(),
                TypeConstructor {
                    origin: location.clone(),
                    module: module.to_owned(),
                    public: *public,
                    parameters,
                    typ,
                },
            )?;
        }

        Statement::CustomType {
            name,
            public,
            opaque,
            args,
            constructors,
            location,
            ..
        } => {
            let hydrator = Hydrator::new();
            let parameters = make_type_vars(args, location, &mut hydrator, environment)?;
            let typ = Arc::new(Type::App {
                public: *public,
                module: module.to_owned(),
                name: name.clone(),
                args: parameters.clone(),
            });

            environment.insert_type_constructor(
                name.clone(),
                TypeConstructor {
                    origin: location.clone(),
                    module: module.to_owned(),
                    public: *public,
                    parameters,
                    typ: typ.clone(),
                },
            )?;

            // If the custom type only has a single constructor then we can access the
            // fields using the record.field syntax, so store any fields accessors.
            if let Some(accessors) =
                custom_type_accessors(constructors.as_slice(), &mut hydrator, environment)?
            {
                let map = AccessorsMap {
                    public: (*public && !*opaque),
                    accessors,
                    typ: typ.clone(),
                };
                environment.insert_accessors(name.as_ref(), map)
            }
        }

        Statement::TypeAlias {
            location,
            public,
            args,
            alias: name,
            resolved_type,
            ..
        } => {
            // Register the paramerterised types
            let hydrator = Hydrator::new();
            let parameters = make_type_vars(args, location, &mut hydrator, environment)?;

            // Disallow creation of new types outside the paramerterised types
            hydrator.disallow_new_type_variables();

            // Create the type that the alias resolves to
            let typ = hydrator.type_from_ast(&resolved_type, environment)?;
            environment.insert_type_constructor(
                name.clone(),
                TypeConstructor {
                    origin: location.clone(),
                    module: module.to_owned(),
                    public: *public,
                    parameters,
                    typ,
                },
            )?;
        }

        _ => {}
    }

    Ok(())
}

pub fn register_import(s: &UntypedStatement, environment: &mut Environment) -> Result<(), Error> {
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
                .gleam_expect("Typer could not find a module being imported.");

            // Determine local alias of imported module
            let module_name = match &as_name {
                None => module[module.len() - 1].clone(),
                Some(name) => name.clone(),
            };

            // Insert unqualified imports into scope
            for UnqualifiedImport {
                name,
                location,
                as_name,
            } in unqualified
            {
                let mut imported = false;

                let imported_name = match &as_name {
                    None => name,
                    Some(alias) => alias,
                };

                if let Some(value) = module_info.1.values.get(name) {
                    environment.insert_variable(
                        imported_name.clone(),
                        value.variant.clone(),
                        value.typ.clone(),
                    );
                    imported = true;
                }

                if let Some(typ) = module_info.1.types.get(name) {
                    match environment.insert_type_constructor(imported_name.clone(), typ.clone()) {
                        Ok(_) => (),
                        Err(e) => return Err(e),
                    }

                    imported = true;
                }

                if !imported {
                    return Err(Error::UnknownModuleField {
                        location: location.clone(),
                        name: name.clone(),
                        module_name: module.clone(),
                        value_constructors: module_info
                            .1
                            .values
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                        type_constructors: module_info
                            .1
                            .types
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                    });
                }
            }

            // Insert imported module into scope
            // TODO: use a refernce to the module to avoid copying
            environment
                .imported_modules
                .insert(module_name, module_info.clone());
            Ok(())
        }

        _ => Ok(()),
    }
}

fn do_infer_fn<'a, 'b>(
    args: Vec<UntypedArg>,
    body: UntypedExpr,
    return_annotation: Option<TypeAst>,
    environment: &'b mut Environment<'a>,
) -> Result<(Arc<Type>, Vec<TypedArg>, TypedExpr), Error> {
    let (args, body) = ExprTyper::new(environment).do_infer_fn(args, body, &return_annotation)?;
    let args_types = args.iter().map(|a| a.typ.clone()).collect();
    let typ = fn_(args_types, body.typ());
    Ok((typ, args, body))
}
