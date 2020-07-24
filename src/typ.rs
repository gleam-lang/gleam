//
//
//
// TODO!
// ExprTyper that holds on to a hydrator so we don't need to pass it as an argument everywhere.
//
//
//
//
//
//
//
//

mod error;
mod fields;
mod hydrator;
mod pattern;
mod prelude;
pub mod pretty;
#[cfg(test)]
mod tests;

pub use error::{Error, Warning};
pub use prelude::*;

use crate::{
    ast::{
        self, Arg, ArgNames, BinOp, BindingKind, BitStringSegment, BitStringSegmentOption, CallArg,
        Clause, ClauseGuard, Constant, HasLocation, Pattern, RecordConstructor, RecordUpdateSpread,
        SrcSpan, Statement, TypeAst, TypedArg, TypedClause, TypedClauseGuard, TypedConstant,
        TypedExpr, TypedModule, TypedMultiPattern, TypedPattern, TypedPatternBitStringSegment,
        TypedRecordUpdateArg, TypedStatement, UnqualifiedImport, UntypedArg, UntypedClause,
        UntypedClauseGuard, UntypedConstant, UntypedConstantBitStringSegment, UntypedExpr,
        UntypedExprBitStringSegment, UntypedModule, UntypedMultiPattern, UntypedPattern,
        UntypedPatternBitStringSegment, UntypedRecordUpdateArg, UntypedStatement,
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
        typer: &mut Typer,
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
                        return typ.get_app_args(public, module, name, arity, typer);
                    }

                    TypeVar::Unbound { level, .. } => {
                        (0..arity).map(|_| typer.new_unbound_var(*level)).collect()
                    }

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

#[derive(Debug)]
pub struct Typer<'a> {
    current_module: &'a [String],
    uid: usize,
    level: usize,
    importable_modules: &'a HashMap<String, (Origin, Module)>,
    imported_modules: HashMap<String, (Origin, Module)>,
    annotated_generic_types: im::HashSet<usize>,

    // Values defined in the current function (or the prelude)
    local_values: im::HashMap<String, ValueConstructor>,

    // Types defined in the current module (or the prelude)
    module_types: HashMap<String, TypeConstructor>,

    // Values defined in the current module
    module_values: HashMap<String, ValueConstructor>,

    // Accessors defined in the current module
    accessors: HashMap<String, AccessorsMap>,

    // Warnings
    warnings: &'a mut Vec<Warning>,
}

impl<'a> Typer<'a> {
    pub fn in_new_scope<T, O: FnOnce(&mut Self) -> Result<T, Error>>(
        &mut self,
        process_scope: O,
    ) -> Result<T, Error> {
        // Record initial scope state
        let initial_local_values = self.local_values.clone();
        // TODO: introduce scope for the hydrator
        // let initial_annotated_type_vars = self.annotated_type_vars.clone();
        let initial_annotated_generic_types = self.annotated_generic_types.clone();

        // Create state for new scope
        self.level += 1;

        // Process scope
        let result = process_scope(self);

        // Discard local state now scope is over
        self.level -= 1;
        self.local_values = initial_local_values;
        // TODO
        // self.annotated_type_vars = initial_annotated_type_vars;
        self.annotated_generic_types = initial_annotated_generic_types;

        // Return result of typing the scope
        result
    }

    pub fn new(
        current_module: &'a [String],
        importable_modules: &'a HashMap<String, (Origin, Module)>,
        warnings: &'a mut Vec<Warning>,
    ) -> Self {
        let typer = Self {
            uid: 0,
            level: 1,
            annotated_generic_types: im::HashSet::new(),
            module_types: HashMap::new(),
            module_values: HashMap::new(),
            imported_modules: HashMap::new(),
            accessors: HashMap::new(),
            local_values: hashmap![],
            importable_modules,
            current_module,
            warnings,
        };
        register_prelude(typer)
    }

    fn next_uid(&mut self) -> usize {
        let i = self.uid;
        self.uid += 1;
        i
    }

    fn previous_uid(&self) -> usize {
        self.uid - 1
    }

    /// Create a new unbound type that is a specific type, we just don't
    /// know which one yet.
    ///
    pub fn new_unbound_var(&mut self, level: usize) -> Arc<Type> {
        Arc::new(Type::Var {
            typ: Arc::new(RefCell::new(TypeVar::Unbound {
                id: self.next_uid(),
                level,
            })),
        })
    }

    /// Create a new generic type that can stand in for any type.
    ///
    pub fn new_generic_var(&mut self) -> Arc<Type> {
        Arc::new(Type::Var {
            typ: Arc::new(RefCell::new(TypeVar::Generic {
                id: self.next_uid(),
            })),
        })
    }

    /// Insert a variable in the current scope.
    ///
    pub fn insert_variable(
        &mut self,
        name: String,
        variant: ValueConstructorVariant,
        typ: Arc<Type>,
    ) {
        self.local_values.insert(
            name,
            ValueConstructor {
                public: false,
                origin: Default::default(), // TODO: use the real one
                variant,
                typ,
            },
        );
    }

    /// Insert a value into the current module.
    /// Errors if the module already has a value with that name.
    ///
    pub fn insert_module_value(
        &mut self,
        name: &str,
        value: ValueConstructor,
    ) -> Result<(), Error> {
        let location = value.origin.clone();
        match self.module_values.insert(name.to_string(), value) {
            None => Ok(()),
            Some(previous) => Err(Error::DuplicateName {
                location,
                previous_location: previous.origin,
                name: name.to_string(),
            }),
        }
    }

    /// Lookup a variable in the current scope.
    ///
    pub fn get_variable(&self, name: &str) -> Option<&ValueConstructor> {
        self.local_values.get(name)
    }

    /// Lookup a module constant in the current scope.
    ///
    pub fn get_module_const(&self, name: &str) -> Option<&ValueConstructor> {
        self.module_values
            .get(name)
            .filter(|ValueConstructor { variant, .. }| {
                if let ValueConstructorVariant::ModuleConstant { .. } = variant {
                    true
                } else {
                    false
                }
            })
    }

    /// Map a type in the current scope.
    /// Errors if the module already has a type with that name, unless the type is from the
    /// prelude.
    ///
    pub fn insert_type_constructor(
        &mut self,
        type_name: String,
        info: TypeConstructor,
    ) -> Result<(), Error> {
        let name = type_name.clone();
        let location = info.origin.clone();
        match self.module_types.insert(type_name, info) {
            None => Ok(()),
            Some(prelude_type) if prelude_type.module.is_empty() => Ok(()),
            Some(previous) => Err(Error::DuplicateTypeName {
                name,
                location,
                previous_location: previous.origin,
            }),
        }
    }

    /// Lookup a type in the current scope.
    ///
    pub fn get_type_constructor(
        &self,
        module_alias: &Option<String>,
        name: &str,
    ) -> Result<&TypeConstructor, GetTypeConstructorError> {
        match module_alias {
            None => {
                self.module_types
                    .get(name)
                    .ok_or_else(|| GetTypeConstructorError::UnknownType {
                        name: name.to_string(),
                        type_constructors: self
                            .module_types
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                    })
            }

            Some(m) => {
                let module = &self.imported_modules.get(m).ok_or_else(|| {
                    GetTypeConstructorError::UnknownModule {
                        name: name.to_string(),
                        imported_modules: self
                            .importable_modules
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                    }
                })?;
                module
                    .1
                    .types
                    .get(name)
                    .ok_or_else(|| GetTypeConstructorError::UnknownModuleType {
                        name: name.to_string(),
                        module_name: module.1.name.clone(),
                        type_constructors: module.1.types.keys().map(|t| t.to_string()).collect(),
                    })
            }
        }
    }

    /// Lookup a value constructor in the current scope.
    ///
    fn get_value_constructor(
        &self,
        module: Option<&String>,
        name: &str,
    ) -> Result<&ValueConstructor, GetValueConstructorError> {
        match module {
            None => self.local_values.get(name).ok_or_else(|| {
                GetValueConstructorError::UnknownVariable {
                    name: name.to_string(),
                    variables: self.local_values.keys().map(|t| t.to_string()).collect(),
                }
            }),

            Some(module) => {
                let module = self.imported_modules.get(&*module).ok_or_else(|| {
                    GetValueConstructorError::UnknownModule {
                        name: name.to_string(),
                        imported_modules: self
                            .importable_modules
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                    }
                })?;
                module.1.values.get(&*name).ok_or_else(|| {
                    GetValueConstructorError::UnknownModuleValue {
                        name: name.to_string(),
                        module_name: module.1.name.clone(),
                        value_constructors: module.1.values.keys().map(|t| t.to_string()).collect(),
                    }
                })
            }
        }
    }

    pub fn insert_accessors(&mut self, type_name: &str, accessors: AccessorsMap) {
        self.accessors.insert(type_name.to_string(), accessors);
    }

    /// Crawl the AST, annotating each node with the inferred type or
    /// returning an error.
    ///
    pub fn infer(&mut self, expr: UntypedExpr) -> Result<TypedExpr, Error> {
        match expr {
            UntypedExpr::ListNil { location, .. } => self.infer_nil(location),

            UntypedExpr::Todo {
                location, label, ..
            } => self.infer_todo(location, label),

            UntypedExpr::Var { location, name, .. } => self.infer_var(name, location),

            UntypedExpr::Int {
                location, value, ..
            } => self.infer_int(value, location),

            UntypedExpr::Seq { first, then, .. } => self.infer_seq(*first, *then),

            UntypedExpr::Tuple {
                location, elems, ..
            } => self.infer_tuple(elems, location),

            UntypedExpr::Float {
                location, value, ..
            } => self.infer_float(value, location),

            UntypedExpr::String {
                location, value, ..
            } => self.infer_string(value, location),

            UntypedExpr::Pipe {
                left,
                right,
                location,
            } => self.infer_pipe(*left, *right, location),

            UntypedExpr::Fn {
                location,
                is_capture,
                args,
                body,
                return_annotation,
                ..
            } => self.infer_fn(args, *body, is_capture, return_annotation, location),

            UntypedExpr::Let {
                location,
                pattern,
                value,
                then,
                kind,
                annotation,
                ..
            } => {
                let mut hydrator = Hydrator::new(); // TODO: expr typer with this embedded in it
                self.infer_let(
                    pattern,
                    *value,
                    *then,
                    kind,
                    &annotation,
                    location,
                    &mut hydrator,
                )
            }

            UntypedExpr::Case {
                location,
                subjects,
                clauses,
                ..
            } => self.infer_case(subjects, clauses, location),

            UntypedExpr::ListCons {
                location,
                head,
                tail,
                ..
            } => self.infer_cons(*head, *tail, location),

            UntypedExpr::Call {
                location,
                fun,
                args,
                ..
            } => self.infer_call(*fun, args, location),

            UntypedExpr::BinOp {
                location,
                name,
                left,
                right,
                ..
            } => self.infer_binop(name, *left, *right, location),

            UntypedExpr::FieldAccess {
                location,
                label,
                container,
                ..
            } => self.infer_field_access(*container, label, location),

            UntypedExpr::TupleIndex {
                location,
                index,
                tuple,
                ..
            } => self.infer_tuple_index(*tuple, index, location),

            UntypedExpr::BitString { location, segments } => {
                self.infer_bit_string(segments, location)
            }

            UntypedExpr::RecordUpdate {
                location,
                constructor,
                spread,
                args,
            } => self.infer_record_update(*constructor, spread, args, location),
        }
    }

    fn infer_pipe(
        &mut self,
        left: UntypedExpr,
        right: UntypedExpr,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        match right {
            // left |> right(..args)
            UntypedExpr::Call { fun, args, .. } => {
                let fun = self.infer(*fun)?;
                match fun.typ().fn_arity() {
                    // Rewrite as right(left, ..args)
                    Some(arity) if arity == args.len() + 1 => {
                        self.infer_insert_pipe(fun, args, left)
                    }

                    // Rewrite as right(..args)(left)
                    _ => self.infer_apply_to_call_pipe(fun, args, left, location),
                }
            }

            // right(left)
            right => self.infer_apply_pipe(left, right, location),
        }
    }

    /// Attempt to infer a |> b(..c) as b(..c)(a)
    fn infer_apply_to_call_pipe(
        &mut self,
        fun: TypedExpr,
        args: Vec<CallArg<UntypedExpr>>,
        left: UntypedExpr,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let (fun, args, typ) = self.do_infer_call_with_known_fun(fun, args, &location)?;
        let fun = TypedExpr::Call {
            location: location.clone(),
            typ,
            args,
            fun: Box::new(fun),
        };
        let args = vec![CallArg {
            label: None,
            location: left.location().clone(),
            value: left,
        }];
        let (fun, args, typ) = self.do_infer_call_with_known_fun(fun, args, &location)?;
        Ok(TypedExpr::Call {
            location,
            typ,
            args,
            fun: Box::new(fun),
        })
    }

    /// Attempt to infer a |> b(c) as b(a, c)
    fn infer_insert_pipe(
        &mut self,
        fun: TypedExpr,
        args: Vec<CallArg<UntypedExpr>>,
        left: UntypedExpr,
    ) -> Result<TypedExpr, Error> {
        let location = left.location().clone();
        let mut new_args = Vec::with_capacity(args.len() + 1);
        new_args.push(CallArg {
            label: None,
            location: left.location().clone(),
            value: left,
        });
        for arg in args {
            new_args.push(arg.clone());
        }

        let (fun, args, typ) = self.do_infer_call_with_known_fun(fun, new_args, &location)?;
        Ok(TypedExpr::Call {
            location,
            typ,
            args,
            fun: Box::new(fun),
        })
    }

    /// Attempt to infer a |> b as b(a)
    fn infer_apply_pipe(
        &mut self,
        left: UntypedExpr,
        right: UntypedExpr,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let left = Box::new(self.infer(left)?);
        let right = Box::new(self.infer(right)?);
        let typ = self.new_unbound_var(self.level);
        let fn_typ = Arc::new(Type::Fn {
            args: vec![left.typ()],
            retrn: typ.clone(),
        });
        self.unify(right.typ(), fn_typ)
            .map_err(|e| convert_unify_error(e, &location))?;

        Ok(TypedExpr::Pipe {
            location,
            typ,
            right,
            left,
        })
    }

    fn infer_nil(&mut self, location: SrcSpan) -> Result<TypedExpr, Error> {
        Ok(TypedExpr::ListNil {
            location,
            typ: list(self.new_unbound_var(self.level)),
        })
    }

    fn infer_todo(&mut self, location: SrcSpan, label: Option<String>) -> Result<TypedExpr, Error> {
        self.warnings.push(Warning::Todo {
            location: location.clone(),
        });

        Ok(TypedExpr::Todo {
            location,
            label,
            typ: self.new_unbound_var(self.level),
        })
    }

    fn infer_string(&mut self, value: String, location: SrcSpan) -> Result<TypedExpr, Error> {
        Ok(TypedExpr::String {
            location,
            value,
            typ: string(),
        })
    }

    fn infer_int(&mut self, value: String, location: SrcSpan) -> Result<TypedExpr, Error> {
        Ok(TypedExpr::Int {
            location,
            value,
            typ: int(),
        })
    }

    fn infer_float(&mut self, value: String, location: SrcSpan) -> Result<TypedExpr, Error> {
        Ok(TypedExpr::Float {
            location,
            value,
            typ: float(),
        })
    }

    fn infer_seq(&mut self, first: UntypedExpr, then: UntypedExpr) -> Result<TypedExpr, Error> {
        let first = self.infer(first)?;
        let then = self.infer(then)?;

        match first.typ().as_ref() {
            typ if typ.is_result() => {
                self.warnings.push(Warning::ImplicitlyDiscardedResult {
                    location: first.location().clone(),
                });
            }

            _ => {}
        }

        Ok(TypedExpr::Seq {
            typ: then.typ(),
            first: Box::new(first),
            then: Box::new(then),
        })
    }

    fn infer_fn(
        &mut self,
        args: Vec<UntypedArg>,
        body: UntypedExpr,
        is_capture: bool,
        return_annotation: Option<TypeAst>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let (args, body) = self.do_infer_fn(args, body, &return_annotation)?;
        let args_types = args.iter().map(|a| a.typ.clone()).collect();
        let typ = fn_(args_types, body.typ());
        Ok(TypedExpr::Fn {
            location,
            typ,
            is_capture,
            args,
            body: Box::new(body),
            return_annotation,
        })
    }

    fn do_infer_fn(
        &mut self,
        args: Vec<UntypedArg>,
        body: UntypedExpr,
        return_annotation: &Option<TypeAst>,
    ) -> Result<(Vec<TypedArg>, TypedExpr), Error> {
        // Construct an initial type for each argument of the function- either an unbound type variable
        // or a type provided by an annotation.
        let mut type_vars = hashmap![];
        let args: Vec<_> = args
            .into_iter()
            .map(|arg| self.infer_arg(arg, &mut type_vars))
            .collect::<Result<_, _>>()?;

        let body = self.in_new_scope(|body_typer| {
            for (arg, t) in args.iter().zip(args.iter().map(|arg| arg.typ.clone())) {
                match &arg.names {
                    ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => body_typer
                        .insert_variable(
                            name.to_string(),
                            ValueConstructorVariant::LocalVariable,
                            t,
                        ),
                    ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => (),
                };
            }

            body_typer.infer(body)
        })?;

        // Check that any return type annotation is accurate.
        if let Some(ann) = return_annotation {
            let ret_typ = self.type_from_ast(ann, &mut type_vars, NewTypeAction::MakeGeneric)?;
            self.unify(ret_typ, body.typ())
                .map_err(|e| convert_unify_error(e, body.location()))?;
        }

        Ok((args, body))
    }

    fn infer_arg(&mut self, arg: UntypedArg, hydrator: &mut Hydrator) -> Result<TypedArg, Error> {
        let Arg {
            names,
            annotation,
            location,
            ..
        } = arg;
        let typ = annotation
            .clone()
            .map(|t| hydrator.type_from_ast(&t, self))
            .unwrap_or_else(|| Ok(self.new_unbound_var(self.level)))?;
        Ok(Arg {
            names,
            location,
            annotation,
            typ,
        })
    }

    fn infer_call(
        &mut self,
        fun: UntypedExpr,
        args: Vec<CallArg<UntypedExpr>>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let (fun, args, typ) = self.do_infer_call(fun, args, &location)?;
        Ok(TypedExpr::Call {
            location,
            typ,
            args,
            fun: Box::new(fun),
        })
    }

    fn infer_cons(
        &mut self,
        head: UntypedExpr,
        tail: UntypedExpr,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let head = self.infer(head)?;
        let tail = self.infer(tail)?;
        self.unify(tail.typ(), list(head.typ()))
            .map_err(|e| convert_unify_error(e, &location))?;

        Ok(TypedExpr::ListCons {
            location,
            typ: tail.typ(),
            head: Box::new(head),
            tail: Box::new(tail),
        })
    }

    fn infer_tuple(
        &mut self,
        elems: Vec<UntypedExpr>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let elems = elems
            .into_iter()
            .map(|e| self.infer(e))
            .collect::<Result<Vec<_>, _>>()?;
        let typ = tuple(elems.iter().map(|e| e.typ()).collect());
        Ok(TypedExpr::Tuple {
            location,
            elems,
            typ,
        })
    }
    fn infer_var(&mut self, name: String, location: SrcSpan) -> Result<TypedExpr, Error> {
        let constructor = self.infer_value_constructor(&name, &location)?;
        Ok(TypedExpr::Var {
            constructor,
            location,
            name,
        })
    }

    fn infer_field_access(
        &mut self,
        container: UntypedExpr,
        label: String,
        access_location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        match container {
            UntypedExpr::Var { name, location, .. } if !self.local_values.contains_key(&name) => {
                self.infer_module_access(name.as_ref(), label, &location, access_location)
            }

            _ => self.infer_record_access(container, label, access_location),
        }
    }

    fn infer_tuple_index(
        &mut self,
        tuple: UntypedExpr,
        index: u64,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let tuple = self.infer(tuple)?;

        match tuple.typ().as_ref() {
            Type::Tuple { elems } => {
                let typ = elems
                    .get(index as usize)
                    .ok_or_else(|| Error::OutOfBoundsTupleIndex {
                        location: location.clone(),
                        index,
                        size: elems.len(),
                    })?
                    .clone();
                Ok(TypedExpr::TupleIndex {
                    location,
                    index,
                    tuple: Box::new(tuple),
                    typ,
                })
            }

            typ if typ.is_unbound() => Err(Error::NotATupleUnbound {
                location: tuple.location().clone(),
            }),

            _ => Err(Error::NotATuple {
                location: tuple.location().clone(),
                given: tuple.typ(),
            }),
        }
    }

    fn infer_bit_string(
        &mut self,
        segments: Vec<UntypedExprBitStringSegment>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let segments = segments
            .into_iter()
            .map(|s| {
                self.infer_bit_segment(*s.value, s.options, s.location, |env, expr| env.infer(expr))
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(TypedExpr::BitString {
            location,
            segments,
            typ: bit_string(),
        })
    }

    fn infer_constant_bit_string(
        &mut self,
        segments: Vec<UntypedConstantBitStringSegment>,
        location: SrcSpan,
    ) -> Result<TypedConstant, Error> {
        let segments = segments
            .into_iter()
            .map(|s| {
                self.infer_bit_segment(*s.value, s.options, s.location, |env, expr| {
                    env.infer_const(&None, expr)
                })
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Constant::BitString { location, segments })
    }

    fn infer_bit_segment<UntypedValue, TypedValue, InferFn>(
        &mut self,
        value: UntypedValue,
        options: Vec<BitStringSegmentOption<UntypedValue>>,
        location: SrcSpan,
        mut infer: InferFn,
    ) -> Result<BitStringSegment<TypedValue, Arc<Type>>, Error>
    where
        InferFn: FnMut(&mut Self, UntypedValue) -> Result<TypedValue, Error>,
        TypedValue: HasType + HasLocation + Clone,
    {
        let value = infer(self, value)?;

        let infer_option = |segment_option: BitStringSegmentOption<UntypedValue>| {
            infer_bit_string_segment_option(segment_option, |value, typ| {
                let typed_value = infer(self, value)?;
                self.unify(typ, typed_value.typ())
                    .map_err(|e| convert_unify_error(e, typed_value.location()))?;
                Ok(typed_value)
            })
        };

        let options = options
            .into_iter()
            .map(infer_option)
            .collect::<Result<Vec<_>, _>>()?;

        let type_specifier = BinaryTypeSpecifier::new(&options, false)
            .map_err(|e| convert_binary_error(e, &location))?;
        let typ = type_specifier.construction_typ().unwrap_or_else(|| int());

        self.unify(typ.clone(), value.typ())
            .map_err(|e| convert_unify_error(e, value.location()))?;

        Ok(BitStringSegment {
            location,
            typ,
            value: Box::new(value),
            options,
        })
    }

    fn infer_binop(
        &mut self,
        name: BinOp,
        left: UntypedExpr,
        right: UntypedExpr,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let (input_type, output_type) = match name {
            BinOp::Eq | BinOp::NotEq => {
                let left = self.infer(left)?;
                let right = self.infer(right)?;
                self.unify(left.typ(), right.typ())
                    .map_err(|e| convert_unify_error(e, right.location()))?;

                return Ok(TypedExpr::BinOp {
                    location,
                    name,
                    typ: bool(),
                    left: Box::new(left),
                    right: Box::new(right),
                });
            }
            BinOp::And => (bool(), bool()),
            BinOp::Or => (bool(), bool()),
            BinOp::LtInt => (int(), bool()),
            BinOp::LtEqInt => (int(), bool()),
            BinOp::LtFloat => (float(), bool()),
            BinOp::LtEqFloat => (float(), bool()),
            BinOp::GtEqInt => (int(), bool()),
            BinOp::GtInt => (int(), bool()),
            BinOp::GtEqFloat => (float(), bool()),
            BinOp::GtFloat => (float(), bool()),
            BinOp::AddInt => (int(), int()),
            BinOp::AddFloat => (float(), float()),
            BinOp::SubInt => (int(), int()),
            BinOp::SubFloat => (float(), float()),
            BinOp::MultInt => (int(), int()),
            BinOp::MultFloat => (float(), float()),
            BinOp::DivInt => (int(), int()),
            BinOp::DivFloat => (float(), float()),
            BinOp::ModuloInt => (int(), int()),
        };

        let left = self.infer(left)?;
        self.unify(input_type.clone(), left.typ())
            .map_err(|e| convert_unify_error(e, left.location()))?;
        let right = self.infer(right)?;
        self.unify(input_type, right.typ())
            .map_err(|e| convert_unify_error(e, right.location()))?;

        Ok(TypedExpr::BinOp {
            location,
            name,
            typ: output_type,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    fn infer_let(
        &mut self,
        pattern: UntypedPattern,
        value: UntypedExpr,
        then: UntypedExpr,
        kind: BindingKind,
        annotation: &Option<TypeAst>,
        location: SrcSpan,
        hydrator: &mut Hydrator,
    ) -> Result<TypedExpr, Error> {
        let value = self.in_new_scope(|value_typer| value_typer.infer(value))?;

        let try_value_type = self.new_unbound_var(self.level);
        let try_error_type = self.new_unbound_var(self.level);

        let value_typ = match kind {
            // Ensure that the value is a result if this is a `try` binding
            BindingKind::Try => {
                let v = try_value_type.clone();
                let e = try_error_type.clone();
                self.unify(result(v, e), value.typ())
                    .map_err(|e| convert_unify_error(e, value.location()))?;
                try_value_type.clone()
            }
            _ => value.typ(),
        };

        let value_typ = generalise(value_typ, self.level + 1);

        // Ensure the pattern matches the type of the value
        let pattern =
            pattern::PatternTyper::new(self, self.level).unify(pattern, value_typ.clone())?;

        // Check the type of the following code
        let then = self.infer(then)?;
        let typ = then.typ();

        // Ensure that a Result with the right error type is returned for `try`
        if kind == BindingKind::Try {
            let value = self.new_unbound_var(self.level);
            self.unify(result(value, try_error_type), typ.clone())
                .map_err(|e| convert_unify_error(e, then.try_binding_location()))?;
        }

        // Check that any type annotation is accurate.
        if let Some(ann) = annotation {
            let ann_typ = hydrator
                .type_from_ast(ann, self)
                .map(|t| self.instantiate(t, self.level, &mut hashmap![]))?;
            self.unify(ann_typ, value_typ)
                .map_err(|e| convert_unify_error(e, value.location()))?;
        }

        Ok(TypedExpr::Let {
            location,
            typ,
            kind,
            pattern,
            value: Box::new(value),
            then: Box::new(then),
        })
    }

    fn infer_case(
        &mut self,
        subjects: Vec<UntypedExpr>,
        clauses: Vec<UntypedClause>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let subjects_count = subjects.len();
        let mut typed_subjects = Vec::with_capacity(subjects_count);
        let mut subject_types = Vec::with_capacity(subjects_count);
        let mut typed_clauses = Vec::with_capacity(clauses.len());

        let return_type = self.new_unbound_var(self.level);

        for subject in subjects.into_iter() {
            let (subject, subject_type) = self.in_new_scope(|subject_typer| {
                let subject = subject_typer.infer(subject)?;
                let subject_type = generalise(subject.typ(), subject_typer.level);

                Ok((subject, subject_type))
            })?;

            typed_subjects.push(subject);
            subject_types.push(subject_type);
        }

        for clause in clauses.into_iter() {
            let typed_clause = self.infer_clause(clause, &subject_types)?;
            self.unify(return_type.clone(), typed_clause.then.typ())
                .map_err(|e| convert_unify_error(e, typed_clause.then.location()))?;
            typed_clauses.push(typed_clause);
        }
        Ok(TypedExpr::Case {
            location,
            typ: return_type,
            subjects: typed_subjects,
            clauses: typed_clauses,
        })
    }

    fn infer_clause(
        &mut self,
        clause: UntypedClause,
        subjects: &[Arc<Type>],
    ) -> Result<TypedClause, Error> {
        let Clause {
            pattern,
            alternative_patterns,
            guard,
            then,
            location,
        } = clause;

        let (guard, then, typed_pattern, typed_alternatives) =
            self.in_new_scope(|clause_typer| {
                // Check the types
                let (typed_pattern, typed_alternatives) = clause_typer.infer_clause_pattern(
                    pattern,
                    alternative_patterns,
                    subjects,
                    &location,
                )?;
                let guard = clause_typer.infer_optional_clause_guard(guard)?;
                let then = clause_typer.infer(then)?;

                Ok((guard, then, typed_pattern, typed_alternatives))
            })?;

        Ok(Clause {
            location,
            pattern: typed_pattern,
            alternative_patterns: typed_alternatives,
            guard,
            then,
        })
    }

    fn infer_clause_pattern(
        &mut self,
        pattern: UntypedMultiPattern,
        alternatives: Vec<UntypedMultiPattern>,
        subjects: &[Arc<Type>],
        location: &SrcSpan,
    ) -> Result<(TypedMultiPattern, Vec<TypedMultiPattern>), Error> {
        let mut pattern_typer = pattern::PatternTyper::new(self, self.level);
        let typed_pattern = pattern_typer.infer_multi_pattern(pattern, subjects, &location)?;

        // Each case clause has one or more patterns that may match the
        // subject in order for the clause to be selected, so we must type
        // check every pattern.
        let mut typed_alternatives = Vec::with_capacity(alternatives.len());
        for m in alternatives {
            typed_alternatives
                .push(pattern_typer.infer_alternative_multi_pattern(m, subjects, &location)?);
        }

        Ok((typed_pattern, typed_alternatives))
    }

    fn infer_optional_clause_guard(
        &mut self,
        guard: Option<UntypedClauseGuard>,
    ) -> Result<Option<TypedClauseGuard>, Error> {
        match guard {
            // If there is no guard we do nothing
            None => Ok(None),

            // If there is a guard we assert that it is of type Bool
            Some(guard) => {
                let guard = self.infer_clause_guard(guard)?;
                self.unify(bool(), guard.typ())
                    .map_err(|e| convert_unify_error(e, guard.location()))?;
                Ok(Some(guard))
            }
        }
    }

    fn infer_clause_guard(&mut self, guard: UntypedClauseGuard) -> Result<TypedClauseGuard, Error> {
        match guard {
            ClauseGuard::Var { location, name, .. } => {
                let constructor = self.infer_value_constructor(&name, &location)?;

                // We cannot support all values in guard expressions as the BEAM does not
                match &constructor.variant {
                    ValueConstructorVariant::LocalVariable => (),
                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::Record { .. } => {
                        return Err(Error::NonLocalClauseGuardVariable { location, name })
                    }

                    ValueConstructorVariant::ModuleConstant { literal } => {
                        return Ok(ClauseGuard::Constant(literal.clone()))
                    }
                };

                Ok(ClauseGuard::Var {
                    location,
                    name,
                    typ: constructor.typ,
                })
            }

            ClauseGuard::And {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                self.unify(bool(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(bool(), right.typ())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::And {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::Or {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                self.unify(bool(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(bool(), right.typ())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::Or {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::Equals {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(left.typ(), right.typ())
                    .map_err(|e| convert_unify_error(e, &location))?;
                Ok(ClauseGuard::Equals {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::NotEquals {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(left.typ(), right.typ())
                    .map_err(|e| convert_unify_error(e, &location))?;
                Ok(ClauseGuard::NotEquals {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::GtInt {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                self.unify(int(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(int(), right.typ())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::GtInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::GtEqInt {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                self.unify(int(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(int(), right.typ())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::GtEqInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::LtInt {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                self.unify(int(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(int(), right.typ())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::LtInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::LtEqInt {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                self.unify(int(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(int(), right.typ())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::LtEqInt {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::GtFloat {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                self.unify(float(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(float(), right.typ())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::GtFloat {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::GtEqFloat {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                self.unify(float(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(float(), right.typ())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::GtEqFloat {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::LtFloat {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                self.unify(float(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(float(), right.typ())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::LtFloat {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::LtEqFloat {
                location,
                left,
                right,
                ..
            } => {
                let left = self.infer_clause_guard(*left)?;
                self.unify(float(), left.typ())
                    .map_err(|e| convert_unify_error(e, left.location()))?;
                let right = self.infer_clause_guard(*right)?;
                self.unify(float(), right.typ())
                    .map_err(|e| convert_unify_error(e, right.location()))?;
                Ok(ClauseGuard::LtEqFloat {
                    location,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }

            ClauseGuard::Constant(constant) => {
                self.infer_const(&None, constant).map(ClauseGuard::Constant)
            }
        }
    }

    fn infer_module_access(
        &mut self,
        module_alias: &str,
        label: String,
        module_location: &SrcSpan,
        select_location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let (module_name, constructor) = {
            let module_info =
                self.imported_modules
                    .get(&*module_alias)
                    .ok_or_else(|| Error::UnknownModule {
                        name: module_alias.to_string(),
                        location: module_location.clone(),
                        imported_modules: self
                            .imported_modules
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                    })?;

            let constructor =
                module_info
                    .1
                    .values
                    .get(&label)
                    .ok_or_else(|| Error::UnknownModuleValue {
                        name: label.clone(),
                        location: select_location.clone(),
                        module_name: module_info.1.name.clone(),
                        value_constructors: module_info
                            .1
                            .values
                            .keys()
                            .map(|t| t.to_string())
                            .collect(),
                    })?;

            (module_info.1.name.clone(), constructor.clone())
        };

        Ok(TypedExpr::ModuleSelect {
            label,
            typ: self.instantiate(constructor.typ, self.level, &mut hashmap![]),
            location: select_location,
            module_name,
            module_alias: module_alias.to_string(),
            constructor: constructor.variant.to_module_value_constructor(),
        })
    }

    fn infer_record_access(
        &mut self,
        record: UntypedExpr,
        label: String,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        // Infer the type of the (presumed) record
        let record = self.infer(record)?;

        self.infer_known_record_access(record, label, location)
    }

    fn infer_known_record_access(
        &mut self,
        record: TypedExpr,
        label: String,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let record = Box::new(record);

        // If we don't yet know the type of the record then we cannot use any accessors
        if record.typ().is_unbound() {
            return Err(Error::RecordAccessUnknownType {
                location: record.location().clone(),
            });
        }

        // Error constructor helper function
        let unknown_field = |fields| Error::UnknownField {
            typ: record.typ(),
            location: location.clone(),
            label: label.clone(),
            fields,
        };

        // Check to see if it's a Type that can have accessible fields
        let accessors = match collapse_links(record.typ()).as_ref() {
            // A type in the current module which may have fields
            Type::App { module, name, .. } if module.as_slice() == self.current_module => {
                self.accessors.get(name)
            }

            // A type in another module which may have fields
            Type::App { module, name, .. } => self
                .importable_modules
                .get(&module.join("/"))
                .and_then(|module| module.1.accessors.get(name)),

            _something_without_fields => return Err(unknown_field(vec![])),
        }
        .ok_or_else(|| unknown_field(vec![]))?;

        // Find the accessor, if the type has one with the same label
        let RecordAccessor {
            index, label, typ, ..
        } = accessors
            .accessors
            .get(&label)
            .ok_or_else(|| {
                unknown_field(accessors.accessors.keys().map(|t| t.to_string()).collect())
            })?
            .clone();

        // Unify the record type with the accessor's stored copy of the record type.
        // This ensure that the type parameters of the retrieved value have the correct
        // types for this instance of the record.
        let accessor_record_type = accessors.typ.clone();
        let mut type_vars = hashmap![];
        let accessor_record_type = self.instantiate(accessor_record_type, 0, &mut type_vars);
        let typ = self.instantiate(typ, 0, &mut type_vars);
        self.unify(accessor_record_type, record.typ())
            .map_err(|e| convert_unify_error(e, record.location()))?;

        Ok(TypedExpr::RecordAccess {
            record,
            label,
            index,
            location,
            typ,
        })
    }

    fn infer_value_constructor(
        &mut self,
        name: &str,
        location: &SrcSpan,
    ) -> Result<ValueConstructor, Error> {
        let ValueConstructor {
            public,
            variant,
            origin,
            typ,
        } = self
            .get_variable(name)
            .or_else(|| self.get_module_const(name))
            .cloned()
            .ok_or_else(|| Error::UnknownVariable {
                location: location.clone(),
                name: name.to_string(),
                variables: self.local_values.keys().map(|t| t.to_string()).collect(),
            })?;
        let typ = self.instantiate(typ, self.level, &mut hashmap![]);
        Ok(ValueConstructor {
            public,
            variant,
            origin,
            typ,
        })
    }

    fn do_infer_call(
        &mut self,
        fun: UntypedExpr,
        args: Vec<CallArg<UntypedExpr>>,
        location: &SrcSpan,
    ) -> Result<(TypedExpr, Vec<TypedCallArg>, Arc<Type>), Error> {
        let fun = self.infer(fun)?;
        let (fun, args, typ) = self.do_infer_call_with_known_fun(fun, args, location)?;
        Ok((fun, args, typ))
    }

    fn do_infer_call_with_known_fun(
        &mut self,
        fun: TypedExpr,
        mut args: Vec<CallArg<UntypedExpr>>,
        location: &SrcSpan,
    ) -> Result<(TypedExpr, Vec<TypedCallArg>, Arc<Type>), Error> {
        // Check to see if the function accepts labelled arguments
        match self
            .get_field_map(&fun)
            .map_err(|e| convert_get_value_constructor_error(e, location))?
        {
            // The fun has a field map so labelled arguments may be present and need to be reordered.
            Some(field_map) => field_map.reorder(&mut args, location)?,

            // The fun has no field map and so we error if arguments have been labelled
            None => assert_no_labelled_arguments(&args)?,
        }

        // Extract the type of the fun, ensuring it actually is a function
        let (mut args_types, return_type) = match_fun_type(fun.typ(), args.len(), self)
            .map_err(|e| convert_not_fun_error(e, fun.location(), location))?;

        // Ensure that the given args have the correct types
        let args = args_types
            .iter_mut()
            .zip(args)
            .map(|(typ, arg): (&mut Arc<Type>, _)| {
                let CallArg {
                    label,
                    value,
                    location,
                } = arg;
                let value = self.infer(value)?;
                self.unify(typ.clone(), value.typ())
                    .map_err(|e| convert_unify_error(e, value.location()))?;
                Ok(CallArg {
                    label,
                    value,
                    location,
                })
            })
            .collect::<Result<_, _>>()?;
        Ok((fun, args, return_type))
    }

    // TODO: extract the type annotation checking into a infer_module_const
    // function that uses this function internally
    fn infer_const(
        &mut self,
        annotation: &Option<TypeAst>,
        value: UntypedConstant,
        hydrator: &mut Hydrator,
    ) -> Result<TypedConstant, Error> {
        let inferred = match value {
            Constant::Int {
                location, value, ..
            } => Ok(Constant::Int { location, value }),

            Constant::Float {
                location, value, ..
            } => Ok(Constant::Float { location, value }),

            Constant::String {
                location, value, ..
            } => Ok(Constant::String { location, value }),

            Constant::Tuple {
                elements, location, ..
            } => self.infer_const_tuple(elements, location),

            Constant::List {
                elements, location, ..
            } => self.infer_const_list(elements, location),

            Constant::BitString { location, segments } => {
                self.infer_constant_bit_string(segments, location)
            }

            Constant::Record {
                module,
                location,
                name,
                mut args,
                ..
            } => {
                let constructor = self.infer_value_constructor(&name, &location)?;

                let tag = match &constructor.variant {
                    ValueConstructorVariant::Record { name, .. } => name.clone(),

                    ValueConstructorVariant::ModuleFn { .. }
                    | ValueConstructorVariant::LocalVariable => {
                        return Err(Error::NonLocalClauseGuardVariable { location, name })
                    }

                    ValueConstructorVariant::ModuleConstant { literal } => {
                        return Ok(literal.clone())
                    }
                };

                // Pretty much all the other infer functions operate on UntypedExpr
                // or TypedExpr rather than ClauseGuard. To make things easier we
                // build the TypedExpr equivalent of the constructor and use that
                let fun = TypedExpr::Var {
                    constructor,
                    location: location.clone(),
                    name: name.clone(),
                };

                // This is basically the same code as do_infer_call_with_known_fun()
                // except the args are typed with infer_clause_guard() here.
                // This duplication is a bit awkward but it works!
                // Potentially this could be improved later
                match self
                    .get_field_map(&fun)
                    .map_err(|e| convert_get_value_constructor_error(e, &location))?
                {
                    // The fun has a field map so labelled arguments may be present and need to be reordered.
                    Some(field_map) => field_map.reorder(&mut args, &location)?,

                    // The fun has no field map and so we error if arguments have been labelled
                    None => assert_no_labelled_arguments(&args)?,
                }

                let (mut args_types, return_type) = match_fun_type(fun.typ(), args.len(), self)
                    .map_err(|e| convert_not_fun_error(e, fun.location(), &location))?;
                let args = args_types
                    .iter_mut()
                    .zip(args)
                    .map(|(typ, arg): (&mut Arc<Type>, _)| {
                        let CallArg {
                            label,
                            value,
                            location,
                        } = arg;
                        let value = self.infer_const(&None, value)?;
                        self.unify(typ.clone(), value.typ())
                            .map_err(|e| convert_unify_error(e, value.location()))?;
                        Ok(CallArg {
                            label,
                            value,
                            location,
                        })
                    })
                    .collect::<Result<_, _>>()?;

                Ok(Constant::Record {
                    module,
                    location,
                    name,
                    args,
                    typ: return_type,
                    tag,
                })
            }
        }?;

        // Check type annotation is accurate.
        if let Some(ann) = annotation {
            let mut type_vars = hashmap![]; // TODO: use the one for this scope
            let const_ann = hydrator.type_from_ast(&ann, self)?;
            self.unify(const_ann, inferred.typ())
                .map_err(|e| convert_unify_error(e, inferred.location()))?;
        };

        Ok(inferred)
    }

    fn infer_const_tuple(
        &mut self,
        untyped_elements: Vec<UntypedConstant>,
        location: SrcSpan,
    ) -> Result<TypedConstant, Error> {
        let mut elements = Vec::with_capacity(untyped_elements.len());

        for element in untyped_elements.into_iter() {
            let element = self.infer_const(&None, element)?;
            elements.push(element);
        }

        Ok(Constant::Tuple { elements, location })
    }

    fn infer_const_list(
        &mut self,
        untyped_elements: Vec<UntypedConstant>,
        location: SrcSpan,
    ) -> Result<TypedConstant, Error> {
        let typ = self.new_unbound_var(0);
        let mut elements = Vec::with_capacity(untyped_elements.len());

        for element in untyped_elements.into_iter() {
            let element = self.infer_const(&None, element)?;
            self.unify(typ.clone(), element.typ())
                .map_err(|e| convert_unify_error(e, element.location()))?;
            elements.push(element);
        }

        Ok(Constant::List {
            elements,
            location,
            typ: list(typ),
        })
    }

    fn infer_record_update(
        &mut self,
        constructor: UntypedExpr,
        spread: RecordUpdateSpread,
        args: Vec<UntypedRecordUpdateArg>,
        location: SrcSpan,
    ) -> Result<TypedExpr, Error> {
        let (module, name) = match self.infer(constructor.clone())? {
            TypedExpr::ModuleSelect {
                module_alias,
                label,
                ..
            } => (Some(module_alias), label),

            TypedExpr::Var { name, .. } => (None, name),

            constructor => {
                return Err(Error::RecordUpdateInvalidConstructor {
                    location: constructor.location().clone(),
                })
            }
        };

        let value_constructor = self
            .get_value_constructor(module.as_ref(), &name)
            .map_err(|e| convert_get_value_constructor_error(e, &location))?
            .clone();

        if let ValueConstructor {
            variant:
                ValueConstructorVariant::Record {
                    field_map: Some(field_map),
                    ..
                },
            ..
        } = value_constructor
        {
            if let Type::Fn { retrn, .. } = value_constructor.typ.as_ref() {
                let spread = self.infer_var(spread.name, spread.location)?;
                let return_type = self.instantiate(retrn.clone(), self.level, &mut hashmap![]);

                // Check that the spread variable unifies with the return type of the constructor
                self.unify(return_type.clone(), spread.typ())
                    .map_err(|e| convert_unify_error(e, spread.location()))?;

                let args: Vec<TypedRecordUpdateArg> = args
                    .iter()
                    .map(
                        |UntypedRecordUpdateArg {
                             label,
                             value,
                             location,
                             ..
                         }| {
                            let value = self.infer(value.clone())?;
                            let spread_field = self.infer_known_record_access(
                                spread.clone(),
                                label.to_string(),
                                location.clone(),
                            )?;

                            // Check that the update argument unifies with the corresponding
                            // field in the record contained within the spread variable. We
                            // need to check the spread, and not the constructor, in order
                            // to handle polymorphic types.
                            self.unify(spread_field.typ(), value.typ())
                                .map_err(|e| convert_unify_error(e, value.location()))?;

                            match field_map.fields.get(label) {
                                None => crate::error::fatal_compiler_bug(
                                    "Failed to lookup record field after successfully inferring that field",
                                ),
                                Some(p) => Ok(TypedRecordUpdateArg {
                                    location: location.clone(),
                                    label: label.to_string(),
                                    value,
                                    index: *p,
                                }),
                            }
                        },
                    )
                    .collect::<Result<_, _>>()?;

                return Ok(TypedExpr::RecordUpdate {
                    location,
                    typ: spread.typ(),
                    spread: Box::new(spread),
                    args,
                });
            }
        };

        Err(Error::RecordUpdateInvalidConstructor {
            location: constructor.location().clone(),
        })
    }

    /// Instantiate converts generic variables into unbound ones.
    ///
    fn instantiate(
        &mut self,
        t: Arc<Type>,
        ctx_level: usize,
        ids: &mut im::HashMap<usize, Arc<Type>>,
    ) -> Arc<Type> {
        match &*t {
            Type::App {
                public,
                name,
                module,
                args,
            } => {
                let args = args
                    .iter()
                    .map(|t| self.instantiate(t.clone(), ctx_level, ids))
                    .collect();
                Arc::new(Type::App {
                    public: *public,
                    name: name.clone(),
                    module: module.clone(),
                    args,
                })
            }

            Type::Var { typ } => {
                match &*typ.borrow() {
                    TypeVar::Link { typ } => return self.instantiate(typ.clone(), ctx_level, ids),

                    TypeVar::Unbound { .. } => return Arc::new(Type::Var { typ: typ.clone() }),

                    TypeVar::Generic { id } => match ids.get(id) {
                        Some(t) => return t.clone(),
                        None => {
                            if !self.annotated_generic_types.contains(id) {
                                // Check this in the hydrator, i.e. is it a created type
                                let v = self.new_unbound_var(ctx_level);
                                ids.insert(*id, v.clone());
                                return v;
                            }
                        }
                    },
                }
                Arc::new(Type::Var { typ: typ.clone() })
            }

            Type::Fn { args, retrn, .. } => fn_(
                args.iter()
                    .map(|t| self.instantiate(t.clone(), ctx_level, ids))
                    .collect(),
                self.instantiate(retrn.clone(), ctx_level, ids),
            ),

            Type::Tuple { elems } => tuple(
                elems
                    .iter()
                    .map(|t| self.instantiate(t.clone(), ctx_level, ids))
                    .collect(),
            ),
        }
    }

    fn make_type_vars(
        &mut self,
        args: &[String],
        location: &SrcSpan,
        hydrator: &mut Hydrator,
    ) -> Result<Vec<Arc<Type>>, Error> {
        args.iter()
            .map(|arg| TypeAst::Var {
                location: location.clone(),
                name: arg.to_string(),
            })
            .map(|ast| hydrator.type_from_ast(&ast, self))
            .collect::<Result<_, _>>()
    }

    fn custom_type_accessors(
        &mut self,
        constructors: &[RecordConstructor],
        hydrator: &mut Hydrator,
    ) -> Result<Option<HashMap<String, RecordAccessor>>, Error> {
        // Get the constructor for this custom type.
        let args = match constructors {
            // If there is not exactly 1 constructor we return as we cannot
            // build any constructors.
            _ => return Ok(None),
            [constructor] if !constructor.args.is_empty() => &constructor.args,
        };

        let mut fields = HashMap::with_capacity(args.len());
        hydrator.disallow_new_type_variables();
        for (index, (label, arg, ..)) in args.iter().enumerate() {
            if let Some(label) = label {
                let typ = hydrator.type_from_ast(arg, self)?;
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

    fn get_field_map(
        &mut self,
        constructor: &TypedExpr,
    ) -> Result<Option<&FieldMap>, GetValueConstructorError> {
        let (module, name) = match constructor {
            TypedExpr::ModuleSelect {
                module_alias,
                label,
                ..
            } => (Some(module_alias), label),

            TypedExpr::Var { name, .. } => (None, name),

            _ => return Ok(None),
        };

        Ok(self.get_value_constructor(module, name)?.field_map())
    }

    /// Unify two types that should be the same.
    /// Any unbound type variables will be linked to the other type as they are the same.
    ///
    /// It two types are found to not be the same an error is returned.
    ///
    fn unify(&mut self, t1: Arc<Type>, t2: Arc<Type>) -> Result<(), UnifyError> {
        if t1 == t2 {
            return Ok(());
        }

        // Collapse right hand side type links. Left hand side will be collapsed in the next block.
        if let Type::Var { typ } = &*t2 {
            if let TypeVar::Link { typ } = &*typ.borrow() {
                return self.unify(t1, typ.clone());
            }
        }

        if let Type::Var { typ } = &*t1 {
            enum Action {
                Unify(Arc<Type>),
                CouldNotUnify,
                Link,
            }

            let action = match &*typ.borrow() {
                TypeVar::Link { typ } => Action::Unify(typ.clone()),

                TypeVar::Unbound { id, level } => {
                    update_levels(t2.clone(), *level, *id)?;
                    Action::Link
                }

                TypeVar::Generic { id } => {
                    if let Type::Var { typ } = &*t2 {
                        if typ.borrow().is_unbound() {
                            *typ.borrow_mut() = TypeVar::Generic { id: *id };
                            return Ok(());
                        }
                    }
                    Action::CouldNotUnify
                }
            };

            return match action {
                Action::Link => {
                    *typ.borrow_mut() = TypeVar::Link { typ: t2 };
                    Ok(())
                }

                Action::Unify(t) => self.unify(t, t2),

                Action::CouldNotUnify => Err(UnifyError::CouldNotUnify {
                    expected: t1.clone(),
                    given: t2,
                }),
            };
        }

        if let Type::Var { .. } = *t2 {
            return self.unify(t2, t1).map_err(flip_unify_error);
        }

        match (&*t1, &*t2) {
            (
                Type::App {
                    module: m1,
                    name: n1,
                    args: args1,
                    ..
                },
                Type::App {
                    module: m2,
                    name: n2,
                    args: args2,
                    ..
                },
            ) if m1 == m2 && n1 == n2 && args1.len() == args2.len() => {
                for (a, b) in args1.iter().zip(args2) {
                    unify_enclosed_type(t1.clone(), t2.clone(), self.unify(a.clone(), b.clone()))?;
                }
                Ok(())
            }

            (Type::Tuple { elems: elems1, .. }, Type::Tuple { elems: elems2, .. })
                if elems1.len() == elems2.len() =>
            {
                for (a, b) in elems1.iter().zip(elems2) {
                    unify_enclosed_type(t1.clone(), t2.clone(), self.unify(a.clone(), b.clone()))?;
                }
                Ok(())
            }

            (
                Type::Fn {
                    args: args1,
                    retrn: retrn1,
                    ..
                },
                Type::Fn {
                    args: args2,
                    retrn: retrn2,
                    ..
                },
            ) if args1.len() == args2.len() => {
                for (a, b) in args1.iter().zip(args2) {
                    self.unify(a.clone(), b.clone())
                        .map_err(|_| UnifyError::CouldNotUnify {
                            expected: t1.clone(),
                            given: t2.clone(),
                        })?;
                }
                self.unify(retrn1.clone(), retrn2.clone())
                    .map_err(|_| UnifyError::CouldNotUnify {
                        expected: t1.clone(),
                        given: t2.clone(),
                    })
            }

            (_, _) => Err(UnifyError::CouldNotUnify {
                expected: t1.clone(),
                given: t2.clone(),
            }),
        }
    }

    fn register_import(&mut self, s: &UntypedStatement) -> Result<(), Error> {
        match s {
            Statement::Import {
                module,
                as_name,
                unqualified,
                ..
            } => {
                // Find imported module
                let module_info = self
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
                        self.insert_variable(
                            imported_name.clone(),
                            value.variant.clone(),
                            value.typ.clone(),
                        );
                        imported = true;
                    }

                    if let Some(typ) = module_info.1.types.get(name) {
                        match self.insert_type_constructor(imported_name.clone(), typ.clone()) {
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
                self.imported_modules
                    .insert(module_name, module_info.clone());
                Ok(())
            }

            _ => Ok(()),
        }
    }

    /// Iterate over a module, registering any new types created by the module into the typer
    fn register_types(
        &mut self,
        statement: &UntypedStatement,
        module: &[String],
    ) -> Result<(), Error> {
        match statement {
            Statement::ExternalType {
                name,
                public,
                args,
                location,
                ..
            } => {
                let mut hydrator = Hydrator::new();
                let parameters = self.make_type_vars(args, location, &mut hydrator)?;
                let typ = Arc::new(Type::App {
                    public: *public,
                    module: module.to_owned(),
                    name: name.clone(),
                    args: parameters.clone(),
                });

                self.insert_type_constructor(
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
                let mut hydrator = Hydrator::new();
                let parameters = self.make_type_vars(args, location, &mut hydrator)?;
                let typ = Arc::new(Type::App {
                    public: *public,
                    module: module.to_owned(),
                    name: name.clone(),
                    args: parameters.clone(),
                });

                self.insert_type_constructor(
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
                    self.custom_type_accessors(constructors.as_slice(), &mut hydrator)?
                {
                    let map = AccessorsMap {
                        public: (*public && !*opaque),
                        accessors,
                        typ: typ.clone(),
                    };
                    self.insert_accessors(name.as_ref(), map)
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
                let mut hydrator = Hydrator::new();
                // Register the paramerterised types
                let parameters = self.make_type_vars(args, location, &mut hydrator)?;

                // Disallow creation of new types outside the paramerterised types
                hydrator.disallow_new_type_variables();

                // Create the type that the alias resolves to
                let typ = hydrator.type_from_ast(&resolved_type, self)?;
                self.insert_type_constructor(
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
    let mut typer = Typer::new(module.name.as_slice(), modules, warnings);
    let module_name = &module.name;

    // Register any modules, types, and values being imported
    // We process imports first so that anything imported can be referenced
    // anywhere in the module.
    for s in module.statements.iter() {
        typer.register_import(s)?;
    }

    // Register types so they can be used in constructors and functions
    // earlier in the module.
    for s in module.statements.iter() {
        typer.register_types(s, module_name)?;
    }

    // Infer the types of each statement in the module
    let statements = module
        .statements
        .into_iter()
        .map(|s| infer_statement(s, module_name, &mut typer))
        .collect::<Result<Vec<_>, Error>>()?;

    // Remove private and imported types and values to create the public interface
    typer
        .module_types
        .retain(|_, info| info.public && &info.module == module_name);
    typer.module_values.retain(|_, info| info.public);
    typer.accessors.retain(|_, accessors| accessors.public);

    // Ensure no exported values have private types in their type signature
    for (_, value) in typer.module_values.iter() {
        if let Some(leaked) = value.typ.find_private_type() {
            return Err(Error::PrivateTypeLeak {
                location: value.origin.clone(),
                leaked,
            });
        }
    }

    let Typer {
        module_types: types,
        module_values: values,
        accessors,
        ..
    } = typer;

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

fn infer_statement(
    s: UntypedStatement,
    module_name: &Vec<String>,
    typer: &mut Typer,
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
            let rec = typer.new_unbound_var(level + 1);
            typer.insert_variable(
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
            let (typ, args, body) = typer.in_new_scope(|typer| {
                // TODO
                // typer.annotated_type_vars.clear();
                let (args, body) = typer.do_infer_fn(args, body, &return_annotation)?;
                let args_types = args.iter().map(|a| a.typ.clone()).collect();
                let typ = fn_(args_types, body.typ());
                Ok((typ, args, body))
            })?;

            // Assert that the inferred type matches the type of any recursive call
            typer
                .unify(rec, typ.clone())
                .map_err(|e| convert_unify_error(e, &location))?;
            let typ = generalise(typ, level);

            // Insert the function into the module's interface
            typer.insert_module_value(
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

            // Insert the function into the typerironment
            typer.insert_variable(
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
            let (return_type, typ, field_map) = typer.in_new_scope(|typer| {
                let mut type_vars = hashmap![];
                let return_type = hydrator.type_from_ast(&retrn, typer)?;
                let mut args_types = Vec::with_capacity(args.len());
                let mut field_map = FieldMap::new(args.len());
                for (i, arg) in args.iter().enumerate() {
                    let t = hydrator.type_from_ast(&arg.typ, typer)?;
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
            typer.insert_module_value(
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
            typer.insert_variable(
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
            let typ = typer
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
            let return_type_constructor = typer
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
                    let t = hydrator.type_from_ast(&arg, typer)?;
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
                    typer.insert_module_value(
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

                typer.insert_variable(
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
                hydrator.type_from_ast(&var, typer)?;
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
            let mut hydrator = Hydrator::new();
            let typed_expr = typer.infer_const(&annotation, *value, &mut hydrator)?;
            let typ = typed_expr.typ();

            typer.insert_module_value(
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
    typer: &mut Typer,
) -> Result<(Vec<Arc<Type>>, Arc<Type>), MatchFunTypeError> {
    if let Type::Var { typ } = &*typ {
        let new_value = match &*typ.borrow() {
            TypeVar::Link { typ, .. } => return match_fun_type(typ.clone(), arity, typer),

            TypeVar::Unbound { level, .. } => {
                let args: Vec<_> = (0..arity).map(|_| typer.new_unbound_var(*level)).collect();
                let retrn = typer.new_unbound_var(*level);
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
