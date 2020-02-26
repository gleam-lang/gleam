pub mod pretty;
#[cfg(test)]
mod tests;

use crate::ast::{
    self, Arg, ArgNames, BinOp, CallArg, Clause, ClauseGuard, Expr, Meta, Pattern, Statement,
    TypeAst, TypedClause, TypedClauseGuard, TypedExpr, TypedModule, TypedMultiPattern,
    TypedPattern, UnqualifiedImport, UntypedClause, UntypedClauseGuard, UntypedExpr, UntypedModule,
    UntypedMultiPattern, UntypedPattern,
};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    App {
        public: bool,
        module: Vec<String>,
        name: String,
        args: Vec<Type>,
    },

    Fn {
        args: Vec<Type>,
        retrn: Box<Type>,
    },

    Var {
        typ: Rc<RefCell<TypeVar>>,
    },

    Tuple {
        elems: Vec<Type>,
    },
}

impl Type {
    pub fn collapse_links(self) -> Type {
        if let Type::Var { typ } = &self {
            if let TypeVar::Link { typ } = &*typ.borrow() {
                return *typ.clone();
            }
        }
        self
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
        env: &mut Env,
    ) -> Option<Vec<Type>> {
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
                enum Action {
                    Link(Vec<Type>),
                }

                let action = match &*typ.borrow() {
                    TypeVar::Link { typ } => {
                        return typ.get_app_args(public, module, name, arity, env);
                    }

                    TypeVar::Unbound { level, .. } => {
                        Action::Link((0..arity).map(|_| env.new_unbound_var(*level)).collect())
                    }

                    TypeVar::Generic { .. } => return None,
                };

                match action {
                    Action::Link(args) => {
                        *typ.borrow_mut() = TypeVar::Link {
                            typ: Box::new(Type::App {
                                name: name.to_string(),
                                module: module.to_owned(),
                                args: args.clone(),
                                public,
                            }),
                        };
                        Some(args)
                    }
                }
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
pub struct FieldMap {
    arity: usize,
    fields: HashMap<String, usize>,
}

pub struct DuplicateField {}

impl FieldMap {
    pub fn new(arity: usize) -> Self {
        Self {
            arity,
            fields: HashMap::new(),
        }
    }

    pub fn insert(&mut self, label: String, index: usize) -> Result<(), DuplicateField> {
        match self.fields.insert(label, index) {
            Some(_) => Err(DuplicateField {}),
            None => Ok(()),
        }
    }

    pub fn into_option(self) -> Option<Self> {
        if self.fields.is_empty() {
            None
        } else {
            Some(self)
        }
    }

    /// Reorder an argument list so that labelled fields supplied out-of-order are in the correct
    /// order.
    ///
    fn reorder<A>(&self, args: &mut Vec<CallArg<A>>, meta: &Meta) -> Result<(), Error> {
        let mut labelled_arguments_given = false;
        let mut seen = std::collections::HashSet::new();

        if self.arity != args.len() {
            return Err(Error::IncorrectArity {
                meta: meta.clone(),
                expected: self.arity,
                given: args.len(),
            });
        }

        for i in 0..args.len() {
            let (label, meta) = match &args[i].label {
                // A labelled argument, we may need to reposition it in the array vector
                Some(l) => {
                    labelled_arguments_given = true;
                    (l, &args[i].meta)
                }

                // Not a labelled argument
                None => {
                    if labelled_arguments_given {
                        return Err(Error::PositionalArgumentAfterLabelled {
                            meta: args[i].meta.clone(),
                        });
                    }
                    continue;
                }
            };

            let position = match self.fields.get(label) {
                None => {
                    return Err(Error::UnknownLabel {
                        meta: meta.clone(),
                        labels: self.fields.clone(),
                        label: label.to_string(),
                    })
                }
                Some(p) => p,
            };

            if *position == i {
                continue;
            }

            if seen.contains(position) {
                return Err(Error::DuplicateArgument {
                    meta: meta.clone(),
                    label: label.to_string(),
                });
            }

            seen.insert(*position);
            args.swap(*position, i);
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueConstructorVariant {
    /// A locally defined variable or function parameter
    LocalVariable,

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
        arity: usize,
    },
}

impl ValueConstructorVariant {
    fn to_module_value_constructor(&self) -> ModuleValueConstructor {
        match self {
            ValueConstructorVariant::Record { name, .. } => {
                ModuleValueConstructor::Record { name: name.clone() }
            }

            ValueConstructorVariant::LocalVariable { .. }
            | ValueConstructorVariant::ModuleFn { .. } => ModuleValueConstructor::Fn,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ModuleValueConstructor {
    Record { name: String },
    Fn,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: Vec<String>,
    pub types: HashMap<String, TypeConstructor>,
    pub values: HashMap<String, ValueConstructor>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternConstructor {
    Record { name: String },
}

#[derive(Debug, Clone)]
pub struct Env<'a> {
    uid: usize,
    annotated_generic_types: im::HashSet<usize>,
    importable_modules: &'a HashMap<String, Module>,
    imported_modules: HashMap<String, Module>,

    // Values defined in the current function (or the prelude)
    local_values: im::HashMap<String, ValueConstructor>,

    // Types defined in the current module (or the prelude)
    module_types: HashMap<String, TypeConstructor>,

    // Values defined in the current module
    module_values: HashMap<String, ValueConstructor>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeVar {
    Unbound { id: usize, level: usize },
    Link { typ: Box<Type> },
    Generic { id: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeConstructor {
    pub public: bool,
    pub module: Vec<String>,
    pub arity: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueConstructor {
    pub public: bool,
    pub origin: Meta,
    pub variant: ValueConstructorVariant,
    pub typ: Type,
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

#[derive(Debug, Clone, Copy)]
pub enum NewTypeAction {
    Disallow,
    MakeGeneric,
}

impl<'a> Env<'a> {
    pub fn new(importable_modules: &'a HashMap<String, Module>) -> Self {
        let mut env = Self {
            uid: 0,
            annotated_generic_types: im::HashSet::new(),
            module_types: HashMap::new(),
            module_values: HashMap::new(),
            imported_modules: HashMap::new(),
            local_values: hashmap![],
            importable_modules,
        };

        env.insert_type_constructor(
            "Int".to_string(),
            TypeConstructor {
                arity: 0,
                module: vec![],
                public: true,
            },
        );

        env.insert_variable(
            "True".to_string(),
            ValueConstructorVariant::Record {
                name: "True".to_string(),
                field_map: None,
                arity: 0,
            },
            bool(),
        );
        env.insert_variable(
            "False".to_string(),
            ValueConstructorVariant::Record {
                name: "False".to_string(),
                field_map: None,
                arity: 0,
            },
            bool(),
        );
        env.insert_type_constructor(
            "Bool".to_string(),
            TypeConstructor {
                arity: 0,
                module: vec![],
                public: true,
            },
        );

        env.insert_type_constructor(
            "List".to_string(),
            TypeConstructor {
                arity: 1,
                module: vec![],
                public: true,
            },
        );

        env.insert_type_constructor(
            "Float".to_string(),
            TypeConstructor {
                arity: 0,
                module: vec![],
                public: true,
            },
        );

        env.insert_type_constructor(
            "String".to_string(),
            TypeConstructor {
                arity: 0,
                module: vec![],
                public: true,
            },
        );

        env.insert_type_constructor(
            "Result".to_string(),
            TypeConstructor {
                arity: 2,
                module: vec![],
                public: true,
            },
        );

        env.insert_variable(
            "Nil".to_string(),
            ValueConstructorVariant::Record {
                name: "Nil".to_string(),
                field_map: None,
                arity: 0,
            },
            Type::App {
                args: vec![],
                public: true,
                name: "Nil".to_string(),
                module: vec![],
            },
        );
        env.insert_type_constructor(
            "Nil".to_string(),
            TypeConstructor {
                arity: 0,
                module: vec![],
                public: true,
            },
        );

        env.insert_variable(
            "+".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(int()),
            },
        );

        env.insert_variable(
            "-".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(int()),
            },
        );

        env.insert_variable(
            "*".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(int()),
            },
        );

        env.insert_variable(
            "/".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(int()),
            },
        );

        env.insert_variable(
            "+.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(float()),
            },
        );

        env.insert_variable(
            "-.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(float()),
            },
        );

        env.insert_variable(
            "*.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(float()),
            },
        );

        env.insert_variable(
            "||".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![bool(), bool()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            "&&".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![bool(), bool()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            "%".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(int()),
            },
        );

        env.insert_variable(
            "%.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(float()),
            },
        );

        env.insert_variable(
            "/.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(float()),
            },
        );

        let a = env.new_generic_var();
        let b = env.new_generic_var();
        let f = Type::Fn {
            args: vec![a.clone()],
            retrn: Box::new(b.clone()),
        };
        env.insert_variable(
            "|>".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![a, f],
                retrn: Box::new(b),
            },
        );

        let a = env.new_generic_var();
        env.insert_variable(
            "==".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![a.clone(), a],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            ">".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            ">=".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            "<".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            "<=".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            ">.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            ">=.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            "<.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            "<=.".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(bool()),
            },
        );

        let a = env.new_generic_var();
        env.insert_variable(
            "!=".to_string(),
            ValueConstructorVariant::LocalVariable,
            Type::Fn {
                args: vec![a.clone(), a],
                retrn: Box::new(bool()),
            },
        );

        let result = |ok, error| Type::App {
            name: "Result".to_string(),
            module: vec![],
            public: true,
            args: vec![ok, error],
        };

        let ok = env.new_generic_var();
        let error = env.new_generic_var();
        env.insert_variable(
            "Ok".to_string(),
            ValueConstructorVariant::Record {
                name: "Ok".to_string(),
                field_map: None,
                arity: 1,
            },
            Type::Fn {
                args: vec![ok.clone()],
                retrn: Box::new(result(ok, error)),
            },
        );

        let ok = env.new_generic_var();
        let error = env.new_generic_var();
        env.insert_variable(
            "Error".to_string(),
            ValueConstructorVariant::Record {
                name: "Error".to_string(),
                field_map: None,
                arity: 1,
            },
            Type::Fn {
                args: vec![error.clone()],
                retrn: Box::new(result(ok, error)),
            },
        );

        env
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
    pub fn new_unbound_var(&mut self, level: usize) -> Type {
        Type::Var {
            typ: Rc::new(RefCell::new(TypeVar::Unbound {
                id: self.next_uid(),
                level,
            })),
        }
    }

    /// Create a new generic type that can stand in for any type.
    ///
    pub fn new_generic_var(&mut self) -> Type {
        Type::Var {
            typ: Rc::new(RefCell::new(TypeVar::Generic {
                id: self.next_uid(),
            })),
        }
    }

    /// Insert a variable in the current scope.
    ///
    pub fn insert_variable(&mut self, name: String, variant: ValueConstructorVariant, typ: Type) {
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

    /// Map a type in the current scope.
    ///
    pub fn insert_type_constructor(&mut self, name: String, info: TypeConstructor) {
        self.module_types.insert(name, info);
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
                        type_constructors: self.module_types.clone(),
                    })
            }

            Some(m) => {
                let module = &self.imported_modules.get(m).ok_or_else(|| {
                    GetTypeConstructorError::UnknownModule {
                        name: name.to_string(),
                        imported_modules: self.importable_modules.clone(),
                    }
                })?;
                module
                    .types
                    .get(name)
                    .ok_or_else(|| GetTypeConstructorError::UnknownModuleType {
                        name: name.to_string(),
                        module_name: module.name.clone(),
                        type_constructors: module.types.clone(),
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
                    variables: self.local_values.clone(),
                }
            }),

            Some(module) => {
                let module = self.imported_modules.get(&*module).ok_or_else(|| {
                    GetValueConstructorError::UnknownModule {
                        name: name.to_string(),
                        imported_modules: self.importable_modules.clone(),
                    }
                })?;
                module.values.get(&*name).ok_or_else(|| {
                    GetValueConstructorError::UnknownModuleValue {
                        name: name.to_string(),
                        module_name: module.name.clone(),
                        value_constructors: module.values.clone(),
                    }
                })
            }
        }
    }

    /// Construct a Type from an AST Type annotation.
    ///
    /// Type variables are managed using a HashMap of names to types- this permits the
    /// same type vars being shared between multiple annotations (such as in the arguments
    /// of an external function declaration)
    ///
    pub fn type_from_ast(
        &mut self,
        ast: &TypeAst,
        vars: &mut im::HashMap<String, (usize, Type)>,
        new: NewTypeAction,
    ) -> Result<Type, Error> {
        match ast {
            TypeAst::Constructor {
                meta,
                module,
                name,
                args,
            } => {
                let args = args
                    .iter()
                    .map(|t| self.type_from_ast(t, vars, new))
                    .collect::<Result<Vec<_>, _>>()?;
                let info = self
                    .get_type_constructor(module, name)
                    .map_err(|e| convert_get_type_constructor_error(e, &meta))?;
                if args.len() != info.arity {
                    return Err(Error::IncorrectTypeArity {
                        meta: meta.clone(),
                        name: name.to_string(),
                        expected: info.arity,
                        given: args.len(),
                    });
                }
                Ok(Type::App {
                    name: name.to_string(),
                    module: info.module.clone(),
                    public: info.public,
                    args,
                })
            }

            TypeAst::Tuple { elems, .. } => {
                let elems = elems
                    .iter()
                    .map(|t| self.type_from_ast(t, vars, new))
                    .collect::<Result<_, _>>()?;
                Ok(Type::Tuple { elems })
            }

            TypeAst::Fn { args, retrn, .. } => {
                let args = args
                    .iter()
                    .map(|t| self.type_from_ast(t, vars, new))
                    .collect::<Result<_, _>>()?;
                let retrn = self.type_from_ast(retrn, vars, new)?;
                Ok(Type::Fn {
                    args,
                    retrn: Box::new(retrn),
                })
            }

            TypeAst::Var { name, meta, .. } => match vars.get(name) {
                Some((_, var)) => Ok(var.clone()),

                None => match new {
                    NewTypeAction::MakeGeneric => {
                        let var = self.new_generic_var();
                        vars.insert(name.to_string(), (self.previous_uid(), var.clone()));
                        Ok(var)
                    }
                    NewTypeAction::Disallow => Err(Error::UnknownType {
                        name: name.to_string(),
                        meta: meta.clone(),
                        types: self.module_types.clone(),
                    }),
                },
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Error {
    UnknownLabel {
        meta: Meta,
        label: String,
        labels: HashMap<String, usize>,
    },

    UnknownVariable {
        meta: Meta,
        name: String,
        variables: im::HashMap<String, ValueConstructor>,
    },

    UnknownType {
        meta: Meta,
        name: String,
        types: HashMap<String, TypeConstructor>,
    },

    UnknownModule {
        meta: Meta,
        name: String,
        imported_modules: HashMap<String, Module>,
    },

    UnknownModuleType {
        meta: Meta,
        name: String,
        module_name: Vec<String>,
        type_constructors: HashMap<String, TypeConstructor>,
    },

    UnknownModuleValue {
        meta: Meta,
        name: String,
        module_name: Vec<String>,
        value_constructors: HashMap<String, ValueConstructor>,
    },

    UnknownModuleField {
        meta: Meta,
        name: String,
        module_name: Vec<String>,
        value_constructors: HashMap<String, ValueConstructor>,
        type_constructors: HashMap<String, TypeConstructor>,
    },

    NotFn {
        meta: Meta,
        typ: Type,
    },

    NotModule {
        meta: Meta,
        typ: Type,
    },

    IncorrectArity {
        meta: Meta,
        expected: usize,
        given: usize,
    },

    IncorrectTypeArity {
        meta: Meta,
        name: String,
        expected: usize,
        given: usize,
    },

    CouldNotUnify {
        meta: Meta,
        expected: Type,
        given: Type,
    },

    RecursiveType {
        meta: Meta,
    },

    DuplicateName {
        location: Meta,
        previous_location: Meta,
        name: String,
    },

    DuplicateArgument {
        meta: Meta,
        label: String,
    },

    DuplicateField {
        meta: Meta,
        label: String,
    },

    PrivateTypeLeak {
        meta: Meta,
        leaked: Type,
    },

    UnexpectedLabelledArg {
        meta: Meta,
        label: String,
    },

    PositionalArgumentAfterLabelled {
        meta: Meta,
    },

    IncorrectNumClausePatterns {
        meta: Meta,
        expected: usize,
        given: usize,
    },

    NonLocalClauseGuardVariable {
        meta: Meta,
        name: String,
    },

    ExtraVarInAlternativePattern {
        meta: Meta,
        name: String,
    },
}

#[derive(Debug, PartialEq)]
pub enum GetValueConstructorError {
    UnknownVariable {
        name: String,
        variables: im::HashMap<String, ValueConstructor>,
    },

    UnknownModule {
        name: String,
        imported_modules: HashMap<String, Module>,
    },

    UnknownModuleValue {
        name: String,
        module_name: Vec<String>,
        value_constructors: HashMap<String, ValueConstructor>,
    },
}

fn convert_get_value_constructor_error(e: GetValueConstructorError, meta: &Meta) -> Error {
    match e {
        GetValueConstructorError::UnknownVariable { name, variables } => Error::UnknownVariable {
            meta: meta.clone(),
            name,
            variables,
        },

        GetValueConstructorError::UnknownModule {
            name,
            imported_modules,
        } => Error::UnknownModule {
            meta: meta.clone(),
            name,
            imported_modules,
        },

        GetValueConstructorError::UnknownModuleValue {
            name,
            module_name,
            value_constructors,
        } => Error::UnknownModuleValue {
            meta: meta.clone(),
            name,
            module_name,
            value_constructors,
        },
    }
}

#[derive(Debug, PartialEq)]
pub enum GetTypeConstructorError {
    UnknownType {
        name: String,
        type_constructors: HashMap<String, TypeConstructor>,
    },

    UnknownModule {
        name: String,
        imported_modules: HashMap<String, Module>,
    },

    UnknownModuleType {
        name: String,
        module_name: Vec<String>,
        type_constructors: HashMap<String, TypeConstructor>,
    },
}

fn convert_get_type_constructor_error(e: GetTypeConstructorError, meta: &Meta) -> Error {
    match e {
        GetTypeConstructorError::UnknownType {
            name,
            type_constructors,
        } => Error::UnknownType {
            meta: meta.clone(),
            name,
            types: type_constructors,
        },

        GetTypeConstructorError::UnknownModule {
            name,
            imported_modules,
        } => Error::UnknownModule {
            meta: meta.clone(),
            name,
            imported_modules,
        },

        GetTypeConstructorError::UnknownModuleType {
            name,
            module_name,
            type_constructors,
        } => Error::UnknownModuleType {
            meta: meta.clone(),
            name,
            module_name,
            type_constructors,
        },
    }
}

/// Crawl the AST, annotating each node with the inferred type or
/// returning an error.
///
pub fn infer_module(
    module: UntypedModule,
    modules: &HashMap<String, Module>,
) -> Result<TypedModule, Error> {
    let mut env = Env::new(modules);
    let module_name = &module.name;

    // Register types so they can be used in constructors and functions
    // earlier in the file
    for s in module.statements.iter() {
        match s {
            Statement::ExternalType {
                name, public, args, ..
            }
            | Statement::CustomType {
                name, public, args, ..
            } => env.insert_type_constructor(
                name.clone(),
                TypeConstructor {
                    module: module_name.clone(),
                    public: *public,
                    arity: args.len(),
                },
            ),

            _ => {}
        }
    }

    let statements: Vec<Statement<_, _, _, Type>> = module
        .statements
        .into_iter()
        .map(|s| match s {
            Statement::Fn {
                meta,
                name,
                public,
                args,
                body,
                return_annotation,
            } => {
                let level = 1;

                let mut field_map = FieldMap::new(args.len());
                for (i, arg) in args.iter().enumerate() {
                    if let ArgNames::NamedLabelled { label, .. } = &arg.names {
                        field_map
                            .insert(label.clone(), i)
                            .map_err(|_| Error::DuplicateField {
                                label: label.to_string(),
                                meta: meta.clone(),
                            })?;
                    }
                }
                let field_map = field_map.into_option();

                // Register a var for the function so that it can call itself recursively
                let rec = env.new_unbound_var(level + 1);
                env.insert_variable(
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
                let (args_types, body) =
                    do_infer_fn(&args, body, &return_annotation, level + 1, &mut env)?;
                let typ = Type::Fn {
                    args: args_types,
                    retrn: Box::new(body.typ().clone()),
                };

                // Assert that the inferred type matches the type of any recursive call
                unify(&rec, &typ, &mut env).map_err(|e| convert_unify_error(e, &meta))?;
                let typ = generalise(typ, level);

                // Insert the function into the module's interface
                env.insert_module_value(
                    &name,
                    ValueConstructor {
                        public,
                        origin: meta.clone(),
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
                env.insert_variable(
                    name.clone(),
                    ValueConstructorVariant::ModuleFn {
                        name: name.clone(),
                        field_map,
                        module: module_name.clone(),
                        arity: args.len(),
                    },
                    typ,
                );

                Ok(Statement::Fn {
                    meta,
                    name,
                    public,
                    args,
                    body,
                    return_annotation,
                })
            }

            Statement::ExternalFn {
                meta,
                name,
                public,
                args,
                retrn,
                module,
                fun,
            } => {
                // Construct type of function from AST
                let mut type_vars = hashmap![];
                let retrn_type =
                    env.type_from_ast(&retrn, &mut type_vars, NewTypeAction::MakeGeneric)?;
                let mut args_types = Vec::with_capacity(args.len());
                let mut field_map = FieldMap::new(args.len());
                for (i, arg) in args.iter().enumerate() {
                    let t =
                        env.type_from_ast(&arg.typ, &mut type_vars, NewTypeAction::MakeGeneric)?;
                    args_types.push(t);
                    if let Some(label) = &arg.label {
                        field_map
                            .insert(label.clone(), i)
                            .map_err(|_| Error::DuplicateField {
                                label: label.to_string(),
                                meta: meta.clone(),
                            })?;
                    }
                }
                let field_map = field_map.into_option();
                let typ = Type::Fn {
                    args: args_types,
                    retrn: Box::new(retrn_type),
                };

                // Insert function into module
                env.insert_module_value(
                    &name,
                    ValueConstructor {
                        public,
                        typ: typ.clone(),
                        origin: meta.clone(),
                        variant: ValueConstructorVariant::ModuleFn {
                            name: name.clone(),
                            field_map: field_map.clone(),
                            module: module_name.clone(),
                            arity: args.len(),
                        },
                    },
                )?;

                // Insert function into module's internal scope
                env.insert_variable(
                    name.clone(),
                    ValueConstructorVariant::ModuleFn {
                        name: name.clone(),
                        module: module_name.clone(),
                        arity: args.len(),
                        field_map,
                    },
                    typ,
                );
                Ok(Statement::ExternalFn {
                    meta,
                    name,
                    public,
                    args,
                    retrn,
                    module,
                    fun,
                })
            }

            Statement::CustomType {
                meta,
                public,
                name,
                args,
                constructors,
            } => {
                // Build return type and collect type vars that can be used in constructors
                let mut type_vars = hashmap![];
                let args_types: Vec<_> = args
                    .iter()
                    .map(|arg| TypeAst::Var {
                        meta: meta.clone(),
                        name: arg.to_string(),
                    })
                    .map(|ast| env.type_from_ast(&ast, &mut type_vars, NewTypeAction::MakeGeneric))
                    .collect::<Result<_, _>>()?;

                let retrn = Type::App {
                    public,
                    module: module_name.clone(),
                    name: name.clone(),
                    args: args_types,
                };
                // Check and register constructors
                for constructor in constructors.iter() {
                    let mut field_map = FieldMap::new(constructor.args.len());
                    let mut args_types = Vec::with_capacity(constructor.args.len());
                    for (i, (label, arg)) in constructor.args.iter().enumerate() {
                        let t = env.type_from_ast(&arg, &mut type_vars, NewTypeAction::Disallow)?;
                        args_types.push(t);
                        if let Some(label) = label {
                            field_map.insert(label.clone(), i).map_err(|_| {
                                Error::DuplicateField {
                                    label: label.to_string(),
                                    meta: meta.clone(),
                                }
                            })?;
                        }
                    }
                    let field_map = field_map.into_option();
                    // Insert constructor function into module scope
                    let typ = match constructor.args.len() {
                        0 => retrn.clone(),
                        _ => Type::Fn {
                            args: args_types,
                            retrn: Box::new(retrn.clone()),
                        },
                    };
                    env.insert_module_value(
                        &constructor.name,
                        ValueConstructor {
                            public,
                            typ: typ.clone(),
                            origin: constructor.meta.clone(),
                            variant: ValueConstructorVariant::Record {
                                name: constructor.name.clone(),
                                arity: args.len(),
                                field_map: field_map.clone(),
                            },
                        },
                    )?;
                    env.insert_variable(
                        constructor.name.clone(),
                        ValueConstructorVariant::Record {
                            name: constructor.name.clone(),
                            arity: constructor.args.len(),
                            field_map,
                        },
                        typ,
                    );
                }
                Ok(Statement::CustomType {
                    meta,
                    public,
                    name,
                    args,
                    constructors,
                })
            }

            Statement::ExternalType {
                meta,
                public,
                name,
                args,
            } => {
                // Check contained types are valid
                let mut type_vars = hashmap![];
                for arg in args.iter() {
                    let var = TypeAst::Var {
                        meta: meta.clone(),
                        name: arg.to_string(),
                    };
                    env.type_from_ast(&var, &mut type_vars, NewTypeAction::MakeGeneric)?;
                }
                Ok(Statement::ExternalType {
                    meta,
                    public,
                    name,
                    args,
                })
            }

            Statement::Import {
                meta,
                module,
                as_name,
                unqualified,
            } => {
                // Find imported module
                let module_info = env.importable_modules.get(&module.join("/")).expect(
                    "COMPILER BUG: Typer could not find a module being imported.
This should not be possible. Please report this crash",
                );

                // Determine local alias of imported module
                let module_name = match &as_name {
                    None => module[module.len() - 1].clone(),
                    Some(name) => name.clone(),
                };

                // Insert unqualified imports into scope
                for UnqualifiedImport {
                    name,
                    meta,
                    as_name,
                } in &unqualified
                {
                    let mut imported = false;

                    let imported_name = match &as_name {
                        None => name,
                        Some(alias) => alias,
                    };

                    if let Some(value) = module_info.values.get(name) {
                        env.insert_variable(
                            imported_name.clone(),
                            value.variant.clone(),
                            value.typ.clone(),
                        );
                        imported = true;
                    }

                    if let Some(typ) = module_info.types.get(name) {
                        env.insert_type_constructor(imported_name.clone(), typ.clone());
                        imported = true;
                    }

                    if !imported {
                        return Err(Error::UnknownModuleField {
                            meta: meta.clone(),
                            name: name.clone(),
                            module_name: module,
                            value_constructors: module_info.values.clone(),
                            type_constructors: module_info.types.clone(),
                        });
                    }
                }

                // Insert imported module into scope
                env.imported_modules
                    .insert(module_name, module_info.clone());

                Ok(Statement::Import {
                    meta,
                    module,
                    as_name,
                    unqualified,
                })
            }
        })
        .collect::<Result<Vec<_>, _>>()?;

    // Remove private and imported types and values to create the public interface
    env.module_types
        .retain(|_, info| info.public && &info.module == module_name);
    env.module_values.retain(|_, info| info.public);

    // Ensure no exported values have private types in their type signature
    for (_, value) in env.module_values.iter() {
        if let Some(leaked) = value.typ.find_private_type() {
            return Err(Error::PrivateTypeLeak {
                meta: value.origin.clone(),
                leaked,
            });
        }
    }

    Ok(ast::Module {
        name: module.name.clone(),
        statements,
        type_info: Module {
            name: module.name,
            types: env.module_types,
            values: env.module_values,
        },
    })
}

/// Crawl the AST, annotating each node with the inferred type or
/// returning an error.
///
pub fn infer(expr: UntypedExpr, level: usize, env: &mut Env) -> Result<TypedExpr, Error> {
    match expr {
        Expr::Nil { meta, .. } => infer_nil(meta, level, env),
        Expr::Var { meta, name, .. } => infer_var(name, meta, level, env),
        Expr::Int { meta, value, .. } => infer_int(value, meta),
        Expr::Seq { first, then, .. } => infer_seq(*first, *then, level, env),
        Expr::Tuple { meta, elems, .. } => infer_tuple(elems, meta, level, env),
        Expr::Float { meta, value, .. } => infer_float(value, meta),
        Expr::String { meta, value, .. } => infer_string(value, meta),

        Expr::Fn {
            meta,
            is_capture,
            args,
            body,
            ..
        } => infer_fn(args, *body, is_capture, level, meta, env),

        Expr::Let {
            meta,
            pattern,
            value,
            then,
            ..
        } => infer_let(pattern, *value, *then, level, meta, env),

        Expr::Case {
            meta,
            subjects,
            clauses,
            ..
        } => infer_case(subjects, clauses, level, meta, env),

        Expr::Cons {
            meta, head, tail, ..
        } => infer_cons(*head, *tail, meta, level, env),

        Expr::Call {
            meta, fun, args, ..
        } => infer_call(*fun, args, level, meta, env),

        Expr::BinOp {
            meta,
            name,
            left,
            right,
            ..
        } => infer_binop(name, *left, *right, level, meta, env),

        Expr::FieldSelect {
            meta: select_meta,
            label,
            container,
            ..
        } => infer_field_select(*container, label, select_meta, level, env),

        // This node is not created by the parser, it is constructed by the typer from
        // the more general FieldSelect. Because of this it should never be present in AST
        // being inferred.
        Expr::ModuleSelect { .. } => {
            crate::error::fatal_compiler_bug("Expr::ModuleSelect erroneously passed to typer.")
        }
    }
}

fn infer_nil(meta: Meta, level: usize, env: &mut Env) -> Result<TypedExpr, Error> {
    Ok(Expr::Nil {
        meta,
        typ: list(env.new_unbound_var(level)),
    })
}

fn infer_string(value: String, meta: Meta) -> Result<TypedExpr, Error> {
    Ok(Expr::String {
        meta,
        value,
        typ: string(),
    })
}

fn infer_int(value: i64, meta: Meta) -> Result<TypedExpr, Error> {
    Ok(Expr::Int {
        meta,
        value,
        typ: int(),
    })
}

fn infer_float(value: f64, meta: Meta) -> Result<TypedExpr, Error> {
    Ok(Expr::Float {
        meta,
        value,
        typ: float(),
    })
}

fn infer_seq(
    first: UntypedExpr,
    then: UntypedExpr,
    level: usize,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let first = infer(first, level, env)?;
    let then = infer(then, level, env)?;
    Ok(Expr::Seq {
        typ: then.typ().clone(),
        first: Box::new(first),
        then: Box::new(then),
    })
}

fn infer_fn(
    args: Vec<Arg>,
    body: UntypedExpr,
    is_capture: bool,
    level: usize,
    meta: Meta,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let (args_types, body) = do_infer_fn(args.as_ref(), body, &None, level, env)?;
    let typ = Type::Fn {
        args: args_types,
        retrn: Box::new(body.typ().clone()),
    };
    Ok(Expr::Fn {
        meta,
        typ,
        is_capture,
        args,
        body: Box::new(body),
    })
}

fn infer_call(
    fun: UntypedExpr,
    args: Vec<CallArg<UntypedExpr>>,
    level: usize,
    meta: Meta,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let (fun, args, typ) = do_infer_call(fun, args, level, &meta, env)?;
    Ok(Expr::Call {
        meta,
        typ,
        args,
        fun: Box::new(fun),
    })
}

fn infer_cons(
    head: UntypedExpr,
    tail: UntypedExpr,
    meta: Meta,
    level: usize,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let head = infer(head, level, env)?;
    let tail = infer(tail, level, env)?;
    unify(tail.typ(), &list(head.typ().clone()), env).map_err(|e| convert_unify_error(e, &meta))?;
    Ok(Expr::Cons {
        meta,
        typ: tail.typ().clone(),
        head: Box::new(head),
        tail: Box::new(tail),
    })
}
fn infer_tuple(
    elems: Vec<UntypedExpr>,
    meta: Meta,
    level: usize,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let elems = elems
        .into_iter()
        .map(|e| infer(e, level, env))
        .collect::<Result<Vec<_>, _>>()?;
    let typ = Type::Tuple {
        elems: elems.iter().map(|e| e.typ().clone()).collect(),
    };
    Ok(Expr::Tuple { meta, elems, typ })
}
fn infer_var(name: String, meta: Meta, level: usize, env: &mut Env) -> Result<TypedExpr, Error> {
    let constructor = infer_value_constructor(&name, level, &meta, env)?;
    Ok(Expr::Var {
        constructor,
        meta,
        name,
    })
}
fn infer_field_select(
    container: UntypedExpr,
    label: String,
    select_meta: Meta,
    level: usize,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    match container {
        Expr::Var { name, meta, .. } if !env.local_values.contains_key(&name) => {
            infer_module_select(name.as_ref(), label, level, &meta, select_meta, env)
        }

        _ => infer_value_field_select(container, label, level, select_meta, env),
    }
}

fn infer_binop(
    name: BinOp,
    left: UntypedExpr,
    right: UntypedExpr,
    level: usize,
    meta: Meta,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let fun = Expr::Var {
        meta: meta.clone(),
        constructor: (),
        name: bin_op_name(&name),
    };
    let args = vec![
        CallArg {
            meta: Default::default(),
            label: None,
            value: left,
        },
        CallArg {
            meta: Default::default(),
            label: None,
            value: right,
        },
    ];
    let (_fun, mut args, typ) = do_infer_call(fun, args, level, &meta, env)?;
    Ok(Expr::BinOp {
        meta,
        name,
        typ,
        right: Box::new(args.pop().unwrap().value),
        left: Box::new(args.pop().unwrap().value),
    })
}

fn infer_let(
    pattern: UntypedPattern,
    value: UntypedExpr,
    then: UntypedExpr,
    level: usize,
    meta: Meta,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let value = infer(value, level + 1, env)?;
    let value_typ = generalise(value.typ().clone(), level + 1);
    let pattern = PatternTyper::new(env, level).unify(pattern, &value_typ)?;
    let then = infer(then, level, env)?;
    let typ = then.typ().clone();
    Ok(Expr::Let {
        meta,
        typ,
        pattern,
        value: Box::new(value),
        then: Box::new(then),
    })
}

fn infer_case(
    subjects: Vec<UntypedExpr>,
    clauses: Vec<UntypedClause>,
    level: usize,
    meta: Meta,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let subjects_count = subjects.len();
    let mut typed_subjects = Vec::with_capacity(subjects_count);
    let mut subject_types = Vec::with_capacity(subjects_count);
    let mut typed_clauses = Vec::with_capacity(clauses.len());

    let return_type = env.new_unbound_var(level); // TODO: should this be level + 1 ?

    for subject in subjects.into_iter() {
        let subject = infer(subject, level + 1, env)?;
        let subject_type = generalise(subject.typ().clone(), level + 1);
        typed_subjects.push(subject);
        subject_types.push(subject_type);
    }

    for clause in clauses.into_iter() {
        let typed_clause = infer_clause(clause, &subject_types, level, env)?;
        unify(&return_type, typed_clause.then.typ(), env)
            .map_err(|e| convert_unify_error(e, typed_clause.then.meta()))?;
        typed_clauses.push(typed_clause);
    }
    Ok(Expr::Case {
        meta,
        typ: return_type,
        subjects: typed_subjects,
        clauses: typed_clauses,
    })
}

fn infer_clause(
    clause: UntypedClause,
    subjects: &Vec<Type>,
    level: usize,
    env: &mut Env,
) -> Result<TypedClause, Error> {
    let Clause {
        pattern,
        alternative_patterns,
        guard,
        then,
        meta,
    } = clause;

    // Store the local scope so it can be reset after the clause
    let vars = env.local_values.clone();

    // Check the types
    let (typed_pattern, typed_alternatives) =
        infer_clause_pattern(pattern, alternative_patterns, subjects, level, &meta, env)?;
    let guard = infer_optional_clause_guard(guard, level, env)?;
    let then = infer(then, level, env)?;

    // Reset the local vars now the clause scope is done
    env.local_values = vars;

    Ok(Clause {
        meta: meta,
        pattern: typed_pattern,
        alternative_patterns: typed_alternatives,
        guard,
        then,
    })
}

fn infer_clause_pattern(
    pattern: UntypedMultiPattern,
    alternatives: Vec<UntypedMultiPattern>,
    subjects: &Vec<Type>,
    level: usize,
    meta: &Meta,
    env: &mut Env,
) -> Result<(TypedMultiPattern, Vec<TypedMultiPattern>), Error> {
    let mut pattern_typer = PatternTyper::new(env, level);
    let typed_pattern = pattern_typer.infer_multi_pattern(pattern, subjects, &meta)?;

    // Each case clause has one or more patterns that may match the
    // subject in order for the clause to be selected, so we must type
    // check every pattern.
    let mut typed_alternatives = Vec::with_capacity(alternatives.len());
    for m in alternatives {
        typed_alternatives.push(pattern_typer.infer_alternative_multi_pattern(m, subjects, &meta)?);
    }

    Ok((typed_pattern, typed_alternatives))
}

fn infer_optional_clause_guard(
    guard: Option<UntypedClauseGuard>,
    level: usize,
    env: &mut Env,
) -> Result<Option<TypedClauseGuard>, Error> {
    match guard {
        // If there is no guard we do nothing
        None => Ok(None),

        // If there is a guard we assert that it is of type Bool
        Some(guard) => {
            let guard = infer_clause_guard(guard, level, env)?;
            unify(&bool(), guard.typ(), env).map_err(|e| convert_unify_error(e, guard.meta()))?;
            Ok(Some(guard))
        }
    }
}

fn infer_clause_guard(
    guard: UntypedClauseGuard,
    level: usize,
    env: &mut Env,
) -> Result<TypedClauseGuard, Error> {
    match guard {
        ClauseGuard::Var { meta, name, .. } => {
            let constructor = infer_value_constructor(&name, level, &meta, env)?;

            // We cannot support all values in guard expressions as the BEAM does not
            match &constructor.variant {
                ValueConstructorVariant::LocalVariable => (),
                ValueConstructorVariant::ModuleFn { .. }
                | ValueConstructorVariant::Record { .. } => {
                    return Err(Error::NonLocalClauseGuardVariable { meta, name })
                }
            };

            Ok(ClauseGuard::Var {
                meta,
                name,
                typ: constructor.typ,
            })
        }

        ClauseGuard::And {
            meta, left, right, ..
        } => {
            let left = infer_clause_guard(*left, level, env)?;
            unify(&bool(), left.typ(), env).map_err(|e| convert_unify_error(e, left.meta()))?;
            let right = infer_clause_guard(*right, level, env)?;
            unify(&bool(), right.typ(), env).map_err(|e| convert_unify_error(e, right.meta()))?;
            Ok(ClauseGuard::And {
                meta,
                typ: bool(),
                left: Box::new(left),
                right: Box::new(right),
            })
        }

        ClauseGuard::Or {
            meta, left, right, ..
        } => {
            let left = infer_clause_guard(*left, level, env)?;
            unify(&bool(), left.typ(), env).map_err(|e| convert_unify_error(e, left.meta()))?;
            let right = infer_clause_guard(*right, level, env)?;
            unify(&bool(), right.typ(), env).map_err(|e| convert_unify_error(e, right.meta()))?;
            Ok(ClauseGuard::Or {
                meta,
                typ: bool(),
                left: Box::new(left),
                right: Box::new(right),
            })
        }

        ClauseGuard::Equals {
            meta, left, right, ..
        } => {
            let left = infer_clause_guard(*left, level, env)?;
            let right = infer_clause_guard(*right, level, env)?;
            unify(left.typ(), right.typ(), env).map_err(|e| convert_unify_error(e, &meta))?;
            Ok(ClauseGuard::Equals {
                meta,
                typ: bool(),
                left: Box::new(left),
                right: Box::new(right),
            })
        }

        ClauseGuard::NotEquals {
            meta, left, right, ..
        } => {
            let left = infer_clause_guard(*left, level, env)?;
            let right = infer_clause_guard(*right, level, env)?;
            unify(left.typ(), right.typ(), env).map_err(|e| convert_unify_error(e, &meta))?;
            Ok(ClauseGuard::NotEquals {
                meta,
                typ: bool(),
                left: Box::new(left),
                right: Box::new(right),
            })
        }
    }
}

fn infer_module_select(
    module_alias: &str,
    label: String,
    level: usize,
    module_meta: &Meta,
    select_meta: Meta,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let (module_name, constructor) = {
        let module_info =
            env.imported_modules
                .get(&*module_alias)
                .ok_or_else(|| Error::UnknownModule {
                    name: module_alias.to_string(),
                    meta: module_meta.clone(),
                    imported_modules: env.imported_modules.clone(),
                })?;

        let constructor =
            module_info
                .values
                .get(&label)
                .ok_or_else(|| Error::UnknownModuleValue {
                    name: label.clone(),
                    meta: select_meta.clone(),
                    module_name: module_info.name.clone(),
                    value_constructors: module_info.values.clone(),
                })?;

        (module_info.name.clone(), constructor.clone())
    };

    Ok(Expr::ModuleSelect {
        label,
        typ: instantiate(constructor.typ, level, &mut hashmap![], env),
        meta: select_meta,
        module_name,
        module_alias: module_alias.to_string(),
        constructor: constructor.variant.to_module_value_constructor(),
    })
}

fn infer_value_field_select(
    container: UntypedExpr,
    _label: String,
    level: usize,
    _meta: Meta,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    Err(Error::NotModule {
        meta: container.meta().clone(),
        typ: infer(container, level, env)?.typ().clone(),
    })
}

struct PatternTyper<'a, 'b> {
    env: &'a mut Env<'b>,
    level: usize,
    mode: PatternMode,
    initial_pattern_vars: HashSet<String>,
}

enum PatternMode {
    Initial,
    Alternative,
}

impl<'a, 'b> PatternTyper<'a, 'b> {
    pub fn new(env: &'a mut Env<'b>, level: usize) -> Self {
        Self {
            env,
            level,
            mode: PatternMode::Initial,
            initial_pattern_vars: HashSet::new(),
        }
    }

    fn insert_variable(&mut self, name: &str, typ: &Type) -> Result<(), UnifyError> {
        match self.mode {
            PatternMode::Initial => {
                self.initial_pattern_vars.insert(name.to_string());
                self.env.insert_variable(
                    name.to_string(),
                    ValueConstructorVariant::LocalVariable,
                    typ.clone(),
                );
                Ok(())
            }

            PatternMode::Alternative => match self.env.local_values.get(name) {
                // This variable was defined in the Initial multi-pattern
                Some(initial) if self.initial_pattern_vars.contains(name) => {
                    unify(&initial.typ, &typ, self.env)
                }

                // This variable was not defined in the Initial multi-pattern
                _ => Err(UnifyError::ExtraVarInAlternativePattern {
                    name: name.to_string(),
                }),
            },
        }
    }

    fn infer_alternative_multi_pattern(
        &mut self,
        multi_pattern: UntypedMultiPattern,
        subjects: &Vec<Type>,
        meta: &Meta,
    ) -> Result<Vec<TypedPattern>, Error> {
        self.mode = PatternMode::Alternative;
        let typed_multi = self.infer_multi_pattern(multi_pattern, subjects, meta)?;
        Ok(typed_multi)
    }

    fn infer_multi_pattern(
        &mut self,
        multi_pattern: UntypedMultiPattern,
        subjects: &Vec<Type>,
        meta: &Meta,
    ) -> Result<Vec<TypedPattern>, Error> {
        // If there are N subjects the multi-pattern is expected to be N patterns
        if subjects.len() != multi_pattern.len() {
            return Err(Error::IncorrectNumClausePatterns {
                meta: meta.clone(),
                expected: subjects.len(),
                given: multi_pattern.len(),
            });
        }

        // Unify each pattern in the multi-pattern with the corresponding subject
        let mut typed_multi = Vec::with_capacity(multi_pattern.len());
        for (pattern, subject_type) in multi_pattern.into_iter().zip(subjects.iter()) {
            let pattern = self.unify(pattern, &subject_type)?;
            typed_multi.push(pattern);
        }
        Ok(typed_multi)
    }

    /// When we have an assignment or a case expression we unify the pattern with the
    /// inferred type of the subject in order to determine what variables to insert
    /// into the environment (or to detect a type error).
    ///
    fn unify(&mut self, pattern: UntypedPattern, typ: &Type) -> Result<TypedPattern, Error> {
        match pattern {
            Pattern::Discard { meta } => Ok(Pattern::Discard { meta }),

            Pattern::Var { name, meta } => {
                self.insert_variable(name.as_ref(), typ)
                    .map_err(|e| convert_unify_error(e, &meta))?;
                Ok(Pattern::Var { name, meta })
            }

            Pattern::Let { name, pattern, .. } => {
                self.insert_variable(name.as_ref(), typ)
                    .map_err(|e| convert_unify_error(e, pattern.meta()))?;
                self.unify(*pattern, typ)
            }

            Pattern::Int { meta, value } => {
                unify(typ, &int(), self.env).map_err(|e| convert_unify_error(e, &meta))?;
                Ok(Pattern::Int { meta, value })
            }

            Pattern::Float { meta, value } => {
                unify(typ, &float(), self.env).map_err(|e| convert_unify_error(e, &meta))?;
                Ok(Pattern::Float { meta, value })
            }

            Pattern::String { meta, value } => {
                unify(typ, &string(), self.env).map_err(|e| convert_unify_error(e, &meta))?;
                Ok(Pattern::String { meta, value })
            }

            Pattern::Nil { meta } => {
                unify(typ, &list(self.env.new_unbound_var(self.level)), self.env)
                    .map_err(|e| convert_unify_error(e, &meta))?;
                Ok(Pattern::Nil { meta })
            }

            Pattern::Cons { meta, head, tail } => {
                match typ.get_app_args(true, &[], "List", 1, self.env) {
                    Some(args) => {
                        let head = Box::new(self.unify(*head, &args[0])?);
                        let tail = Box::new(self.unify(*tail, typ)?);
                        Ok(Pattern::Cons { meta, head, tail })
                    }

                    None => Err(Error::CouldNotUnify {
                        given: list(self.env.new_unbound_var(self.level)),
                        expected: typ.clone(),
                        meta,
                    }),
                }
            }

            Pattern::Tuple { elems, meta } => match typ.clone().collapse_links() {
                Type::Tuple { elems: type_elems } => {
                    let elems = elems
                        .into_iter()
                        .zip(type_elems)
                        .map(|(pattern, typ)| self.unify(pattern, &typ))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(Pattern::Tuple { elems, meta })
                }

                typ @ Type::Var { .. } => {
                    let elems_types = (0..(elems.len()))
                        .map(|_| self.env.new_unbound_var(self.level))
                        .collect();
                    unify(&Type::Tuple { elems: elems_types }, &typ, self.env)
                        .map_err(|e| convert_unify_error(e, &meta))?;
                    self.unify(Pattern::Tuple { elems, meta }, &typ)
                }

                other => {
                    dbg!(&other);
                    unimplemented!();
                }
            },

            Pattern::Constructor {
                meta,
                module,
                name,
                args: mut pattern_args,
                ..
            } => {
                let cons = self
                    .env
                    .get_value_constructor(module.as_ref(), &name)
                    .map_err(|e| convert_get_value_constructor_error(e, &meta))?;

                match cons.field_map() {
                    // The fun has a field map so labelled arguments may be present and need to be reordered.
                    Some(field_map) => field_map.reorder(&mut pattern_args, &meta)?,

                    // The fun has no field map and so we error if arguments have been labelled
                    None => assert_no_labelled_arguments(&pattern_args)?,
                }

                let constructor_typ = cons.typ.clone();
                let constructor = match cons.variant {
                    ValueConstructorVariant::Record { ref name, .. } => {
                        PatternConstructor::Record { name: name.clone() }
                    }
                    ValueConstructorVariant::LocalVariable
                    | ValueConstructorVariant::ModuleFn { .. } => crate::error::fatal_compiler_bug(
                        "Unexpected value constructor type for a constructor pattern.",
                    ),
                };

                match instantiate(constructor_typ, self.level, &mut hashmap![], self.env) {
                    Type::Fn { args, retrn } => {
                        if args.len() == pattern_args.len() {
                            let pattern_args = pattern_args
                                .into_iter()
                                .zip(args)
                                .map(|(arg, typ)| {
                                    let CallArg { value, meta, label } = arg;
                                    let value = self.unify(value, &typ)?;
                                    Ok(CallArg { value, meta, label })
                                })
                                .collect::<Result<Vec<_>, _>>()?;
                            unify(&typ, &retrn, self.env)
                                .map_err(|e| convert_unify_error(e, &meta))?;
                            Ok(Pattern::Constructor {
                                meta,
                                module,
                                name,
                                args: pattern_args,
                                constructor,
                            })
                        } else {
                            Err(Error::IncorrectArity {
                                meta,
                                expected: args.len(),
                                given: pattern_args.len(),
                            })
                        }
                    }

                    c @ Type::App { .. } => {
                        if pattern_args.is_empty() {
                            unify(&typ, &c, self.env).map_err(|e| convert_unify_error(e, &meta))?;
                            Ok(Pattern::Constructor {
                                meta,
                                module,
                                name,
                                args: vec![],
                                constructor,
                            })
                        } else {
                            Err(Error::IncorrectArity {
                                meta,
                                expected: 0,
                                given: pattern_args.len(),
                            })
                        }
                    }

                    typ => {
                        dbg!(typ);
                        unimplemented!();
                    }
                }
            }
        }
    }
}

fn infer_value_constructor(
    name: &str,
    level: usize,
    meta: &Meta,
    env: &mut Env,
) -> Result<ValueConstructor, Error> {
    let ValueConstructor {
        public,
        variant,
        origin,
        typ,
    } = env
        .get_variable(name)
        .cloned()
        .ok_or_else(|| Error::UnknownVariable {
            meta: meta.clone(),
            name: name.to_string(),
            variables: env.local_values.clone(),
        })?;
    let typ = instantiate(typ, level, &mut hashmap![], env);
    Ok(ValueConstructor {
        public,
        variant,
        origin,
        typ,
    })
}

fn do_infer_call(
    fun: UntypedExpr,
    mut args: Vec<CallArg<UntypedExpr>>,
    level: usize,
    meta: &Meta,
    env: &mut Env,
) -> Result<(TypedExpr, Vec<CallArg<TypedExpr>>, Type), Error> {
    let fun = infer(fun, level, env)?;

    match get_field_map(&fun, env).map_err(|e| convert_get_value_constructor_error(e, meta))? {
        // The fun has a field map so labelled arguments may be present and need to be reordered.
        Some(field_map) => field_map.reorder(&mut args, meta)?,

        // The fun has no field map and so we error if arguments have been labelled
        None => assert_no_labelled_arguments(&args)?,
    }

    let (mut args_types, return_type) = match_fun_type(fun.typ(), args.len(), env)
        .map_err(|e| convert_not_fun_error(e, fun.meta(), &meta))?;
    let args = args_types
        .iter_mut()
        .zip(args)
        .map(|(typ, CallArg { label, value, meta }): (&mut Type, _)| {
            let value = infer(value, level, env)?;
            unify(typ, value.typ(), env).map_err(|e| convert_unify_error(e, value.meta()))?;
            Ok(CallArg { label, value, meta })
        })
        .collect::<Result<_, _>>()?;
    Ok((fun, args, return_type))
}

fn assert_no_labelled_arguments<A>(args: &[CallArg<A>]) -> Result<(), Error> {
    for arg in args {
        if let Some(label) = &arg.label {
            return Err(Error::UnexpectedLabelledArg {
                meta: arg.meta.clone(),
                label: label.to_string(),
            });
        }
    }
    Ok(())
}

fn get_field_map<'a>(
    constructor: &TypedExpr,
    env: &'a Env,
) -> Result<Option<&'a FieldMap>, GetValueConstructorError> {
    let (module, name) = match constructor {
        Expr::ModuleSelect {
            module_alias,
            label,
            ..
        } => (Some(module_alias), label),

        Expr::Var { name, .. } => (None, name),

        _ => return Ok(None),
    };

    Ok(env.get_value_constructor(module, name)?.field_map())
}

fn do_infer_fn(
    args: &[Arg],
    body: UntypedExpr,
    return_annotation: &Option<TypeAst>,
    level: usize,
    env: &mut Env,
) -> Result<(Vec<Type>, TypedExpr), Error> {
    // Construct an initial type for each argument of the function- either an unbound type variable
    // or a type provided by an annotation.
    let mut type_vars = hashmap![];
    let args_types: Vec<_> = args
        .iter()
        .map(|arg| {
            arg.annotation
                .clone()
                .map(|t| env.type_from_ast(&t, &mut type_vars, NewTypeAction::MakeGeneric))
                .unwrap_or_else(|| Ok(env.new_unbound_var(level)))
        })
        .collect::<Result<_, _>>()?;

    // Record generic type variables that comes from type annotations.
    // They cannot be instantiated so we need to keep track of them.
    let previous_annotated_generic_types = env.annotated_generic_types.clone();
    for (id, _type) in type_vars.values() {
        env.annotated_generic_types.insert(*id);
    }

    // Insert arguments into function body scope.
    let previous_vars = env.local_values.clone();
    for (arg, t) in args.iter().zip(args_types.iter()) {
        match &arg.names {
            ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => env.insert_variable(
                name.to_string(),
                ValueConstructorVariant::LocalVariable,
                (*t).clone(),
            ),
            ArgNames::Discard | ArgNames::LabelledDiscard { .. } => (),
        };
    }

    let body = infer(body, level, env)?;

    // Check that any return type annotation is accurate.
    if let Some(ann) = return_annotation {
        let ret_typ = env.type_from_ast(ann, &mut type_vars, NewTypeAction::MakeGeneric)?;
        unify(&ret_typ, body.typ(), env).map_err(|e| convert_unify_error(e, body.meta()))?;
    }

    // Reset the env now that the scope of the function has ended.
    env.local_values = previous_vars;
    env.annotated_generic_types = previous_annotated_generic_types;
    Ok((args_types, body))
}

fn bin_op_name(name: &BinOp) -> String {
    match name {
        BinOp::Pipe => "|>".to_string(),
        BinOp::And => "&&".to_string(),
        BinOp::Or => "||".to_string(),
        BinOp::LtInt => "<".to_string(),
        BinOp::LtEqInt => "<=".to_string(),
        BinOp::LtFloat => "<.".to_string(),
        BinOp::LtEqFloat => "<=.".to_string(),
        BinOp::Eq => "==".to_string(),
        BinOp::NotEq => "!=".to_string(),
        BinOp::GtEqInt => ">=".to_string(),
        BinOp::GtInt => ">".to_string(),
        BinOp::GtEqFloat => ">=.".to_string(),
        BinOp::GtFloat => ">.".to_string(),
        BinOp::AddInt => "+".to_string(),
        BinOp::AddFloat => "+.".to_string(),
        BinOp::SubInt => "-".to_string(),
        BinOp::SubFloat => "-.".to_string(),
        BinOp::MultInt => "*".to_string(),
        BinOp::MultFloat => "*.".to_string(),
        BinOp::DivInt => "/".to_string(),
        BinOp::DivFloat => "/.".to_string(),
        BinOp::ModuloInt => "%".to_string(),
    }
}

fn convert_unify_error(e: UnifyError, meta: &Meta) -> Error {
    match e {
        UnifyError::CouldNotUnify { expected, given } => Error::CouldNotUnify {
            meta: meta.clone(),
            expected,
            given,
        },

        UnifyError::ExtraVarInAlternativePattern { name } => Error::ExtraVarInAlternativePattern {
            meta: meta.clone(),
            name,
        },

        UnifyError::RecursiveType => Error::RecursiveType { meta: meta.clone() },
    }
}

/// Instantiate converts generic variables into unbound ones.
///
fn instantiate(
    t: Type,
    ctx_level: usize,
    ids: &mut im::HashMap<usize, Type>,
    env: &mut Env,
) -> Type {
    match t {
        Type::App {
            public,
            name,
            module,
            args,
        } => {
            let args = args
                .into_iter()
                .map(|t| instantiate(t, ctx_level, ids, env))
                .collect();
            Type::App {
                public,
                name,
                module,
                args,
            }
        }

        Type::Var { typ } => {
            match &*typ.borrow() {
                TypeVar::Link { typ } => return instantiate(*typ.clone(), ctx_level, ids, env),

                TypeVar::Unbound { .. } => return Type::Var { typ: typ.clone() },

                TypeVar::Generic { id } => match ids.get(id) {
                    Some(t) => return t.clone(),
                    None => {
                        if !env.annotated_generic_types.contains(id) {
                            let v = env.new_unbound_var(ctx_level);
                            ids.insert(*id, v.clone());
                            return v;
                        }
                    }
                },
            }
            Type::Var { typ }
        }

        Type::Fn { args, retrn, .. } => {
            let args = args
                .into_iter()
                .map(|t| instantiate(t, ctx_level, ids, env))
                .collect();
            let retrn = Box::new(instantiate(*retrn, ctx_level, ids, env));
            Type::Fn { args, retrn }
        }

        Type::Tuple { elems } => Type::Tuple {
            elems: elems
                .into_iter()
                .map(|t| instantiate(t, ctx_level, ids, env))
                .collect(),
        },
    }
}

#[derive(Debug, PartialEq)]
enum UnifyError {
    CouldNotUnify { expected: Type, given: Type },

    ExtraVarInAlternativePattern { name: String },

    RecursiveType,
}

fn unify_enclosed_type(
    e1: &Type,
    e2: &Type,
    result: Result<(), UnifyError>,
) -> Result<(), UnifyError> {
    // If types cannot unify, show the type error with the enclosing types, e1 and e2.
    match result {
        Err(UnifyError::CouldNotUnify { .. }) => Err(UnifyError::CouldNotUnify {
            expected: (*e1).clone(),
            given: (*e2).clone(),
        }),

        _ => result,
    }
}
fn unify(t1: &Type, t2: &Type, env: &Env) -> Result<(), UnifyError> {
    if t1 == t2 {
        return Ok(());
    }

    // Collapse right hand side type links. Left hand side will be collapsed in the next block.
    if let Type::Var { typ } = t2 {
        if let TypeVar::Link { typ } = &*typ.borrow() {
            return unify(t1, typ, env);
        }
    }

    if let Type::Var { typ } = t1 {
        enum Action {
            Unify(Type),
            CouldNotUnify,
            Link,
        }

        let action = match &*typ.borrow() {
            TypeVar::Link { typ } => Action::Unify((**typ).clone()),

            TypeVar::Unbound { id, level } => {
                update_levels(t2, *level, *id)?;
                Action::Link
            }

            TypeVar::Generic { id } => {
                if let Type::Var { typ } = t2 {
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
                *typ.borrow_mut() = TypeVar::Link {
                    typ: Box::new((*t2).clone()),
                };
                Ok(())
            }

            Action::Unify(t) => unify(&t, t2, env),

            Action::CouldNotUnify => Err(UnifyError::CouldNotUnify {
                expected: (*t1).clone(),
                given: (*t2).clone(),
            }),
        };
    }

    if let Type::Var { .. } = t2 {
        return unify(t2, t1, env).map_err(flip_unify_error);
    }

    match (t1, t2) {
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
                unify_enclosed_type(t1, t2, unify(a, b, env))?;
            }
            Ok(())
        }

        (Type::Tuple { elems: elems1, .. }, Type::Tuple { elems: elems2, .. })
            if elems1.len() == elems2.len() =>
        {
            for (a, b) in elems1.iter().zip(elems2) {
                unify_enclosed_type(t1, t2, unify(a, b, env))?;
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
                unify(a, b, env)?;
            }
            unify(retrn1, retrn2, env)
        }

        (_, _) => Err(UnifyError::CouldNotUnify {
            expected: (*t1).clone(),
            given: (*t2).clone(),
        }),
    }
}

fn flip_unify_error(e: UnifyError) -> UnifyError {
    match e {
        UnifyError::CouldNotUnify { expected, given } => UnifyError::CouldNotUnify {
            expected: given,
            given: expected,
        },
        other => other,
    }
}

/// This function makes sure that the type variable being unified
/// doesn't occur within the type it is being unified with. This
/// prevents the algorithm from inferring recursive types, which
/// could cause naively-implemented type checking to diverge.
/// While traversing the type tree, this function also takes care
/// of updating the levels of the type variables appearing within
/// the type, thus ensuring the type will be correctly generalized.
///
fn update_levels(typ: &Type, own_level: usize, own_id: usize) -> Result<(), UnifyError> {
    if let Type::Var { typ } = &typ {
        let new_value = match &*typ.borrow() {
            TypeVar::Link { typ, .. } => return update_levels(typ, own_level, own_id),

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

    match typ {
        Type::App { args, .. } => {
            for arg in args.iter() {
                update_levels(arg, own_level, own_id)?
            }
            Ok(())
        }

        Type::Fn { args, retrn } => {
            for arg in args.iter() {
                update_levels(arg, own_level, own_id)?;
            }
            update_levels(retrn, own_level, own_id)
        }

        Type::Tuple { elems, .. } => {
            for elem in elems.iter() {
                update_levels(elem, own_level, own_id)?
            }
            Ok(())
        }

        Type::Var { .. } => unreachable!(),
    }
}

fn match_fun_type(
    typ: &Type,
    arity: usize,
    env: &mut Env,
) -> Result<(Vec<Type>, Type), MatchFunTypeError> {
    if let Type::Var { typ } = &typ {
        let new_value = match &*typ.borrow() {
            TypeVar::Link { typ, .. } => return match_fun_type(typ, arity, env),

            TypeVar::Unbound { level, .. } => {
                let args: Vec<_> = (0..arity).map(|_| env.new_unbound_var(*level)).collect();
                let retrn = env.new_unbound_var(*level);
                Some((args, retrn))
            }

            TypeVar::Generic { .. } => None,
        };

        if let Some((args, retrn)) = new_value {
            *typ.borrow_mut() = TypeVar::Link {
                typ: Box::new(Type::Fn {
                    args: args.clone(),
                    retrn: Box::new(retrn.clone()),
                }),
            };
            return Ok((args, retrn));
        }
    }

    if let Type::Fn { args, retrn } = typ {
        return if args.len() != arity {
            Err(MatchFunTypeError::IncorrectArity {
                expected: args.len(),
                given: arity,
            })
        } else {
            Ok((args.clone(), (**retrn).clone()))
        };
    }

    Err(MatchFunTypeError::NotFn { typ: typ.clone() })
}

enum MatchFunTypeError {
    IncorrectArity { expected: usize, given: usize },
    NotFn { typ: Type },
}

fn convert_not_fun_error(e: MatchFunTypeError, fn_meta: &Meta, call_meta: &Meta) -> Error {
    match e {
        MatchFunTypeError::IncorrectArity { expected, given } => Error::IncorrectArity {
            meta: call_meta.clone(),
            expected,
            given,
        },

        MatchFunTypeError::NotFn { typ } => Error::NotFn {
            meta: fn_meta.clone(),
            typ,
        },
    }
}

/// Takes a level and a type and turns all type variables within the type that have
/// level higher than the input level into generalized (polymorphic) type variables.
///
fn generalise(t: Type, ctx_level: usize) -> Type {
    match t {
        Type::Var { typ } => {
            let new_var = match &*typ.borrow() {
                TypeVar::Unbound { id, level } => {
                    let id = *id;
                    if *level > ctx_level {
                        return Type::Var {
                            typ: Rc::new(RefCell::new(TypeVar::Generic { id })),
                        };
                    } else {
                        Some(TypeVar::Unbound { id, level: *level })
                    }
                }

                TypeVar::Link { typ } => return generalise((**typ).clone(), ctx_level),

                TypeVar::Generic { .. } => None,
            };

            if let Some(v) = new_var {
                *typ.borrow_mut() = v;
            }
            Type::Var { typ }
        }

        Type::App {
            public,
            module,
            name,
            args,
        } => {
            let args = args.into_iter().map(|t| generalise(t, ctx_level)).collect();
            Type::App {
                public,
                module,
                name,
                args,
            }
        }

        Type::Fn { args, retrn } => {
            let args = args.into_iter().map(|t| generalise(t, ctx_level)).collect();
            let retrn = generalise(*retrn, ctx_level);
            Type::Fn {
                args,
                retrn: Box::new(retrn),
            }
        }

        Type::Tuple { elems } => Type::Tuple {
            elems: elems
                .into_iter()
                .map(|t| generalise(t, ctx_level))
                .collect(),
        },
    }
}

pub fn int() -> Type {
    Type::App {
        public: true,
        name: "Int".to_string(),
        module: vec![],
        args: vec![],
    }
}

pub fn float() -> Type {
    Type::App {
        args: vec![],
        public: true,
        name: "Float".to_string(),
        module: vec![],
    }
}

pub fn bool() -> Type {
    Type::App {
        args: vec![],
        public: true,
        name: "Bool".to_string(),
        module: vec![],
    }
}

pub fn string() -> Type {
    Type::App {
        args: vec![],
        public: true,
        name: "String".to_string(),
        module: vec![],
    }
}

pub fn list(t: Type) -> Type {
    Type::App {
        public: true,
        name: "List".to_string(),
        module: vec![],
        args: vec![t],
    }
}
