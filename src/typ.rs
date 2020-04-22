pub mod pretty;
#[cfg(test)]
mod tests;

use crate::ast::{
    self, Arg, ArgNames, BinOp, CallArg, Clause, ClauseGuard, Pattern, RecordConstructor, SrcSpan,
    Statement, TypeAst, TypedArg, TypedClause, TypedClauseGuard, TypedExpr, TypedModule,
    TypedMultiPattern, TypedPattern, TypedStatement, UnqualifiedImport, UntypedArg, UntypedClause,
    UntypedClauseGuard, UntypedExpr, UntypedModule, UntypedMultiPattern, UntypedPattern,
    UntypedStatement,
};
use crate::error::GleamExpect;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

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
    pub fn is_unbound(&self) -> bool {
        match self {
            Type::Var { typ } => typ.borrow().is_unbound(),
            _ => false,
        }
    }

    pub fn app_parameters(&self) -> Option<&[Arc<Type>]> {
        match self {
            Type::App { args, .. } => Some(args.as_slice()),
            _ => None,
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
        env: &mut Env,
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
                        return typ.get_app_args(public, module, name, arity, env);
                    }

                    TypeVar::Unbound { level, .. } => {
                        (0..arity).map(|_| env.new_unbound_var(*level)).collect()
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
    fn reorder<A>(&self, args: &mut Vec<CallArg<A>>, location: &SrcSpan) -> Result<(), Error> {
        let mut labelled_arguments_given = false;
        let mut seen = std::collections::HashSet::new();

        if self.arity != args.len() {
            return Err(Error::IncorrectArity {
                location: location.clone(),
                expected: self.arity,
                given: args.len(),
            });
        }

        for i in 0..args.len() {
            let (label, location) = match &args[i].label {
                // A labelled argument, we may need to reposition it in the array vector
                Some(l) => {
                    labelled_arguments_given = true;
                    (l, &args[i].location)
                }

                // Not a labelled argument
                None => {
                    if labelled_arguments_given {
                        return Err(Error::PositionalArgumentAfterLabelled {
                            location: args[i].location.clone(),
                        });
                    }
                    continue;
                }
            };

            let position = match self.fields.get(label) {
                None => {
                    return Err(Error::UnknownLabel {
                        location: location.clone(),
                        labels: self.fields.keys().map(|t| t.to_string()).collect(),
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
                    location: location.clone(),
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
    pub accessors: HashMap<String, AccessorsMap>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PatternConstructor {
    Record { name: String },
}

#[derive(Debug, Clone)]
pub struct Env<'a, 'b> {
    current_module: &'b [String],
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

    // Accessors defined in the current module
    accessors: HashMap<String, AccessorsMap>,
}

impl<'a, 'b> Env<'a, 'b> {
    pub fn new(
        current_module: &'b [String],
        importable_modules: &'a HashMap<String, Module>,
    ) -> Self {
        let mut env = Self {
            uid: 0,
            annotated_generic_types: im::HashSet::new(),
            module_types: HashMap::new(),
            module_values: HashMap::new(),
            imported_modules: HashMap::new(),
            accessors: HashMap::new(),
            local_values: hashmap![],
            importable_modules,
            current_module,
        };

        env.insert_type_constructor(
            "Int".to_string(),
            TypeConstructor {
                parameters: vec![],
                typ: int(),
                origin: Default::default(),
                module: vec![],
                public: true,
            },
        )
        .gleam_expect("prelude inserting Int type");

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
                origin: Default::default(),
                parameters: vec![],
                typ: bool(),
                module: vec![],
                public: true,
            },
        )
        .gleam_expect("prelude inserting Bool type");

        let list_parameter = env.new_generic_var();
        env.insert_type_constructor(
            "List".to_string(),
            TypeConstructor {
                origin: Default::default(),
                parameters: vec![list_parameter.clone()],
                typ: list(list_parameter),
                module: vec![],
                public: true,
            },
        )
        .gleam_expect("prelude inserting List type");

        env.insert_type_constructor(
            "Float".to_string(),
            TypeConstructor {
                origin: Default::default(),
                parameters: vec![],
                typ: float(),
                module: vec![],
                public: true,
            },
        )
        .gleam_expect("prelude inserting Float type");

        env.insert_type_constructor(
            "String".to_string(),
            TypeConstructor {
                origin: Default::default(),
                parameters: vec![],
                typ: string(),
                module: vec![],
                public: true,
            },
        )
        .gleam_expect("prelude inserting String type");

        let result_value = env.new_generic_var();
        let result_error = env.new_generic_var();
        env.insert_type_constructor(
            "Result".to_string(),
            TypeConstructor {
                origin: Default::default(),
                parameters: vec![result_value.clone(), result_error.clone()],
                typ: result(result_value, result_error),
                module: vec![],
                public: true,
            },
        )
        .gleam_expect("prelude inserting Result type");

        env.insert_variable(
            "Nil".to_string(),
            ValueConstructorVariant::Record {
                name: "Nil".to_string(),
                field_map: None,
                arity: 0,
            },
            nil(),
        );
        env.insert_type_constructor(
            "Nil".to_string(),
            TypeConstructor {
                origin: Default::default(),
                parameters: vec![],
                typ: nil(),
                module: vec![],
                public: true,
            },
        )
        .gleam_expect("prelude inserting Nil type");

        let ok = env.new_generic_var();
        let error = env.new_generic_var();
        env.insert_variable(
            "Ok".to_string(),
            ValueConstructorVariant::Record {
                name: "Ok".to_string(),
                field_map: None,
                arity: 1,
            },
            fn_(vec![ok.clone()], result(ok, error)),
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
            fn_(vec![error.clone()], result(ok, error)),
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
                    .types
                    .get(name)
                    .ok_or_else(|| GetTypeConstructorError::UnknownModuleType {
                        name: name.to_string(),
                        module_name: module.name.clone(),
                        type_constructors: module.types.keys().map(|t| t.to_string()).collect(),
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
                module.values.get(&*name).ok_or_else(|| {
                    GetValueConstructorError::UnknownModuleValue {
                        name: name.to_string(),
                        module_name: module.name.clone(),
                        value_constructors: module.values.keys().map(|t| t.to_string()).collect(),
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
        vars: &mut im::HashMap<String, (usize, Arc<Type>)>,
        new: NewTypeAction,
    ) -> Result<Arc<Type>, Error> {
        match ast {
            TypeAst::Constructor {
                location,
                module,
                name,
                args,
            } => {
                // Hydrate the type argument AST into types
                let mut argument_types = Vec::with_capacity(args.len());
                for t in args {
                    let typ = self.type_from_ast(t, vars, new)?;
                    argument_types.push((t.location(), typ));
                }

                // Look up the constructor
                let TypeConstructor {
                    parameters,
                    typ: return_type,
                    ..
                } = self
                    .get_type_constructor(module, name)
                    .map_err(|e| convert_get_type_constructor_error(e, &location))?
                    .clone();

                // Ensure that the correct number of arguments have been given to the constructor
                if args.len() != parameters.len() {
                    return Err(Error::IncorrectTypeArity {
                        location: location.clone(),
                        name: name.to_string(),
                        expected: parameters.len(),
                        given: args.len(),
                    });
                }

                // Instantiate the constructor type for this specific usage
                let mut type_vars = hashmap![];
                let mut parameter_types = Vec::with_capacity(parameters.len());
                for typ in parameters {
                    parameter_types.push(instantiate(typ, 0, &mut type_vars, self));
                }
                let return_type = instantiate(return_type, 0, &mut type_vars, self);

                // Unify argument types with instantiated parameter types so that the correct types
                // are inserted into the return type
                for (parameter, (location, argument)) in
                    parameter_types.iter().zip(argument_types.iter())
                {
                    unify(parameter.clone(), argument.clone(), self)
                        .map_err(|e| convert_unify_error(e, &location))?;
                }

                Ok(return_type)
            }

            TypeAst::Tuple { elems, .. } => Ok(tuple(
                elems
                    .iter()
                    .map(|t| self.type_from_ast(t, vars, new))
                    .collect::<Result<_, _>>()?,
            )),

            TypeAst::Fn { args, retrn, .. } => {
                let args = args
                    .iter()
                    .map(|t| self.type_from_ast(t, vars, new))
                    .collect::<Result<_, _>>()?;
                let retrn = self.type_from_ast(retrn, vars, new)?;
                Ok(fn_(args, retrn))
            }

            TypeAst::Var { name, location, .. } => match vars.get(name) {
                Some((_, var)) => Ok(var.clone()),

                None => match new {
                    NewTypeAction::MakeGeneric => {
                        let var = self.new_generic_var();
                        vars.insert(name.to_string(), (self.previous_uid(), var.clone()));
                        Ok(var)
                    }
                    NewTypeAction::Disallow => Err(Error::UnknownType {
                        name: name.to_string(),
                        location: location.clone(),
                        types: self.module_types.keys().map(|t| t.to_string()).collect(),
                    }),
                },
            },
        }
    }

    pub fn insert_accessors(&mut self, type_name: &str, accessors: AccessorsMap) {
        self.accessors.insert(type_name.to_string(), accessors);
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

#[derive(Debug, Clone, Copy)]
pub enum NewTypeAction {
    Disallow,
    MakeGeneric,
}

#[derive(Debug, PartialEq)]
pub enum Error {
    UnknownLabel {
        location: SrcSpan,
        label: String,
        labels: Vec<String>,
    },

    UnknownVariable {
        location: SrcSpan,
        name: String,
        variables: Vec<String>,
    },

    UnknownType {
        location: SrcSpan,
        name: String,
        types: Vec<String>,
    },

    UnknownModule {
        location: SrcSpan,
        name: String,
        imported_modules: Vec<String>,
    },

    UnknownModuleType {
        location: SrcSpan,
        name: String,
        module_name: Vec<String>,
        type_constructors: Vec<String>,
    },

    UnknownModuleValue {
        location: SrcSpan,
        name: String,
        module_name: Vec<String>,
        value_constructors: Vec<String>,
    },

    UnknownModuleField {
        location: SrcSpan,
        name: String,
        module_name: Vec<String>,
        value_constructors: Vec<String>,
        type_constructors: Vec<String>,
    },

    NotFn {
        location: SrcSpan,
        typ: Arc<Type>,
    },

    UnknownField {
        location: SrcSpan,
        typ: Arc<Type>,
        label: String,
        fields: Vec<String>,
    },

    IncorrectArity {
        location: SrcSpan,
        expected: usize,
        given: usize,
    },

    IncorrectTypeArity {
        location: SrcSpan,
        name: String,
        expected: usize,
        given: usize,
    },

    CouldNotUnify {
        location: SrcSpan,
        expected: Arc<Type>,
        given: Arc<Type>,
    },

    RecursiveType {
        location: SrcSpan,
    },

    DuplicateName {
        location: SrcSpan,
        previous_location: SrcSpan,
        name: String,
    },

    DuplicateTypeName {
        location: SrcSpan,
        previous_location: SrcSpan,
        name: String,
    },

    DuplicateArgument {
        location: SrcSpan,
        label: String,
    },

    DuplicateField {
        location: SrcSpan,
        label: String,
    },

    PrivateTypeLeak {
        location: SrcSpan,
        leaked: Type,
    },

    UnexpectedLabelledArg {
        location: SrcSpan,
        label: String,
    },

    PositionalArgumentAfterLabelled {
        location: SrcSpan,
    },

    IncorrectNumClausePatterns {
        location: SrcSpan,
        expected: usize,
        given: usize,
    },

    NonLocalClauseGuardVariable {
        location: SrcSpan,
        name: String,
    },

    ExtraVarInAlternativePattern {
        location: SrcSpan,
        name: String,
    },

    OutOfBoundsTupleIndex {
        location: SrcSpan,
        index: u64,
        size: usize,
    },

    NotATuple {
        location: SrcSpan,
        given: Arc<Type>,
    },

    NotATupleUnbound {
        location: SrcSpan,
    },

    RecordAccessUnknownType {
        location: SrcSpan,
    },
}

#[derive(Debug, PartialEq)]
pub enum GetValueConstructorError {
    UnknownVariable {
        name: String,
        variables: Vec<String>,
    },

    UnknownModule {
        name: String,
        imported_modules: Vec<String>,
    },

    UnknownModuleValue {
        name: String,
        module_name: Vec<String>,
        value_constructors: Vec<String>,
    },
}

fn convert_get_value_constructor_error(e: GetValueConstructorError, location: &SrcSpan) -> Error {
    match e {
        GetValueConstructorError::UnknownVariable { name, variables } => Error::UnknownVariable {
            location: location.clone(),
            name,
            variables,
        },

        GetValueConstructorError::UnknownModule {
            name,
            imported_modules,
        } => Error::UnknownModule {
            location: location.clone(),
            name,
            imported_modules,
        },

        GetValueConstructorError::UnknownModuleValue {
            name,
            module_name,
            value_constructors,
        } => Error::UnknownModuleValue {
            location: location.clone(),
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
        type_constructors: Vec<String>,
    },

    UnknownModule {
        name: String,
        imported_modules: Vec<String>,
    },

    UnknownModuleType {
        name: String,
        module_name: Vec<String>,
        type_constructors: Vec<String>,
    },
}

fn convert_get_type_constructor_error(e: GetTypeConstructorError, location: &SrcSpan) -> Error {
    match e {
        GetTypeConstructorError::UnknownType {
            name,
            type_constructors,
        } => Error::UnknownType {
            location: location.clone(),
            name,
            types: type_constructors,
        },

        GetTypeConstructorError::UnknownModule {
            name,
            imported_modules,
        } => Error::UnknownModule {
            location: location.clone(),
            name,
            imported_modules,
        },

        GetTypeConstructorError::UnknownModuleType {
            name,
            module_name,
            type_constructors,
        } => Error::UnknownModuleType {
            location: location.clone(),
            name,
            module_name,
            type_constructors,
        },
    }
}

fn make_type_vars(
    args: &[String],
    vars: &mut im::HashMap<String, (usize, Arc<Type>)>,
    location: &SrcSpan,
    env: &mut Env,
) -> Result<Vec<Arc<Type>>, Error> {
    args.iter()
        .map(|arg| TypeAst::Var {
            location: location.clone(),
            name: arg.to_string(),
        })
        .map(|ast| env.type_from_ast(&ast, vars, NewTypeAction::MakeGeneric))
        .collect::<Result<_, _>>()
}

/// Iterate over a module, registering any new types created by the module into the env
fn register_types(
    statement: &UntypedStatement,
    module: &[String],
    env: &mut Env,
) -> Result<(), Error> {
    match statement {
        Statement::ExternalType {
            name,
            public,
            args,
            location,
            ..
        }
        | Statement::CustomType {
            name,
            public,
            args,
            location,
            ..
        } => {
            let mut type_vars = hashmap![];
            let parameters = make_type_vars(args, &mut type_vars, location, env)?;
            let typ = Arc::new(Type::App {
                public: *public,
                module: module.to_owned(),
                name: name.clone(),
                args: parameters.clone(),
            });
            env.insert_type_constructor(
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

        Statement::TypeAlias {
            location,
            public,
            args,
            alias: name,
            resolved_type,
            ..
        } => {
            let mut type_vars = hashmap![];
            let parameters = make_type_vars(args, &mut type_vars, location, env)?;
            let typ = env.type_from_ast(&resolved_type, &mut type_vars, NewTypeAction::Disallow)?;
            env.insert_type_constructor(
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

/// Crawl the AST, annotating each node with the inferred type or
/// returning an error.
///
pub fn infer_module(
    module: UntypedModule,
    modules: &HashMap<String, Module>,
) -> Result<TypedModule, Error> {
    let mut env = Env::new(module.name.as_slice(), modules);
    let module_name = &module.name;

    // Register types so they can be used in constructors and functions
    // earlier in the file
    for s in module.statements.iter() {
        register_types(s, module_name, &mut env)?;
    }

    let statements: Vec<TypedStatement> = module
        .statements
        .into_iter()
        .map(|s| match s {
            Statement::Fn {
                doc,
                location,
                name,
                public,
                args,
                body,
                return_annotation,
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
                let (args, body) =
                    do_infer_fn(args, body, &return_annotation, level + 1, &mut env)?;
                let args_types = args.iter().map(|a| a.typ.clone()).collect();
                let typ = fn_(args_types, body.typ());

                // Assert that the inferred type matches the type of any recursive call
                unify(rec, typ.clone(), &env).map_err(|e| convert_unify_error(e, &location))?;
                let typ = generalise(typ, level);

                // Insert the function into the module's interface
                env.insert_module_value(
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

                let statement: TypedStatement = Statement::Fn {
                    doc,
                    location,
                    name,
                    public,
                    args,
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
                // Construct type of function from AST
                let mut type_vars = hashmap![];
                let return_type =
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
                                location: location.clone(),
                            })?;
                    }
                }
                let field_map = field_map.into_option();
                let typ = fn_(args_types, return_type.clone());

                // Insert function into module
                env.insert_module_value(
                    &name,
                    ValueConstructor {
                        public,
                        typ: typ.clone(),
                        origin: location.clone(),
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
                let typ = env
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
                name,
                args,
                constructors,
            } => {
                let mut type_vars = hashmap![];

                // This custom type was inserted into the module types in the `register_types`
                // pass, so we can expect this type to exist already.
                let retrn = env
                    .module_types
                    .get(&name)
                    .gleam_expect("Type for custom type not found on constructor infer pass")
                    .typ
                    .clone();

                // Register the parameterised types in the type into type_vars so that they are
                // used when building the constructors below.
                for (typ, name) in retrn
                    .app_parameters()
                    .unwrap_or(&[])
                    .iter()
                    .zip(args.iter())
                {
                    type_vars.insert(name.to_string(), (0, typ.clone()));
                }

                // If the custom type only has a single constructor then we can access the
                // fields using the record.field syntax, so store any fields accessors.
                if let Some(accessors) =
                    custom_type_accessors(constructors.as_slice(), &mut type_vars, &mut env)?
                {
                    let map = AccessorsMap {
                        public,
                        accessors,
                        typ: retrn.clone(),
                    };
                    env.insert_accessors(name.as_ref(), map)
                }

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
                                    location: location.clone(),
                                }
                            })?;
                        }
                    }
                    let field_map = field_map.into_option();
                    // Insert constructor function into module scope
                    let typ = match constructor.args.len() {
                        0 => retrn.clone(),
                        _ => fn_(args_types, retrn.clone()),
                    };
                    env.insert_module_value(
                        &constructor.name,
                        ValueConstructor {
                            public,
                            typ: typ.clone(),
                            origin: constructor.location.clone(),
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
                    doc,
                    location,
                    public,
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
                let mut type_vars = hashmap![];
                for arg in args.iter() {
                    let var = TypeAst::Var {
                        location: location.clone(),
                        name: arg.to_string(),
                    };
                    env.type_from_ast(&var, &mut type_vars, NewTypeAction::MakeGeneric)?;
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
                    location,
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
                        env.insert_type_constructor(imported_name.clone(), typ.clone())?;
                        imported = true;
                    }

                    if !imported {
                        return Err(Error::UnknownModuleField {
                            location: location.clone(),
                            name: name.clone(),
                            module_name: module,
                            value_constructors: module_info
                                .values
                                .keys()
                                .map(|t| t.to_string())
                                .collect(),
                            type_constructors: module_info
                                .types
                                .keys()
                                .map(|t| t.to_string())
                                .collect(),
                        });
                    }
                }

                // Insert imported module into scope
                env.imported_modules
                    .insert(module_name, module_info.clone());

                Ok(Statement::Import {
                    location,
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
    env.accessors.retain(|_, accessors| accessors.public);

    // Ensure no exported values have private types in their type signature
    for (_, value) in env.module_values.iter() {
        if let Some(leaked) = value.typ.find_private_type() {
            return Err(Error::PrivateTypeLeak {
                location: value.origin.clone(),
                leaked,
            });
        }
    }

    let Env {
        module_types: types,
        module_values: values,
        accessors,
        ..
    } = env;

    Ok(ast::Module {
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

fn custom_type_accessors(
    constructors: &[RecordConstructor],
    type_vars: &mut im::HashMap<String, (usize, Arc<Type>)>,
    env: &mut Env,
) -> Result<Option<HashMap<String, RecordAccessor>>, Error> {
    let args = match constructors {
        [constructor] if !constructor.args.is_empty() => &constructor.args,
        _ => return Ok(None),
    };

    let mut fields = HashMap::with_capacity(args.len());
    for (index, (label, arg)) in args.iter().enumerate() {
        if let Some(label) = label {
            let typ = env.type_from_ast(arg, type_vars, NewTypeAction::Disallow)?;
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

/// Crawl the AST, annotating each node with the inferred type or
/// returning an error.
///
pub fn infer(expr: UntypedExpr, level: usize, env: &mut Env) -> Result<TypedExpr, Error> {
    match expr {
        UntypedExpr::ListNil { location, .. } => infer_nil(location, level, env),
        UntypedExpr::Todo { location, .. } => infer_todo(location, level, env),
        UntypedExpr::Var { location, name, .. } => infer_var(name, location, level, env),
        UntypedExpr::Int {
            location, value, ..
        } => infer_int(value, location),
        UntypedExpr::Seq { first, then, .. } => infer_seq(*first, *then, level, env),
        UntypedExpr::Tuple {
            location, elems, ..
        } => infer_tuple(elems, location, level, env),
        UntypedExpr::Float {
            location, value, ..
        } => infer_float(value, location),
        UntypedExpr::String {
            location, value, ..
        } => infer_string(value, location),
        UntypedExpr::Pipe {
            left,
            right,
            location,
        } => infer_pipe(*left, *right, location, level, env),

        UntypedExpr::Fn {
            location,
            is_capture,
            args,
            body,
            return_annotation,
            ..
        } => infer_fn(
            args,
            *body,
            is_capture,
            return_annotation,
            level,
            location,
            env,
        ),

        UntypedExpr::Let {
            location,
            pattern,
            value,
            then,
            assert,
            ..
        } => infer_let(pattern, *value, *then, assert, level, location, env),

        UntypedExpr::Case {
            location,
            subjects,
            clauses,
            ..
        } => infer_case(subjects, clauses, level, location, env),

        UntypedExpr::ListCons {
            location,
            head,
            tail,
            ..
        } => infer_cons(*head, *tail, location, level, env),

        UntypedExpr::Call {
            location,
            fun,
            args,
            ..
        } => infer_call(*fun, args, level, location, env),

        UntypedExpr::BinOp {
            location,
            name,
            left,
            right,
            ..
        } => infer_binop(name, *left, *right, level, location, env),

        UntypedExpr::FieldAccess {
            location,
            label,
            container,
            ..
        } => infer_field_access(*container, label, location, level, env),

        UntypedExpr::TupleIndex {
            location,
            index,
            tuple,
            ..
        } => infer_tuple_index(*tuple, index, location, level, env),
    }
}

fn infer_pipe(
    left: UntypedExpr,
    right: UntypedExpr,
    location: SrcSpan,
    level: usize,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    infer_insert_pipe(&left, &right, level, env)
        .or_else(|()| infer_apply_pipe(left, right, location, level, env))
}

/// Attempt to infer a |> b(c) as b(a, c)
fn infer_insert_pipe(
    left: &UntypedExpr,
    right: &UntypedExpr,
    level: usize,
    env: &mut Env,
) -> Result<TypedExpr, ()> {
    if let UntypedExpr::Call {
        location,
        fun,
        args,
    } = right
    {
        // TODO: This clones the full tree under the args. :(
        let mut new_args = Vec::with_capacity(args.len() + 1);
        new_args.push(CallArg {
            label: None,
            location: left.location().clone(),
            value: left.clone(),
        });
        for arg in args {
            new_args.push(arg.clone());
        }
        let call = UntypedExpr::Call {
            location: location.clone(),
            fun: fun.clone(),
            args: new_args,
        };
        return infer(call, level, env).map_err(|_| ());
    }
    Err(())
}

/// Attempt to infer a |> b as b(a)
fn infer_apply_pipe(
    left: UntypedExpr,
    right: UntypedExpr,
    location: SrcSpan,
    level: usize,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let left = Box::new(infer(left, level, env)?);
    let right = Box::new(infer(right, level, env)?);
    let typ = env.new_unbound_var(level);
    let fn_typ = Arc::new(Type::Fn {
        args: vec![left.typ()],
        retrn: typ.clone(),
    });
    unify(right.typ(), fn_typ, env).map_err(|e| convert_unify_error(e, &location))?;

    Ok(TypedExpr::Pipe {
        location,
        typ,
        right,
        left,
    })
}

fn infer_nil(location: SrcSpan, level: usize, env: &mut Env) -> Result<TypedExpr, Error> {
    Ok(TypedExpr::ListNil {
        location,
        typ: list(env.new_unbound_var(level)),
    })
}

fn infer_todo(location: SrcSpan, level: usize, env: &mut Env) -> Result<TypedExpr, Error> {
    Ok(TypedExpr::Todo {
        location,
        typ: env.new_unbound_var(level),
    })
}

fn infer_string(value: String, location: SrcSpan) -> Result<TypedExpr, Error> {
    Ok(TypedExpr::String {
        location,
        value,
        typ: string(),
    })
}

fn infer_int(value: String, location: SrcSpan) -> Result<TypedExpr, Error> {
    Ok(TypedExpr::Int {
        location,
        value,
        typ: int(),
    })
}

fn infer_float(value: String, location: SrcSpan) -> Result<TypedExpr, Error> {
    Ok(TypedExpr::Float {
        location,
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
    Ok(TypedExpr::Seq {
        typ: then.typ(),
        first: Box::new(first),
        then: Box::new(then),
    })
}

fn infer_fn(
    args: Vec<UntypedArg>,
    body: UntypedExpr,
    is_capture: bool,
    return_annotation: Option<TypeAst>,
    level: usize,
    location: SrcSpan,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let (args, body) = do_infer_fn(args, body, &return_annotation, level, env)?;
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

fn infer_call(
    fun: UntypedExpr,
    args: Vec<CallArg<UntypedExpr>>,
    level: usize,
    location: SrcSpan,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let (fun, args, typ) = do_infer_call(fun, args, level, &location, env)?;
    Ok(TypedExpr::Call {
        location,
        typ,
        args,
        fun: Box::new(fun),
    })
}

fn infer_cons(
    head: UntypedExpr,
    tail: UntypedExpr,
    location: SrcSpan,
    level: usize,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let head = infer(head, level, env)?;
    let tail = infer(tail, level, env)?;
    unify(tail.typ(), list(head.typ()), env).map_err(|e| convert_unify_error(e, &location))?;
    Ok(TypedExpr::ListCons {
        location,
        typ: tail.typ(),
        head: Box::new(head),
        tail: Box::new(tail),
    })
}

fn infer_tuple(
    elems: Vec<UntypedExpr>,
    location: SrcSpan,
    level: usize,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let elems = elems
        .into_iter()
        .map(|e| infer(e, level, env))
        .collect::<Result<Vec<_>, _>>()?;
    let typ = tuple(elems.iter().map(|e| e.typ()).collect());
    Ok(TypedExpr::Tuple {
        location,
        elems,
        typ,
    })
}
fn infer_var(
    name: String,
    location: SrcSpan,
    level: usize,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let constructor = infer_value_constructor(&name, level, &location, env)?;
    Ok(TypedExpr::Var {
        constructor,
        location,
        name,
    })
}

fn infer_field_access(
    container: UntypedExpr,
    label: String,
    access_location: SrcSpan,
    level: usize,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    match container {
        UntypedExpr::Var { name, location, .. } if !env.local_values.contains_key(&name) => {
            infer_module_access(name.as_ref(), label, level, &location, access_location, env)
        }

        _ => infer_record_access(container, label, level, access_location, env),
    }
}

fn infer_tuple_index(
    tuple: UntypedExpr,
    index: u64,
    location: SrcSpan,
    level: usize,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let tuple = infer(tuple, level, env)?;

    match &*tuple.typ() {
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

fn infer_binop(
    name: BinOp,
    left: UntypedExpr,
    right: UntypedExpr,
    level: usize,
    location: SrcSpan,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let (input_type, output_type) = match name {
        BinOp::Eq | BinOp::NotEq => {
            let left = infer(left, level, env)?;
            let right = infer(right, level, env)?;
            unify(left.typ(), right.typ(), env)
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

    let left = infer(left, level, env)?;
    unify(input_type.clone(), left.typ(), env)
        .map_err(|e| convert_unify_error(e, left.location()))?;
    let right = infer(right, level, env)?;
    unify(input_type, right.typ(), env).map_err(|e| convert_unify_error(e, right.location()))?;

    Ok(TypedExpr::BinOp {
        location,
        name,
        typ: output_type,
        left: Box::new(left),
        right: Box::new(right),
    })
}

fn infer_let(
    pattern: UntypedPattern,
    value: UntypedExpr,
    then: UntypedExpr,
    assert: bool,
    level: usize,
    location: SrcSpan,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let value = infer(value, level + 1, env)?;
    let value_typ = generalise(value.typ(), level + 1);
    let pattern = PatternTyper::new(env, level).unify(pattern, value_typ)?;
    let then = infer(then, level, env)?;
    let typ = then.typ();
    Ok(TypedExpr::Let {
        location,
        typ,
        pattern,
        value: Box::new(value),
        then: Box::new(then),
        assert: assert,
    })
}

fn infer_case(
    subjects: Vec<UntypedExpr>,
    clauses: Vec<UntypedClause>,
    level: usize,
    location: SrcSpan,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let subjects_count = subjects.len();
    let mut typed_subjects = Vec::with_capacity(subjects_count);
    let mut subject_types = Vec::with_capacity(subjects_count);
    let mut typed_clauses = Vec::with_capacity(clauses.len());

    let return_type = env.new_unbound_var(level);

    for subject in subjects.into_iter() {
        let subject = infer(subject, level + 1, env)?;
        let subject_type = generalise(subject.typ(), level + 1);
        typed_subjects.push(subject);
        subject_types.push(subject_type);
    }

    for clause in clauses.into_iter() {
        let typed_clause = infer_clause(clause, &subject_types, level, env)?;
        unify(return_type.clone(), typed_clause.then.typ(), env)
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
    clause: UntypedClause,
    subjects: &[Arc<Type>],
    level: usize,
    env: &mut Env,
) -> Result<TypedClause, Error> {
    let Clause {
        pattern,
        alternative_patterns,
        guard,
        then,
        location,
    } = clause;

    // Store the local scope so it can be reset after the clause
    let vars = env.local_values.clone();

    // Check the types
    let (typed_pattern, typed_alternatives) = infer_clause_pattern(
        pattern,
        alternative_patterns,
        subjects,
        level,
        &location,
        env,
    )?;
    let guard = infer_optional_clause_guard(guard, level, env)?;
    let then = infer(then, level, env)?;

    // Reset the local vars now the clause scope is done
    env.local_values = vars;

    Ok(Clause {
        location,
        pattern: typed_pattern,
        alternative_patterns: typed_alternatives,
        guard,
        then,
    })
}

fn infer_clause_pattern(
    pattern: UntypedMultiPattern,
    alternatives: Vec<UntypedMultiPattern>,
    subjects: &[Arc<Type>],
    level: usize,
    location: &SrcSpan,
    env: &mut Env,
) -> Result<(TypedMultiPattern, Vec<TypedMultiPattern>), Error> {
    let mut pattern_typer = PatternTyper::new(env, level);
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
            unify(bool(), guard.typ(), env)
                .map_err(|e| convert_unify_error(e, guard.location()))?;
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
        ClauseGuard::Var { location, name, .. } => {
            let constructor = infer_value_constructor(&name, level, &location, env)?;

            // We cannot support all values in guard expressions as the BEAM does not
            match &constructor.variant {
                ValueConstructorVariant::LocalVariable => (),
                ValueConstructorVariant::ModuleFn { .. }
                | ValueConstructorVariant::Record { .. } => {
                    return Err(Error::NonLocalClauseGuardVariable { location, name })
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
            let left = infer_clause_guard(*left, level, env)?;
            unify(bool(), left.typ(), env).map_err(|e| convert_unify_error(e, left.location()))?;
            let right = infer_clause_guard(*right, level, env)?;
            unify(bool(), right.typ(), env)
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
            let left = infer_clause_guard(*left, level, env)?;
            unify(bool(), left.typ(), env).map_err(|e| convert_unify_error(e, left.location()))?;
            let right = infer_clause_guard(*right, level, env)?;
            unify(bool(), right.typ(), env)
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
            let left = infer_clause_guard(*left, level, env)?;
            let right = infer_clause_guard(*right, level, env)?;
            unify(left.typ(), right.typ(), env).map_err(|e| convert_unify_error(e, &location))?;
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
            let left = infer_clause_guard(*left, level, env)?;
            let right = infer_clause_guard(*right, level, env)?;
            unify(left.typ(), right.typ(), env).map_err(|e| convert_unify_error(e, &location))?;
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
            let left = infer_clause_guard(*left, level, env)?;
            unify(int(), left.typ(), env).map_err(|e| convert_unify_error(e, left.location()))?;
            let right = infer_clause_guard(*right, level, env)?;
            unify(int(), right.typ(), env).map_err(|e| convert_unify_error(e, right.location()))?;
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
            let left = infer_clause_guard(*left, level, env)?;
            unify(int(), left.typ(), env).map_err(|e| convert_unify_error(e, left.location()))?;
            let right = infer_clause_guard(*right, level, env)?;
            unify(int(), right.typ(), env).map_err(|e| convert_unify_error(e, right.location()))?;
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
            let left = infer_clause_guard(*left, level, env)?;
            unify(int(), left.typ(), env).map_err(|e| convert_unify_error(e, left.location()))?;
            let right = infer_clause_guard(*right, level, env)?;
            unify(int(), right.typ(), env).map_err(|e| convert_unify_error(e, right.location()))?;
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
            let left = infer_clause_guard(*left, level, env)?;
            unify(int(), left.typ(), env).map_err(|e| convert_unify_error(e, left.location()))?;
            let right = infer_clause_guard(*right, level, env)?;
            unify(int(), right.typ(), env).map_err(|e| convert_unify_error(e, right.location()))?;
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
            let left = infer_clause_guard(*left, level, env)?;
            unify(float(), left.typ(), env).map_err(|e| convert_unify_error(e, left.location()))?;
            let right = infer_clause_guard(*right, level, env)?;
            unify(float(), right.typ(), env)
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
            let left = infer_clause_guard(*left, level, env)?;
            unify(float(), left.typ(), env).map_err(|e| convert_unify_error(e, left.location()))?;
            let right = infer_clause_guard(*right, level, env)?;
            unify(float(), right.typ(), env)
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
            let left = infer_clause_guard(*left, level, env)?;
            unify(float(), left.typ(), env).map_err(|e| convert_unify_error(e, left.location()))?;
            let right = infer_clause_guard(*right, level, env)?;
            unify(float(), right.typ(), env)
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
            let left = infer_clause_guard(*left, level, env)?;
            unify(float(), left.typ(), env).map_err(|e| convert_unify_error(e, left.location()))?;
            let right = infer_clause_guard(*right, level, env)?;
            unify(float(), right.typ(), env)
                .map_err(|e| convert_unify_error(e, right.location()))?;
            Ok(ClauseGuard::LtEqFloat {
                location,
                left: Box::new(left),
                right: Box::new(right),
            })
        }

        ClauseGuard::Int {
            location, value, ..
        } => Ok(ClauseGuard::Int { location, value }),
    }
}

fn infer_module_access(
    module_alias: &str,
    label: String,
    level: usize,
    module_location: &SrcSpan,
    select_location: SrcSpan,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    let (module_name, constructor) = {
        let module_info =
            env.imported_modules
                .get(&*module_alias)
                .ok_or_else(|| Error::UnknownModule {
                    name: module_alias.to_string(),
                    location: module_location.clone(),
                    imported_modules: env.imported_modules.keys().map(|t| t.to_string()).collect(),
                })?;

        let constructor =
            module_info
                .values
                .get(&label)
                .ok_or_else(|| Error::UnknownModuleValue {
                    name: label.clone(),
                    location: select_location.clone(),
                    module_name: module_info.name.clone(),
                    value_constructors: module_info.values.keys().map(|t| t.to_string()).collect(),
                })?;

        (module_info.name.clone(), constructor.clone())
    };

    Ok(TypedExpr::ModuleSelect {
        label,
        typ: instantiate(constructor.typ, level, &mut hashmap![], env),
        location: select_location,
        module_name,
        module_alias: module_alias.to_string(),
        constructor: constructor.variant.to_module_value_constructor(),
    })
}

fn infer_record_access(
    record: UntypedExpr,
    label: String,
    level: usize,
    location: SrcSpan,
    env: &mut Env,
) -> Result<TypedExpr, Error> {
    // Infer the type of the (presumed) record
    let record = Box::new(infer(record, level, env)?);

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
    let accessors = match collapse_links(record.typ().clone()).as_ref() {
        // A type in the current module which may have fields
        Type::App { module, name, .. } if module.as_slice() == env.current_module => {
            env.accessors.get(name)
        }

        // A type in another module which may have fields
        Type::App { module, name, .. } => env
            .importable_modules
            .get(&module.join("/"))
            .and_then(|module| module.accessors.get(name)),

        _something_without_fields => return Err(unknown_field(vec![])),
    }
    .ok_or_else(|| unknown_field(vec![]))?;

    // Find the accessor, if the type has one with the same label
    let RecordAccessor {
        index, label, typ, ..
    } = accessors
        .accessors
        .get(&label)
        .ok_or_else(|| unknown_field(accessors.accessors.keys().map(|t| t.to_string()).collect()))?
        .clone();

    // Unify the record type with the accessor's stored copy of the record type.
    // This ensure that the type parameters of the retrieved value have the correct
    // types for this instance of the record.
    let accessor_record_type = accessors.typ.clone();
    let mut type_vars = hashmap![];
    let accessor_record_type = instantiate(accessor_record_type, 0, &mut type_vars, env);
    let typ = instantiate(typ, 0, &mut type_vars, env);
    unify(accessor_record_type, record.typ().clone(), env)
        .map_err(|e| convert_unify_error(e, record.location()))?;

    Ok(TypedExpr::RecordAccess {
        record,
        label,
        index,
        location,
        typ,
    })
}

struct PatternTyper<'a, 'b, 'c> {
    env: &'a mut Env<'b, 'c>,
    level: usize,
    mode: PatternMode,
    initial_pattern_vars: HashSet<String>,
}

enum PatternMode {
    Initial,
    Alternative,
}

impl<'a, 'b, 'c> PatternTyper<'a, 'b, 'c> {
    pub fn new(env: &'a mut Env<'b, 'c>, level: usize) -> Self {
        Self {
            env,
            level,
            mode: PatternMode::Initial,
            initial_pattern_vars: HashSet::new(),
        }
    }

    fn insert_variable(&mut self, name: &str, typ: Arc<Type>) -> Result<(), UnifyError> {
        match self.mode {
            PatternMode::Initial => {
                self.initial_pattern_vars.insert(name.to_string());
                self.env.insert_variable(
                    name.to_string(),
                    ValueConstructorVariant::LocalVariable,
                    typ,
                );
                Ok(())
            }

            PatternMode::Alternative => match self.env.local_values.get(name) {
                // This variable was defined in the Initial multi-pattern
                Some(initial) if self.initial_pattern_vars.contains(name) => {
                    unify(initial.typ.clone(), typ, self.env)
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
        subjects: &[Arc<Type>],
        location: &SrcSpan,
    ) -> Result<Vec<TypedPattern>, Error> {
        self.mode = PatternMode::Alternative;
        let typed_multi = self.infer_multi_pattern(multi_pattern, subjects, location)?;
        Ok(typed_multi)
    }

    fn infer_multi_pattern(
        &mut self,
        multi_pattern: UntypedMultiPattern,
        subjects: &[Arc<Type>],
        location: &SrcSpan,
    ) -> Result<Vec<TypedPattern>, Error> {
        // If there are N subjects the multi-pattern is expected to be N patterns
        if subjects.len() != multi_pattern.len() {
            return Err(Error::IncorrectNumClausePatterns {
                location: location.clone(),
                expected: subjects.len(),
                given: multi_pattern.len(),
            });
        }

        // Unify each pattern in the multi-pattern with the corresponding subject
        let mut typed_multi = Vec::with_capacity(multi_pattern.len());
        for (pattern, subject_type) in multi_pattern.into_iter().zip(subjects.iter()) {
            let pattern = self.unify(pattern, subject_type.clone())?;
            typed_multi.push(pattern);
        }
        Ok(typed_multi)
    }

    /// When we have an assignment or a case expression we unify the pattern with the
    /// inferred type of the subject in order to determine what variables to insert
    /// into the environment (or to detect a type error).
    ///
    fn unify(&mut self, pattern: UntypedPattern, typ: Arc<Type>) -> Result<TypedPattern, Error> {
        match pattern {
            Pattern::Discard { name, location } => Ok(Pattern::Discard { name, location }),

            Pattern::Var { name, location } => {
                self.insert_variable(name.as_ref(), typ)
                    .map_err(|e| convert_unify_error(e, &location))?;
                Ok(Pattern::Var { name, location })
            }

            Pattern::Let { name, pattern, .. } => {
                self.insert_variable(name.as_ref(), typ.clone())
                    .map_err(|e| convert_unify_error(e, pattern.location()))?;
                self.unify(*pattern, typ)
            }

            Pattern::Int { location, value } => {
                unify(typ, int(), self.env).map_err(|e| convert_unify_error(e, &location))?;
                Ok(Pattern::Int { location, value })
            }

            Pattern::Float { location, value } => {
                unify(typ, float(), self.env).map_err(|e| convert_unify_error(e, &location))?;
                Ok(Pattern::Float { location, value })
            }

            Pattern::String { location, value } => {
                unify(typ, string(), self.env).map_err(|e| convert_unify_error(e, &location))?;
                Ok(Pattern::String { location, value })
            }

            Pattern::Nil { location } => {
                unify(typ, list(self.env.new_unbound_var(self.level)), self.env)
                    .map_err(|e| convert_unify_error(e, &location))?;
                Ok(Pattern::Nil { location })
            }

            Pattern::Cons {
                location,
                head,
                tail,
            } => match typ.get_app_args(true, &[], "List", 1, self.env) {
                Some(args) => {
                    let head = Box::new(self.unify(*head, args[0].clone())?);
                    let tail = Box::new(self.unify(*tail, typ)?);
                    Ok(Pattern::Cons {
                        location,
                        head,
                        tail,
                    })
                }

                None => Err(Error::CouldNotUnify {
                    given: list(self.env.new_unbound_var(self.level)),
                    expected: typ.clone(),
                    location,
                }),
            },

            Pattern::Tuple { elems, location } => match &*collapse_links(typ.clone()) {
                Type::Tuple { elems: type_elems } => {
                    let elems = elems
                        .into_iter()
                        .zip(type_elems)
                        .map(|(pattern, typ)| self.unify(pattern, typ.clone()))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(Pattern::Tuple { elems, location })
                }

                Type::Var { .. } => {
                    let elems_types = (0..(elems.len()))
                        .map(|_| self.env.new_unbound_var(self.level))
                        .collect();
                    unify(tuple(elems_types), typ.clone(), self.env)
                        .map_err(|e| convert_unify_error(e, &location))?;
                    self.unify(Pattern::Tuple { elems, location }, typ)
                }

                other => {
                    dbg!(&other);
                    unimplemented!();
                }
            },

            Pattern::Constructor {
                location,
                module,
                name,
                args: mut pattern_args,
                ..
            } => {
                let cons = self
                    .env
                    .get_value_constructor(module.as_ref(), &name)
                    .map_err(|e| convert_get_value_constructor_error(e, &location))?;

                match cons.field_map() {
                    // The fun has a field map so labelled arguments may be present and need to be reordered.
                    Some(field_map) => field_map.reorder(&mut pattern_args, &location)?,

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

                let instantiated_constructor_type =
                    instantiate(constructor_typ, self.level, &mut hashmap![], self.env);
                match &*instantiated_constructor_type {
                    Type::Fn { args, retrn } => {
                        if args.len() == pattern_args.len() {
                            let pattern_args = pattern_args
                                .into_iter()
                                .zip(args)
                                .map(|(arg, typ)| {
                                    let CallArg {
                                        value,
                                        location,
                                        label,
                                    } = arg;
                                    let value = self.unify(value, typ.clone())?;
                                    Ok(CallArg {
                                        value,
                                        location,
                                        label,
                                    })
                                })
                                .collect::<Result<Vec<_>, _>>()?;
                            unify(typ, retrn.clone(), self.env)
                                .map_err(|e| convert_unify_error(e, &location))?;
                            Ok(Pattern::Constructor {
                                location,
                                module,
                                name,
                                args: pattern_args,
                                constructor,
                            })
                        } else {
                            Err(Error::IncorrectArity {
                                location,
                                expected: args.len(),
                                given: pattern_args.len(),
                            })
                        }
                    }

                    Type::App { .. } => {
                        if pattern_args.is_empty() {
                            unify(typ, instantiated_constructor_type, self.env)
                                .map_err(|e| convert_unify_error(e, &location))?;
                            Ok(Pattern::Constructor {
                                location,
                                module,
                                name,
                                args: vec![],
                                constructor,
                            })
                        } else {
                            Err(Error::IncorrectArity {
                                location,
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
    location: &SrcSpan,
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
            location: location.clone(),
            name: name.to_string(),
            variables: env.local_values.keys().map(|t| t.to_string()).collect(),
        })?;
    let typ = instantiate(typ, level, &mut hashmap![], env);
    Ok(ValueConstructor {
        public,
        variant,
        origin,
        typ,
    })
}

pub type TypedCallArg = CallArg<TypedExpr>;

fn do_infer_call(
    fun: UntypedExpr,
    mut args: Vec<CallArg<UntypedExpr>>,
    level: usize,
    location: &SrcSpan,
    env: &mut Env,
) -> Result<(TypedExpr, Vec<TypedCallArg>, Arc<Type>), Error> {
    let fun = infer(fun, level, env)?;

    match get_field_map(&fun, env).map_err(|e| convert_get_value_constructor_error(e, location))? {
        // The fun has a field map so labelled arguments may be present and need to be reordered.
        Some(field_map) => field_map.reorder(&mut args, location)?,

        // The fun has no field map and so we error if arguments have been labelled
        None => assert_no_labelled_arguments(&args)?,
    }

    let (mut args_types, return_type) = match_fun_type(fun.typ(), args.len(), env)
        .map_err(|e| convert_not_fun_error(e, fun.location(), &location))?;
    let args = args_types
        .iter_mut()
        .zip(args)
        .map(
            |(
                typ,
                CallArg {
                    label,
                    value,
                    location,
                },
            ): (&mut Arc<Type>, _)| {
                let value = infer(value, level, env)?;
                unify(typ.clone(), value.typ(), env)
                    .map_err(|e| convert_unify_error(e, value.location()))?;
                Ok(CallArg {
                    label,
                    value,
                    location,
                })
            },
        )
        .collect::<Result<_, _>>()?;
    Ok((fun, args, return_type))
}

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

fn get_field_map<'a>(
    constructor: &TypedExpr,
    env: &'a Env,
) -> Result<Option<&'a FieldMap>, GetValueConstructorError> {
    let (module, name) = match constructor {
        TypedExpr::ModuleSelect {
            module_alias,
            label,
            ..
        } => (Some(module_alias), label),

        TypedExpr::Var { name, .. } => (None, name),

        _ => return Ok(None),
    };

    Ok(env.get_value_constructor(module, name)?.field_map())
}

fn infer_arg(
    arg: UntypedArg,
    type_vars: &mut im::HashMap<String, (usize, Arc<Type>)>,
    level: usize,
    env: &mut Env,
) -> Result<TypedArg, Error> {
    let Arg {
        names,
        annotation,
        location,
        ..
    } = arg;
    let typ = annotation
        .clone()
        .map(|t| env.type_from_ast(&t, type_vars, NewTypeAction::MakeGeneric))
        .unwrap_or_else(|| Ok(env.new_unbound_var(level)))?;
    Ok(Arg {
        names,
        location,
        annotation,
        typ,
    })
}

fn do_infer_fn(
    args: Vec<UntypedArg>,
    body: UntypedExpr,
    return_annotation: &Option<TypeAst>,
    level: usize,
    env: &mut Env,
) -> Result<(Vec<TypedArg>, TypedExpr), Error> {
    // Construct an initial type for each argument of the function- either an unbound type variable
    // or a type provided by an annotation.
    let mut type_vars = hashmap![];
    let args: Vec<_> = args
        .into_iter()
        .map(|arg| infer_arg(arg, &mut type_vars, level, env))
        .collect::<Result<_, _>>()?;

    // Record generic type variables that comes from type annotations.
    // They cannot be instantiated so we need to keep track of them.
    let previous_annotated_generic_types = env.annotated_generic_types.clone();
    for (id, _type) in type_vars.values() {
        env.annotated_generic_types.insert(*id);
    }

    // Insert arguments into function body scope.
    let previous_vars = env.local_values.clone();
    for (arg, t) in args.iter().zip(args.iter().map(|arg| arg.typ.clone())) {
        match &arg.names {
            ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
                env.insert_variable(name.to_string(), ValueConstructorVariant::LocalVariable, t)
            }
            ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => (),
        };
    }

    let body = infer(body, level, env)?;

    // Check that any return type annotation is accurate.
    if let Some(ann) = return_annotation {
        let ret_typ = env.type_from_ast(ann, &mut type_vars, NewTypeAction::MakeGeneric)?;
        unify(ret_typ, body.typ(), env).map_err(|e| convert_unify_error(e, body.location()))?;
    }

    // Reset the env now that the scope of the function has ended.
    env.local_values = previous_vars;
    env.annotated_generic_types = previous_annotated_generic_types;
    Ok((args, body))
}

fn convert_unify_error(e: UnifyError, location: &SrcSpan) -> Error {
    match e {
        UnifyError::CouldNotUnify { expected, given } => Error::CouldNotUnify {
            location: location.clone(),
            expected,
            given,
        },

        UnifyError::ExtraVarInAlternativePattern { name } => Error::ExtraVarInAlternativePattern {
            location: location.clone(),
            name,
        },

        UnifyError::RecursiveType => Error::RecursiveType {
            location: location.clone(),
        },
    }
}

/// Instantiate converts generic variables into unbound ones.
///
fn instantiate(
    t: Arc<Type>,
    ctx_level: usize,
    ids: &mut im::HashMap<usize, Arc<Type>>,
    env: &mut Env,
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
                .map(|t| instantiate(t.clone(), ctx_level, ids, env))
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
                TypeVar::Link { typ } => return instantiate(typ.clone(), ctx_level, ids, env),

                TypeVar::Unbound { .. } => return Arc::new(Type::Var { typ: typ.clone() }),

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
            Arc::new(Type::Var { typ: typ.clone() })
        }

        Type::Fn { args, retrn, .. } => fn_(
            args.iter()
                .map(|t| instantiate(t.clone(), ctx_level, ids, env))
                .collect(),
            instantiate(retrn.clone(), ctx_level, ids, env),
        ),

        Type::Tuple { elems } => tuple(
            elems
                .iter()
                .map(|t| instantiate(t.clone(), ctx_level, ids, env))
                .collect(),
        ),
    }
}

#[derive(Debug, PartialEq)]
enum UnifyError {
    CouldNotUnify {
        expected: Arc<Type>,
        given: Arc<Type>,
    },

    ExtraVarInAlternativePattern {
        name: String,
    },

    RecursiveType,
}

fn unify_enclosed_type(
    e1: Arc<Type>,
    e2: Arc<Type>,
    result: Result<(), UnifyError>,
) -> Result<(), UnifyError> {
    // If types cannot unify, show the type error with the enclosing types, e1 and e2.
    match result {
        Err(UnifyError::CouldNotUnify { .. }) => Err(UnifyError::CouldNotUnify {
            expected: e1,
            given: e2,
        }),

        _ => result,
    }
}
fn unify(t1: Arc<Type>, t2: Arc<Type>, env: &Env) -> Result<(), UnifyError> {
    if t1 == t2 {
        return Ok(());
    }

    // Collapse right hand side type links. Left hand side will be collapsed in the next block.
    if let Type::Var { typ } = &*t2 {
        if let TypeVar::Link { typ } = &*typ.borrow() {
            return unify(t1, typ.clone(), env);
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

            Action::Unify(t) => unify(t, t2, env),

            Action::CouldNotUnify => Err(UnifyError::CouldNotUnify {
                expected: t1.clone(),
                given: t2,
            }),
        };
    }

    if let Type::Var { .. } = *t2 {
        return unify(t2, t1, env).map_err(flip_unify_error);
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
                unify_enclosed_type(t1.clone(), t2.clone(), unify(a.clone(), b.clone(), env))?;
            }
            Ok(())
        }

        (Type::Tuple { elems: elems1, .. }, Type::Tuple { elems: elems2, .. })
            if elems1.len() == elems2.len() =>
        {
            for (a, b) in elems1.iter().zip(elems2) {
                unify_enclosed_type(t1.clone(), t2.clone(), unify(a.clone(), b.clone(), env))?;
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
                unify(a.clone(), b.clone(), env)?;
            }
            unify(retrn1.clone(), retrn2.clone(), env)
        }

        (_, _) => Err(UnifyError::CouldNotUnify {
            expected: t1.clone(),
            given: t2.clone(),
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
    env: &mut Env,
) -> Result<(Vec<Arc<Type>>, Arc<Type>), MatchFunTypeError> {
    if let Type::Var { typ } = &*typ {
        let new_value = match &*typ.borrow() {
            TypeVar::Link { typ, .. } => return match_fun_type(typ.clone(), arity, env),

            TypeVar::Unbound { level, .. } => {
                let args: Vec<_> = (0..arity).map(|_| env.new_unbound_var(*level)).collect();
                let retrn = env.new_unbound_var(*level);
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

enum MatchFunTypeError {
    IncorrectArity { expected: usize, given: usize },
    NotFn { typ: Arc<Type> },
}

fn convert_not_fun_error(
    e: MatchFunTypeError,
    fn_location: &SrcSpan,
    call_location: &SrcSpan,
) -> Error {
    match e {
        MatchFunTypeError::IncorrectArity { expected, given } => Error::IncorrectArity {
            location: call_location.clone(),
            expected,
            given,
        },

        MatchFunTypeError::NotFn { typ } => Error::NotFn {
            location: fn_location.clone(),
            typ,
        },
    }
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

pub fn int() -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: "Int".to_string(),
        module: vec![],
        args: vec![],
    })
}

pub fn float() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: "Float".to_string(),
        module: vec![],
    })
}

pub fn bool() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: "Bool".to_string(),
        module: vec![],
    })
}

pub fn string() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: "String".to_string(),
        module: vec![],
    })
}

pub fn nil() -> Arc<Type> {
    Arc::new(Type::App {
        args: vec![],
        public: true,
        name: "Nil".to_string(),
        module: vec![],
    })
}

pub fn list(t: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: "List".to_string(),
        module: vec![],
        args: vec![t],
    })
}

pub fn result(a: Arc<Type>, e: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::App {
        public: true,
        name: "Result".to_string(),
        module: vec![],
        args: vec![a, e],
    })
}

pub fn tuple(elems: Vec<Arc<Type>>) -> Arc<Type> {
    Arc::new(Type::Tuple { elems })
}

pub fn fn_(args: Vec<Arc<Type>>, retrn: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::Fn { retrn, args })
}
