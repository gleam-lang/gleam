use crate::ast::{
    self, Arg, BinOp, Clause, Expr, Meta, Module, Pattern, Scope, Statement, TypedExpr,
    TypedModule, UntypedExpr, UntypedModule,
};
use crate::pretty::*;
use im::{hashmap::HashMap, ordmap::OrdMap};
use itertools::Itertools;
use std::cell::RefCell;
use std::rc::Rc;

const INDENT: isize = 2;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Const {
        public: bool,
        module: String,
        name: String,
    },

    App {
        public: bool,
        module: String,
        name: String,
        args: Vec<Type>,
    },

    Tuple {
        elems: Vec<Type>,
    },

    Fn {
        args: Vec<Type>,
        retrn: Box<Type>,
    },

    Record {
        row: Box<Type>,
    },

    Module {
        row: Box<Type>,
    },

    Var {
        typ: Rc<RefCell<TypeVar>>,
    },

    RowNil,

    RowCons {
        label: String,
        head: Box<Type>,
        tail: Box<Type>,
    },
}

impl Type {
    pub fn pretty_print(&self, mut initial_indent: usize) -> String {
        let mut b = String::with_capacity(initial_indent);
        for _ in 0..initial_indent {
            b.push(' ');
        }
        b.to_doc()
            .append(
                self.to_gleam_doc(&mut hashmap![], &mut initial_indent)
                    .nest(initial_indent as isize),
            )
            .format(80)
    }

    pub fn to_gleam_doc(&self, names: &mut HashMap<usize, String>, uid: &mut usize) -> Document {
        match self {
            Type::Const { name, .. } => name.clone().to_doc(),

            Type::App { name, args, .. } => name
                .clone()
                .to_doc()
                .append("(")
                .append(args_to_gleam_doc(args, names, uid))
                .append(")"),

            Type::Fn { args, retrn } => "fn("
                .to_doc()
                .append(args_to_gleam_doc(args, names, uid))
                .append(") -> ")
                .append(retrn.to_gleam_doc(names, uid)),

            Type::Tuple { elems, .. } => args_to_gleam_doc(elems, names, uid).surround("{", "}"),

            Type::Record { row } => {
                if let Type::RowNil { .. } = **row {
                    return "{}".to_doc();
                }
                let mut row_to_doc = |row: &Type| {
                    let mut fields = ordmap![];
                    let tail = row.gather_fields(&mut fields);
                    let fields_doc = fields
                        .into_iter()
                        .map(|(label, typ)| {
                            label.to_doc().append(" =").append(
                                break_("", " ")
                                    .append(typ.to_gleam_doc(names, uid))
                                    .nest(INDENT)
                                    .group(),
                            )
                        })
                        .intersperse(break_(",", ", "))
                        .collect::<Vec<_>>()
                        .to_doc();
                    match tail {
                        None => fields_doc,
                        Some(tail) => tail
                            .to_gleam_doc(names, uid)
                            .group()
                            .append(" | ")
                            .append(fields_doc),
                    }
                };
                "{".to_doc()
                    .append(
                        break_("", " ")
                            .append(row_to_doc(row))
                            .nest(INDENT)
                            .append(break_("", " "))
                            .group(),
                    )
                    .append("}")
            }

            Type::Module { row } => {
                let mut row_to_doc = |row: &Type| {
                    let mut fields = ordmap![];
                    let tail = row.gather_fields(&mut fields);
                    let fields_docs = fields
                        .into_iter()
                        .map(|(label, t)| match t.collapse_links() {
                            Type::Fn { args, retrn } => "fn "
                                .to_doc()
                                .append(label)
                                .append(
                                    "(".to_doc()
                                        .append(break_("", ""))
                                        .append(args_to_gleam_doc(&args, names, uid))
                                        .append(break_("", ""))
                                        .append(")")
                                        .group(),
                                )
                                .append(" -> ")
                                .append(retrn.to_gleam_doc(names, uid)),

                            other => "const "
                                .to_doc()
                                .append(label)
                                .append(": ")
                                .append(other.to_gleam_doc(names, uid).nest(INDENT)),
                        })
                        .intersperse(break_("", " "))
                        .collect::<Vec<_>>();
                    let fields_doc = if fields_docs.len() > 1 {
                        force_break().append(fields_docs)
                    } else {
                        fields_docs.to_doc()
                    };
                    match tail {
                        None => fields_doc,
                        Some(tail) => tail
                            .to_gleam_doc(names, uid)
                            .append(" | ")
                            .append(fields_doc),
                    }
                };

                "module {"
                    .to_doc()
                    .append(
                        break_("", " ")
                            .append(row_to_doc(row))
                            .nest(INDENT)
                            .append(break_("", " "))
                            .group(),
                    )
                    .append("}")
            }

            Type::Var { typ, .. } => typ.borrow().to_gleam_doc(names, uid),

            Type::RowCons { .. } => unreachable!(),

            Type::RowNil { .. } => nil(),
        }
    }

    fn gather_fields(&self, fields: &mut OrdMap<String, Type>) -> Option<Type> {
        match self {
            Type::RowNil => None,

            Type::RowCons { label, head, tail } => {
                if !fields.contains_key(label) {
                    fields.insert(label.clone(), *head.clone());
                }
                tail.gather_fields(fields)
            }

            Type::Var { typ } => match &*typ.borrow() {
                TypeVar::Link { typ } => typ.gather_fields(fields),
                _other => Some(self.clone()),
            },

            other => Some(other.clone()),
        }
    }

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
    /// TODO: This is currently wrong in that if an unbound var is found it assumes that the App
    /// takes a single arg. Really the number of args should be passed in.
    ///
    pub fn get_app_args(
        &self,
        public: bool,
        module: &str,
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
                if module == m && name == n {
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

                    TypeVar::Generic { .. } => unimplemented!(),
                };

                match action {
                    Action::Link(args) => {
                        *typ.borrow_mut() = TypeVar::Link {
                            typ: Box::new(Type::App {
                                name: name.to_string(),
                                module: module.to_string(),
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
            Type::RowNil { .. } => None,

            Type::Const { public: true, .. } => None,

            Type::Const { .. } => Some(self.clone()),

            Type::Module { row, .. } => row.find_private_type(),

            Type::Record { row, .. } => row.find_private_type(),

            Type::Tuple { elems, .. } => elems.iter().find_map(|t| t.find_private_type()),

            Type::App { public: false, .. } => Some(self.clone()),

            Type::App { args, .. } => args.iter().find_map(|t| t.find_private_type()),

            Type::RowCons { head, tail, .. } => head
                .find_private_type()
                .or_else(|| tail.find_private_type()),

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
    pub fn to_gleam_doc(&self, names: &mut HashMap<usize, String>, uid: &mut usize) -> Document {
        match self {
            TypeVar::Link { ref typ, .. } => typ.to_gleam_doc(names, uid),

            TypeVar::Unbound { id, .. } => TypeVar::Generic { id: *id }.to_gleam_doc(names, uid),

            TypeVar::Generic { id, .. } => match names.get(&id) {
                Some(n) => n.clone().to_doc(),
                None => {
                    let n = next_letter(uid);
                    names.insert(*id, n.clone());
                    n.to_doc()
                }
            },
        }
    }
}

// TODO: Handle more than 27 letters
fn next_letter(i: &mut usize) -> String {
    let c = ((*i as u8 + 97) as char).to_string();
    *i += 1;
    c
}

#[test]
fn letter_test() {
    let mut i = 0;
    assert_eq!("a", next_letter(&mut i));
    assert_eq!("b", next_letter(&mut i));
    assert_eq!("c", next_letter(&mut i));
}

fn args_to_gleam_doc(
    args: &[Type],
    names: &mut HashMap<usize, String>,
    uid: &mut usize,
) -> Document {
    args.iter()
        .map(|t| t.to_gleam_doc(names, uid).group())
        .intersperse(break_(",", ", "))
        .collect::<Vec<_>>()
        .to_doc()
        .nest(INDENT)
        .append(break_(",", ""))
        .group()
}

#[test]
fn to_gleam_doc_test() {
    let cases = [
        (
            Type::Const {
                module: "whatever".to_string(),
                name: "Int".to_string(),
                public: true,
            },
            "Int",
        ),
        (
            Type::App {
                module: "".to_string(),
                name: "Pair".to_string(),
                public: true,
                args: vec![
                    Type::Const {
                        module: "whatever".to_string(),
                        name: "Int".to_string(),
                        public: true,
                    },
                    Type::Const {
                        module: "whatever".to_string(),
                        name: "Bool".to_string(),
                        public: true,
                    },
                ],
            },
            "Pair(Int, Bool)",
        ),
        (
            Type::Fn {
                args: vec![
                    Type::Const {
                        module: "whatever".to_string(),
                        name: "Int".to_string(),
                        public: true,
                    },
                    Type::Const {
                        module: "whatever".to_string(),
                        name: "Bool".to_string(),
                        public: true,
                    },
                ],
                retrn: Box::new(Type::Const {
                    module: "whatever".to_string(),
                    name: "Bool".to_string(),
                    public: true,
                }),
            },
            "fn(Int, Bool) -> Bool",
        ),
        (
            Type::Var {
                typ: Rc::new(RefCell::new(TypeVar::Link {
                    typ: Box::new(Type::Const {
                        module: "whatever".to_string(),
                        name: "Int".to_string(),
                        public: true,
                    }),
                })),
            },
            "Int",
        ),
        (
            Type::Var {
                typ: Rc::new(RefCell::new(TypeVar::Unbound { level: 1, id: 2231 })),
            },
            "a",
        ),
        (
            Type::Fn {
                args: vec![Type::Var {
                    typ: Rc::new(RefCell::new(TypeVar::Unbound { level: 1, id: 78 })),
                }],
                retrn: Box::new(Type::Var {
                    typ: Rc::new(RefCell::new(TypeVar::Unbound { level: 1, id: 2 })),
                }),
            },
            "fn(a) -> b",
        ),
        (
            Type::Fn {
                args: vec![Type::Var {
                    typ: Rc::new(RefCell::new(TypeVar::Generic { id: 78 })),
                }],
                retrn: Box::new(Type::Var {
                    typ: Rc::new(RefCell::new(TypeVar::Generic { id: 2 })),
                }),
            },
            "fn(a) -> b",
        ),
    ];

    for (typ, s) in cases.into_iter() {
        assert_eq!(
            s.to_string(),
            typ.to_gleam_doc(&mut hashmap![], &mut 0).format(80)
        );
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeVar {
    Unbound { id: usize, level: usize },
    Link { typ: Box<Type> },
    Generic { id: usize },
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeConstructorInfo {
    public: bool,
    module: String,
    arity: usize,
}

#[derive(Debug, Clone)]
pub struct Env {
    uid: usize,
    variables: HashMap<String, (Scope<Type>, Type)>,
    type_constructors: HashMap<String, TypeConstructorInfo>,
}

impl Env {
    pub fn new() -> Self {
        let mut env = Self {
            uid: 0,
            type_constructors: hashmap![],
            variables: hashmap![],
        };

        env.insert_type_constructor(
            "Int".to_string(),
            TypeConstructorInfo {
                arity: 0,
                module: "".to_string(),
                public: true,
            },
        );

        env.insert_type_constructor(
            "Bool".to_string(),
            TypeConstructorInfo {
                arity: 0,
                module: "".to_string(),
                public: true,
            },
        );

        env.insert_type_constructor(
            "List".to_string(),
            TypeConstructorInfo {
                arity: 1,
                module: "".to_string(),
                public: true,
            },
        );

        env.insert_type_constructor(
            "Float".to_string(),
            TypeConstructorInfo {
                arity: 0,
                module: "".to_string(),
                public: true,
            },
        );

        env.insert_type_constructor(
            "String".to_string(),
            TypeConstructorInfo {
                arity: 0,
                module: "".to_string(),
                public: true,
            },
        );

        env.insert_type_constructor(
            "Result".to_string(),
            TypeConstructorInfo {
                arity: 2,
                module: "".to_string(),
                public: true,
            },
        );

        env.insert_variable("True".to_string(), Scope::Module { arity: 0 }, bool());
        env.insert_variable("False".to_string(), Scope::Module { arity: 0 }, bool());

        env.insert_variable(
            "+".to_string(),
            Scope::Local,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(int()),
            },
        );

        env.insert_variable(
            "-".to_string(),
            Scope::Local,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(int()),
            },
        );

        env.insert_variable(
            "*".to_string(),
            Scope::Local,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(int()),
            },
        );

        env.insert_variable(
            "/".to_string(),
            Scope::Local,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(int()),
            },
        );

        env.insert_variable(
            "+.".to_string(),
            Scope::Local,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(float()),
            },
        );

        env.insert_variable(
            "-.".to_string(),
            Scope::Local,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(float()),
            },
        );

        env.insert_variable(
            "*.".to_string(),
            Scope::Local,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(float()),
            },
        );

        env.insert_variable(
            "||".to_string(),
            Scope::Local,
            Type::Fn {
                args: vec![bool(), bool()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            "&&".to_string(),
            Scope::Local,
            Type::Fn {
                args: vec![bool(), bool()],
                retrn: Box::new(bool()),
            },
        );

        env.insert_variable(
            "%".to_string(),
            Scope::Local,
            Type::Fn {
                args: vec![int(), int()],
                retrn: Box::new(int()),
            },
        );

        env.insert_variable(
            "%.".to_string(),
            Scope::Local,
            Type::Fn {
                args: vec![float(), float()],
                retrn: Box::new(float()),
            },
        );

        env.insert_variable(
            "/.".to_string(),
            Scope::Local,
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
            Scope::Local,
            Type::Fn {
                args: vec![a, f],
                retrn: Box::new(b),
            },
        );

        let a = env.new_generic_var();
        env.insert_variable(
            "==".to_string(),
            Scope::Local,
            Type::Fn {
                args: vec![a.clone(), a],
                retrn: Box::new(bool()),
            },
        );

        let a = env.new_generic_var();
        env.insert_variable(
            "!=".to_string(),
            Scope::Local,
            Type::Fn {
                args: vec![a.clone(), a],
                retrn: Box::new(bool()),
            },
        );

        let result = |ok, error| Type::App {
            name: "Result".to_string(),
            module: "".to_string(),
            public: true,
            args: vec![ok, error],
        };

        let ok = env.new_generic_var();
        let error = env.new_generic_var();
        env.insert_variable(
            "Ok".to_string(),
            Scope::Local,
            Type::Fn {
                args: vec![ok.clone()],
                retrn: Box::new(result(ok, error)),
            },
        );

        let ok = env.new_generic_var();
        let error = env.new_generic_var();
        env.insert_variable(
            "Error".to_string(),
            Scope::Local,
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

    /// Record a variable in the current scope.
    ///
    pub fn insert_variable(&mut self, name: String, scope: Scope<Type>, typ: Type) {
        self.variables.insert(name, (scope, typ));
    }

    /// Lookup a variable in the current scope.
    ///
    pub fn get_variable(&self, name: &str) -> Option<&(Scope<Type>, Type)> {
        self.variables.get(name)
    }

    /// Record a type in the current scope.
    ///
    pub fn insert_type_constructor(&mut self, name: String, info: TypeConstructorInfo) {
        self.type_constructors.insert(name, info);
    }

    /// Lookup a type in the current scope.
    ///
    pub fn get_type_constructor(&self, name: &str) -> Option<&TypeConstructorInfo> {
        self.type_constructors.get(name)
    }

    /// Construct a Type from an AST Type annotation.
    ///
    /// Type variables are managed using a HashMap of names to types- this permits the
    /// same type vars being shared between multiple annotations (such as in the arguments
    /// of an external function declaration)
    ///
    pub fn type_from_ast(
        &mut self,
        ast: &ast::Type,
        vars: &mut HashMap<String, Type>,
        permit_new_vars: bool,
    ) -> Result<Type, TypeFromAstError> {
        match ast {
            ast::Type::Constructor { meta, name, args } => {
                let args = args
                    .iter()
                    .map(|t| self.type_from_ast(t, vars, permit_new_vars))
                    .collect::<Result<Vec<_>, _>>()?;
                let types = self.type_constructors.clone();
                let info =
                    self.get_type_constructor(name)
                        .ok_or(TypeFromAstError::UnknownType {
                            name: name.to_string(),
                            types,
                        })?;
                if args.len() != info.arity {
                    return Err(TypeFromAstError::IncorrectTypeArity {
                        meta: meta.clone(),
                        name: name.to_string(),
                        expected: info.arity,
                        given: args.len(),
                    });
                }
                match args.len() {
                    0 => Ok(Type::Const {
                        name: name.to_string(),
                        module: info.module.clone(),
                        public: info.public,
                    }),
                    _ => Ok(Type::App {
                        name: name.to_string(),
                        module: info.module.clone(),
                        public: info.public,
                        args,
                    }),
                }
            }

            ast::Type::Tuple { elems, .. } => {
                let elems = elems
                    .iter()
                    .map(|t| self.type_from_ast(t, vars, permit_new_vars))
                    .collect::<Result<_, _>>()?;
                Ok(Type::Tuple { elems })
            }

            ast::Type::Fn { args, retrn, .. } => {
                let args = args
                    .iter()
                    .map(|t| self.type_from_ast(t, vars, permit_new_vars))
                    .collect::<Result<_, _>>()?;
                let retrn = self.type_from_ast(retrn, vars, permit_new_vars)?;
                Ok(Type::Fn {
                    args,
                    retrn: Box::new(retrn),
                })
            }

            ast::Type::Var { name, .. } => match vars.get(name) {
                Some(var) => Ok(var.clone()),

                None => {
                    if permit_new_vars {
                        let var = self.new_generic_var();
                        vars.insert(name.to_string(), var.clone());
                        Ok(var)
                    } else {
                        // TODO: test that enum constructors using unknown vars in their
                        // definitions is not permitted.
                        unimplemented!()
                    }
                }
            },
        }
    }
}

pub enum TypeFromAstError {
    UnknownType {
        name: String,
        types: HashMap<String, TypeConstructorInfo>,
    },

    IncorrectTypeArity {
        meta: Meta,
        name: String,
        expected: usize,
        given: usize,
    },
}

#[derive(Debug, PartialEq)]
pub enum Error {
    UnknownVariable {
        meta: Meta,
        name: String,
        variables: HashMap<String, (Scope<Type>, Type)>,
    },

    UnknownType {
        meta: Meta,
        name: String,
        types: HashMap<String, TypeConstructorInfo>,
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
        meta: Meta,
        name: String,
    },

    PrivateTypeLeak {
        meta: Meta,
        leaked: Type,
    },
}

/// Crawl the AST, annotating each node with the inferred type or
/// returning an error.
///
pub fn infer_module(
    module: UntypedModule,
    modules: &std::collections::HashMap<String, Type>,
) -> Result<TypedModule, Error> {
    let mut env = Env::new();
    let mut fields = vec![];
    let module_name = &module.name;

    let statements = module
        .statements
        .into_iter()
        .map(|s| match s {
            Statement::Fn {
                meta,
                name,
                public,
                args,
                body,
            } => {
                let level = 1;

                // Ensure function has not already been defined in this module
                if let Some((Scope::Module { .. }, _)) = env.get_variable(&name) {
                    return Err(Error::DuplicateName { meta, name });
                };

                // Register a var for the function so that it can call itself recursively
                let rec = env.new_unbound_var(level + 1);
                env.insert_variable(
                    name.clone(),
                    Scope::Module { arity: args.len() },
                    rec.clone(),
                );

                // Infer the type
                let (args_types, body) = infer_fun(&args, body, level + 1, &mut env)?;
                let typ = Type::Fn {
                    args: args_types,
                    retrn: Box::new(body.typ().clone()),
                };

                // Assert that the inferred type matches the type of any recursive call
                unify(&rec, &typ).map_err(|e| convert_unify_error(e, &meta))?;

                // Insert the function into the environment
                let typ = generalise(typ, level);
                env.insert_variable(
                    name.clone(),
                    Scope::Module { arity: args.len() },
                    typ.clone(),
                );

                // Insert the function into the module's type
                if public {
                    if let Some(leaked) = typ.find_private_type() {
                        return Err(Error::PrivateTypeLeak {
                            meta: meta.clone(),
                            leaked,
                        });
                    }

                    fields.push((name.clone(), typ));
                }
                Ok(Statement::Fn {
                    meta,
                    name,
                    public,
                    args,
                    body,
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
                // TODO: DUPE: 63846
                let mut type_vars = hashmap![];
                let retrn_type = env
                    .type_from_ast(&retrn, &mut type_vars, true)
                    .map_err(|e| convert_type_from_ast_error(e, meta.clone()))?;
                let mut args_types = Vec::with_capacity(args.len());
                for arg in args.iter() {
                    let t = env
                        .type_from_ast(arg, &mut type_vars, true)
                        .map_err(|e| convert_type_from_ast_error(e, meta.clone()))?;
                    args_types.push(t)
                }
                let typ = Type::Fn {
                    args: args_types,
                    retrn: Box::new(retrn_type),
                };
                if public {
                    fields.push((name.clone(), typ.clone()));

                    if let Some(leaked) = typ.find_private_type() {
                        return Err(Error::PrivateTypeLeak {
                            meta: meta.clone(),
                            leaked,
                        });
                    }
                }
                env.insert_variable(name.clone(), Scope::Module { arity: args.len() }, typ);
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

            Statement::Test { meta, name, body } => {
                let vars = env.variables.clone();
                let body = infer(body, 2, &mut env)?;
                env.variables = vars;
                Ok(Statement::Test { meta, name, body })
            }

            Statement::Enum {
                meta,
                public,
                name,
                args,
                constructors,
            } => {
                // Register type
                env.insert_type_constructor(
                    name.clone(),
                    TypeConstructorInfo {
                        module: module_name.clone(),
                        public,
                        arity: args.len(),
                    },
                );
                // Build return type and collect type vars that can be used in constructors
                let mut type_vars = hashmap![];
                let ast = ast::Type::Constructor {
                    meta: meta.clone(),
                    name: name.clone(),
                    args: args
                        .iter()
                        .map(|arg| ast::Type::Var {
                            meta: meta.clone(),
                            name: arg.to_string(),
                        })
                        .collect(),
                };
                let retrn = env
                    .type_from_ast(&ast, &mut type_vars, true)
                    .map_err(|e| convert_type_from_ast_error(e, meta.clone()))?;
                // Check and register constructors
                for constructor in constructors.iter() {
                    let args_types = constructor
                        .args
                        .iter()
                        .map(|arg| {
                            env.type_from_ast(&arg, &mut type_vars, false)
                                .map_err(|e| convert_type_from_ast_error(e, meta.clone()))
                        })
                        .collect::<Result<Vec<_>, _>>()?;
                    // Insert constructor function into module scope
                    let typ = match constructor.args.len() {
                        0 => retrn.clone(),
                        _ => Type::Fn {
                            args: args_types,
                            retrn: Box::new(retrn.clone()),
                        },
                    };
                    if public {
                        fields.push((constructor.name.clone(), typ.clone()));
                    };
                    env.insert_variable(
                        constructor.name.clone(),
                        Scope::Module {
                            arity: constructor.args.len(),
                        },
                        typ,
                    );
                }
                Ok(Statement::Enum {
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
                // Register type
                env.insert_type_constructor(
                    name.clone(),
                    TypeConstructorInfo {
                        module: module_name.clone(),
                        public,
                        arity: args.len(),
                    },
                );
                // Check contained types are valid
                // TODO: DUPE: 63846
                let mut type_vars = hashmap![];
                for arg in args.iter() {
                    let var = ast::Type::Var {
                        meta: meta.clone(),
                        name: arg.to_string(),
                    };
                    env.type_from_ast(&var, &mut type_vars, true)
                        .map_err(|e| convert_type_from_ast_error(e, meta.clone()))?;
                }
                Ok(Statement::ExternalType {
                    meta,
                    public,
                    name,
                    args,
                })
            }

            Statement::Import { meta, module } => {
                let typ = modules.get(&module).unwrap(); // TODO: handle unknown
                env.insert_variable(
                    module.clone(),
                    Scope::Import {
                        module: module.clone(),
                    },
                    typ.clone(),
                );
                Ok(Statement::Import { meta, module })
            }
        })
        .collect::<Result<Vec<_>, _>>()?;

    let row = fields
        .into_iter()
        .fold(Type::RowNil, |tail, (label, head)| Type::RowCons {
            label,
            head: Box::new(head),
            tail: Box::new(tail),
        });

    Ok(Module {
        name: module.name,
        statements,
        typ: Type::Module { row: Box::new(row) },
    })
}

/// Crawl the AST, annotating each node with the inferred type or
/// returning an error.
///
pub fn infer(expr: UntypedExpr, level: usize, env: &mut Env) -> Result<TypedExpr, Error> {
    match expr {
        Expr::Int { meta, value, .. } => Ok(Expr::Int {
            meta,
            value,
            typ: int(),
        }),

        Expr::Float { meta, value, .. } => Ok(Expr::Float {
            meta,
            value,
            typ: float(),
        }),

        Expr::String { meta, value, .. } => Ok(Expr::String {
            meta,
            value,
            typ: string(),
        }),

        Expr::Nil { meta, .. } => Ok(Expr::Nil {
            meta,
            typ: list(env.new_unbound_var(level)),
        }),

        Expr::Seq {
            meta, first, then, ..
        } => {
            let first = infer(*first, level, env)?;
            let then = infer(*then, level, env)?;
            Ok(Expr::Seq {
                meta,
                typ: then.typ().clone(),
                first: Box::new(first),
                then: Box::new(then),
            })
        }

        Expr::Fn {
            meta,
            is_capture,
            args,
            body,
            ..
        } => {
            let (args_types, body) = infer_fun(&args, *body, level, env)?;
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

        Expr::Let {
            meta,
            pattern,
            value,
            then,
            ..
        } => {
            let value = infer(*value, level + 1, env)?;
            let value_typ = generalise(value.typ().clone(), level + 1);
            unify_pattern(&pattern, &value_typ, level, env)?;
            let then = infer(*then, level, env)?;
            let typ = then.typ().clone();
            Ok(Expr::Let {
                meta,
                typ,
                pattern,
                value: Box::new(value),
                then: Box::new(then),
            })
        }

        Expr::Case {
            meta,
            subject,
            clauses,
            ..
        } => {
            let return_type = env.new_unbound_var(level); // TODO: st'hould this be level + 1 ?
            let mut typed_clauses = Vec::with_capacity(clauses.len());
            let subject = infer(*subject, level + 1, env)?;
            let subject_type = generalise(subject.typ().clone(), level + 1);

            for clause in clauses.into_iter() {
                let vars = env.variables.clone();

                unify_pattern(&clause.pattern, &subject_type, level, env)?;

                let then = infer(*clause.then, level, env)?;
                unify(&return_type, then.typ()).map_err(|e| convert_unify_error(e, then.meta()))?;
                typed_clauses.push(Clause {
                    meta: clause.meta,
                    pattern: clause.pattern,
                    then: Box::new(then),
                });

                env.variables = vars;
            }
            Ok(Expr::Case {
                meta,
                typ: return_type,
                subject: Box::new(subject),
                clauses: typed_clauses,
            })
        }

        Expr::Cons {
            meta, head, tail, ..
        } => {
            let head = infer(*head, level, env)?;
            let tail = infer(*tail, level, env)?;
            unify(tail.typ(), &list(head.typ().clone()))
                .map_err(|e| convert_unify_error(e, &meta))?;
            Ok(Expr::Cons {
                meta,
                typ: tail.typ().clone(),
                head: Box::new(head),
                tail: Box::new(tail),
            })
        }

        Expr::Call {
            meta, fun, args, ..
        } => {
            let (fun, args, typ) = infer_call(*fun, args, level, &meta, env)?;
            Ok(Expr::Call {
                meta,
                typ,
                args,
                fun: Box::new(fun),
            })
        }

        Expr::Tuple { meta, elems, .. } => {
            let elems = elems
                .into_iter()
                .map(|e| infer(e, level, env))
                .collect::<Result<Vec<_>, _>>()?;
            let typ = Type::Tuple {
                elems: elems.iter().map(|e| e.typ().clone()).collect(),
            };
            Ok(Expr::Tuple { meta, elems, typ })
        }

        Expr::BinOp {
            meta,
            name,
            left,
            right,
            ..
        } => {
            let fun = Expr::Var {
                meta: meta.clone(),
                typ: (),
                scope: (),
                name: bin_op_name(&name),
            };
            let (_fun, mut args, typ) = infer_call(fun, vec![*left, *right], level, &meta, env)?;
            Ok(Expr::BinOp {
                meta,
                name,
                typ,
                right: Box::new(args.pop().unwrap()),
                left: Box::new(args.pop().unwrap()),
            })
        }

        Expr::RecordNil { meta, .. } => Ok(Expr::RecordNil {
            meta,
            typ: Type::Record {
                row: Box::new(Type::RowNil),
            },
        }),

        Expr::RecordCons {
            meta,
            tail,
            label,
            value,
            ..
        } => {
            let value = infer(*value, level, env)?;
            let tail = infer(*tail, level, env)?;

            let value_type = env.new_unbound_var(level);
            unify(&value_type, value.typ()).map_err(|e| convert_unify_error(e, &meta))?;

            let tail_row_type = env.new_unbound_var(level);
            unify(
                &Type::Record {
                    row: Box::new(tail_row_type.clone()),
                },
                tail.typ(),
            )
            .map_err(|e| convert_unify_error(e, &meta))?;

            let typ = Type::Record {
                row: Box::new(Type::RowCons {
                    label: label.clone(),
                    head: Box::new(value_type),
                    tail: Box::new(tail_row_type),
                }),
            };
            Ok(Expr::RecordCons {
                meta,
                typ,
                label,
                tail: Box::new(tail),
                value: Box::new(value),
            })
        }

        Expr::Var { meta, name, .. } => {
            let (scope, typ) = infer_var(&name, level, &meta, env)?;
            Ok(Expr::Var {
                meta,
                scope,
                typ,
                name,
            })
        }

        Expr::Constructor { meta, name, .. } => {
            let (_scope, typ) = infer_var(&name, level, &meta, env)?;
            Ok(Expr::Constructor { meta, typ, name })
        }

        Expr::RecordSelect {
            meta,
            label,
            record,
            ..
        } => {
            let record = infer(*record, level, env)?;

            // We unify the record with a dummy record in order to determine the type of the
            // selected field.
            let other_fields_typ = env.new_unbound_var(level);
            let selected_field_typ = env.new_unbound_var(level);
            let dummy_record = Type::Record {
                row: Box::new(Type::RowCons {
                    label: label.clone(),
                    head: Box::new(selected_field_typ.clone()),
                    tail: Box::new(other_fields_typ),
                }),
            };
            unify(&dummy_record, record.typ()).map_err(|e| convert_unify_error(e, &meta))?;

            Ok(Expr::RecordSelect {
                meta,
                label,
                record: Box::new(record),
                typ: selected_field_typ,
            })
        }

        Expr::ModuleSelect {
            meta,
            module,
            label,
            ..
        } => {
            let module = infer(*module, level, env)?;
            let typ = infer_module_select(module.typ(), &label, level, &meta, env)?;

            Ok(Expr::ModuleSelect {
                meta,
                label,
                module: Box::new(module),
                typ,
            })
        }
    }
}

fn infer_module_select(
    module_type: &Type,
    label: &str,
    level: usize,
    meta: &Meta,
    env: &mut Env,
) -> Result<Type, Error> {
    // We unify the module with a dummy module in order to determine the type of the
    // selected field.
    let other_fields_typ = env.new_unbound_var(level);
    let selected_field_typ = env.new_unbound_var(level);
    let dummy_module = Type::Module {
        row: Box::new(Type::RowCons {
            label: label.to_string(),
            head: Box::new(selected_field_typ.clone()),
            tail: Box::new(other_fields_typ),
        }),
    };
    unify(&dummy_module, module_type).map_err(|e| convert_unify_error(e, meta))?;

    Ok(selected_field_typ)
}

/// When we have an assignment or a case expression we unify the pattern with the
/// inferred type of the subject in order to determine what variables to insert
/// into the environment (or to detect a type error).
///
fn unify_pattern(pattern: &Pattern, typ: &Type, level: usize, env: &mut Env) -> Result<(), Error> {
    //
    // TODO: I think we might be unifying backwards for some of these.
    // The typ should be the `expected` and the `pattern` is the actual?
    // Or perhaps because it's a pattern there should be a different Error variant
    // so we can display a more specific error message.
    //
    match pattern {
        Pattern::Discard { .. } => Ok(()),

        Pattern::Var { name, .. } => {
            env.insert_variable(name.to_string(), Scope::Local, typ.clone());
            Ok(())
        }

        Pattern::Int { meta, .. } => unify(&int(), typ).map_err(|e| convert_unify_error(e, &meta)),

        Pattern::Float { meta, .. } => {
            unify(&float(), typ).map_err(|e| convert_unify_error(e, &meta))
        }

        Pattern::String { meta, .. } => {
            unify(&string(), typ).map_err(|e| convert_unify_error(e, &meta))
        }

        Pattern::Nil { meta, .. } => {
            unify(&list(env.new_unbound_var(level)), typ).map_err(|e| convert_unify_error(e, &meta))
        }

        Pattern::Cons {
            meta, head, tail, ..
        } => match typ.get_app_args(true, "", "List", 1, env) {
            Some(args) => {
                unify_pattern(head, &args[0], level, env)?;
                unify_pattern(tail, typ, level, env)
            }

            None => Err(Error::CouldNotUnify {
                given: list(env.new_unbound_var(level)),
                expected: typ.clone(),
                meta: meta.clone(),
            }),
        },

        Pattern::Constructor {
            meta,
            module,
            name,
            args: pattern_args,
        } => {
            match infer_possibly_namespaced_var(module, name, level, meta, env)?
                .1
                .collapse_links()
            {
                Type::Fn { args, retrn } => {
                    if args.len() == pattern_args.len() {
                        for (pattern, typ) in pattern_args.iter().zip(&args) {
                            unify_pattern(pattern, typ, level, env)?;
                        }
                        unify(&retrn, typ).map_err(|e| convert_unify_error(e, &meta))
                    } else {
                        unimplemented!()
                    }
                }

                c @ Type::Const { .. } => {
                    if pattern_args.is_empty() {
                        unify(&c, typ).map_err(|e| convert_unify_error(e, &meta))
                    } else {
                        // Error: singleton given args
                        unimplemented!()
                    }
                }

                typ => {
                    dbg!(typ);
                    unreachable!()
                }
            }
        }

        Pattern::Tuple { elems, .. } => match typ {
            Type::Tuple { elems: type_elems } => {
                for (pattern, typ) in elems.iter().zip(type_elems) {
                    unify_pattern(pattern, typ, level, env)?;
                }
                Ok(())
            }

            _ => unimplemented!(),
        },
        // Pattern::Record { .. } => unimplemented!(),
    }
}

fn infer_possibly_namespaced_var(
    module: &Option<String>,
    name: &str,
    level: usize,
    meta: &Meta,
    env: &mut Env,
) -> Result<(Scope<Type>, Type), Error> {
    match module {
        None => infer_var(name, level, meta, env),

        Some(module) => {
            let (_, typ) = infer_var(module, level, meta, env)?;
            let typ = infer_module_select(&typ, name, level, meta, env)?;
            Ok((Scope::Module { arity: 0 }, typ))
        }
    }
}

fn infer_var(
    name: &str,
    level: usize,
    meta: &Meta,
    env: &mut Env,
) -> Result<(Scope<Type>, Type), Error> {
    let (scope, typ) = env
        .get_variable(name)
        .cloned()
        .ok_or_else(|| Error::UnknownVariable {
            meta: meta.clone(),
            name: name.to_string(),
            variables: env.variables.clone(),
        })?;
    let typ = instantiate(typ, level, env);
    Ok((scope, typ))
}

fn infer_call(
    fun: UntypedExpr,
    args: Vec<UntypedExpr>,
    level: usize,
    meta: &Meta,
    env: &mut Env,
) -> Result<(TypedExpr, Vec<TypedExpr>, Type), Error> {
    let fun = infer(fun, level, env)?;
    // TODO: Use the meta of the arg instead of the meta of the entire call
    let (mut args_types, return_type) =
        match_fun_type(fun.typ(), args.len(), env).map_err(|e| convert_not_fun_error(e, &meta))?;
    let args = args_types
        .iter_mut()
        .zip(args)
        .map(|(typ, arg): (&mut Type, _)| {
            let arg = infer(arg, level, env)?;
            unify(typ, arg.typ()).map_err(|e| convert_unify_error(e, &meta))?;
            Ok(arg)
        })
        .collect::<Result<_, _>>()?;
    Ok((fun, args, return_type))
}

fn infer_fun(
    args: &[Arg],
    body: UntypedExpr,
    level: usize,
    env: &mut Env,
) -> Result<(Vec<Type>, TypedExpr), Error> {
    let args_types: Vec<_> = args.iter().map(|_| env.new_unbound_var(level)).collect();
    let vars = env.variables.clone();
    args.iter()
        .zip(args_types.iter())
        .for_each(|(arg, t)| match &arg.name {
            Some(name) => env.insert_variable(name.to_string(), Scope::Local, (*t).clone()),
            None => (),
        });
    let body = infer(body, level, env)?;
    env.variables = vars;
    Ok((args_types, body))
}

fn bin_op_name(name: &BinOp) -> String {
    match name {
        BinOp::Pipe => "|>".to_string(),
        BinOp::And => "&&".to_string(),
        BinOp::Or => "||".to_string(),
        BinOp::Lt => "<".to_string(),
        BinOp::LtEq => "<=".to_string(),
        BinOp::Eq => "==".to_string(),
        BinOp::NotEq => "!=".to_string(),
        BinOp::GtEq => ">=".to_string(),
        BinOp::Gt => ">".to_string(),
        BinOp::AddInt => "+".to_string(),
        BinOp::AddFloat => "+.".to_string(),
        BinOp::SubInt => "-".to_string(),
        BinOp::SubFloat => "-.".to_string(),
        BinOp::MultInt => "*".to_string(),
        BinOp::MultFloat => "*.".to_string(),
        BinOp::DivInt => "/".to_string(),
        BinOp::DivFloat => "/.".to_string(),
        BinOp::ModuloInt => "*".to_string(),
        BinOp::ModuloFloat => "*.".to_string(),
    }
}

fn convert_type_from_ast_error(e: TypeFromAstError, meta: Meta) -> Error {
    match e {
        TypeFromAstError::UnknownType { name, types } => Error::UnknownType { meta, name, types },

        TypeFromAstError::IncorrectTypeArity {
            meta,
            name,
            expected,
            given,
        } => Error::IncorrectTypeArity {
            meta,
            name,
            expected,
            given,
        },
    }
}

fn convert_unify_error(e: UnifyError, meta: &Meta) -> Error {
    match e {
        UnifyError::CouldNotUnify { expected, given } => Error::CouldNotUnify {
            meta: meta.clone(),
            expected,
            given,
        },

        UnifyError::RecursiveType => Error::RecursiveType { meta: meta.clone() },
    }
}

/// Instantiate converts generic variables into unbound ones.
///
fn instantiate(typ: Type, ctx_level: usize, env: &mut Env) -> Type {
    fn go(t: Type, ctx_level: usize, ids: &mut HashMap<usize, Type>, env: &mut Env) -> Type {
        match t {
            Type::Const { .. } => t,

            Type::App {
                public,
                name,
                module,
                args,
            } => Type::App {
                public,
                name,
                module,
                args: args
                    .into_iter()
                    .map(|t| go(t, ctx_level, ids, env))
                    .collect(),
            },

            Type::Var { typ } => match &*typ.borrow() {
                TypeVar::Link { typ } => go(*typ.clone(), ctx_level, ids, env),

                TypeVar::Unbound { .. } => Type::Var { typ: typ.clone() },

                TypeVar::Generic { id } => match ids.get(id) {
                    Some(t) => t.clone(),
                    None => {
                        let v = env.new_unbound_var(ctx_level);
                        ids.insert(*id, v.clone());
                        v
                    }
                },
            },

            Type::Tuple { elems } => Type::Tuple {
                elems: elems
                    .into_iter()
                    .map(|t| go(t, ctx_level, ids, env))
                    .collect(),
            },

            Type::Fn { args, retrn, .. } => {
                let args = args
                    .into_iter()
                    .map(|t| go(t, ctx_level, ids, env))
                    .collect();
                let retrn = Box::new(go(*retrn, ctx_level, ids, env));
                Type::Fn { args, retrn }
            }

            Type::Record { row } => Type::Record {
                row: Box::new(go(*row, ctx_level, ids, env)),
            },

            Type::Module { row } => Type::Module {
                row: Box::new(go(*row, ctx_level, ids, env)),
            },

            Type::RowCons { label, head, tail } => Type::RowCons {
                label,
                head: Box::new(go(*head, ctx_level, ids, env)),
                tail: Box::new(go(*tail, ctx_level, ids, env)),
            },

            Type::RowNil => t,
        }
    }

    go(typ, ctx_level, &mut hashmap![], env)
}

enum UnifyError {
    CouldNotUnify { expected: Type, given: Type },
    RecursiveType,
}

// let rec unify ty1 ty2 =
// 	if ty1 == ty2 then () else
// 	match (ty1, ty2) with
// 		| TConst name1, TConst name2 when name1 = name2 -> ()
// 		| TApp(ty1, ty_arg_list1), TApp(ty2, ty_arg_list2) ->
// 				unify ty1 ty2 ;
// 				List.iter2 unify ty_arg_list1 ty_arg_list2
// 		| TArrow(param_ty_list1, return_ty1), TArrow(param_ty_list2, return_ty2) ->
// 				List.iter2 unify param_ty_list1 param_ty_list2 ;
// 				unify return_ty1 return_ty2
// 		| TVar {contents = Link ty1}, ty2 | ty1, TVar {contents = Link ty2} -> unify ty1 ty2
// 		| TVar {contents = Unbound(id1, _)}, TVar {contents = Unbound(id2, _)} when id1 = id2 ->
// 				assert false (* There is only a single instance of a particular type variable. *)
// 		| TVar ({contents = Unbound(id, level)} as tvar), ty
// 		| ty, TVar ({contents = Unbound(id, level)} as tvar) ->
// 				occurs_check_adjust_levels id level ty ;
// 				tvar := Link ty
// 		| _, _ -> error ("cannot unify types " ^ string_of_ty ty1 ^ " and " ^ string_of_ty ty2)
fn unify(t1: &Type, t2: &Type) -> Result<(), UnifyError> {
    if t1 == t2 {
        return Ok(());
    }

    // Collapse right hand side type links. Left hand side will be collapsed in the next block.
    if let Type::Var { typ } = t2 {
        if let TypeVar::Link { typ } = &*typ.borrow() {
            return unify(t1, typ);
        }
    }

    if let Type::Var { typ } = t1 {
        enum Action {
            Unify(Type),
            Link(Type),
        }

        let action = match &*typ.borrow() {
            TypeVar::Link { typ } => Action::Unify((**typ).clone()),

            TypeVar::Unbound { id, level } => {
                update_levels(t2, *level, *id)?;
                Action::Link((*t2).clone())
            }

            TypeVar::Generic { .. } => unimplemented!(),
        };

        return match action {
            Action::Link(t) => {
                *typ.borrow_mut() = TypeVar::Link { typ: Box::new(t) };
                Ok(())
            }
            Action::Unify(t) => unify(&t, t2),
        };
    }

    if let Type::Var { .. } = t2 {
        return unify(t2, t1).map_err(flip_unify_error);
    }

    match (t1, t2) {
        (
            Type::Const {
                module: m1,
                name: n1,
                ..
            },
            Type::Const {
                module: m2,
                name: n2,
                ..
            },
        ) => {
            if n1 == n2 && m1 == m2 {
                Ok(())
            } else {
                Err(UnifyError::CouldNotUnify {
                    expected: (*t1).clone(),
                    given: (*t2).clone(),
                })
            }
        }

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
        ) => {
            if m1 == m2 && n1 == n2 && args1.len() == args2.len() {
                for (a, b) in args1.iter().zip(args2) {
                    unify(a, b)?;
                }
                Ok(())
            } else {
                unimplemented!()
            }
        }

        (Type::Tuple { elems: elems1, .. }, Type::Tuple { elems: elems2, .. }) => {
            if elems1.len() == elems2.len() {
                for (a, b) in elems1.iter().zip(elems2) {
                    unify(a, b)?;
                }
                Ok(())
            } else {
                unimplemented!()
            }
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
        ) => {
            if args1.len() == args2.len() {
                for (a, b) in args1.iter().zip(args2) {
                    unify(a, b)?;
                }
                unify(retrn1, retrn2)
            } else {
                unimplemented!()
            }
        }

        (Type::Record { row: row1 }, Type::Record { row: row2 }) => unify(row1, row2),

        (Type::Module { row: row1 }, Type::Module { row: row2 }) => unify(row1, row2),

        (Type::RowNil, Type::RowNil) => unimplemented!(),

        (
            Type::RowCons {
                label: label1,
                head: head1,
                tail: tail1,
            },
            Type::RowCons {
                label: label2,
                head: head2,
                tail: tail2,
            },
        ) => {
            let unbound = match &**tail1 {
                Type::Var { typ } => match &*typ.borrow() {
                    TypeVar::Unbound { .. } => Some(typ.clone()),
                    _ => None,
                },
                _ => None,
            };

            // TODO: If we use Rc for types then we clone the Rc not the type
            let t2 = Type::RowCons {
                label: (*label2).clone(),
                head: (*head2).clone(),
                tail: (*tail2).clone(),
            };
            let tail2 = rewrite_row(t2, label1.clone(), *head1.clone())?;

            if let Some(typ) = unbound {
                if let TypeVar::Link { .. } = &*typ.borrow() {
                    unimplemented!()
                }
            }
            unify(tail1, &tail2)
        }

        (_, _) => Err(UnifyError::CouldNotUnify {
            expected: (*t1).clone(),
            given: (*t2).clone(),
        }),
    }
}

// and rewrite_row row2 label1 field_ty1 = match row2 with
// 	| TRowEmpty -> error ("row does not contain label " ^ label1)
// 	| TRowExtend(label2, field_ty2, rest_row2) when label2 = label1 ->
// 			unify field_ty1 field_ty2 ;
// 			rest_row2
// 	| TRowExtend(label2, field_ty2, rest_row2) ->
// 			TRowExtend(label2, field_ty2, rewrite_row rest_row2 label1 field_ty1)
// 	| TVar {contents = Link row2} -> rewrite_row row2 label1 field_ty1
// 	| TVar ({contents = Unbound(id, level)} as tvar) ->
// 			let rest_row2 = new_var level in
// 			let ty2 = TRowExtend(label1, field_ty1, rest_row2) in
// 			tvar := Link ty2 ;
// 			rest_row2
// 	| _ -> error "row type expected"
fn rewrite_row(row: Type, label1: String, head1: Type) -> Result<Type, UnifyError> {
    match row {
        Type::RowNil => unimplemented!(),

        Type::RowCons { label, head, tail } => {
            if label == label1 {
                unify(&head1, &head)?;
                Ok(*tail)
            } else {
                let tail = rewrite_row(*tail, label1, head1)?;
                Ok(Type::RowCons {
                    label,
                    head,
                    tail: Box::new(tail),
                })
            }
        }

        Type::Var { typ } => match &*typ.borrow() {
            TypeVar::Unbound { .. } => unimplemented!(),

            TypeVar::Link { typ } => rewrite_row(*typ.clone(), label1, head1),

            _ => unimplemented!(),
        },

        _ => unimplemented!(),
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

// let occurs_check_adjust_levels tvar_id tvar_level ty =
// 	let rec f = function
// 		| TVar {contents = Link ty} -> f ty
// 		| TVar {contents = Generic _} -> assert false
// 		| TVar ({contents = Unbound(other_id, other_level)} as other_tvar) ->
// 				if other_id = tvar_id then
// 					error "recursive types"
// 				else
// 					if other_level > tvar_level then
// 						other_tvar := Unbound(other_id, tvar_level)
// 					else
// 						()
// 		| TApp(ty, ty_arg_list) ->
// 				f ty ;
// 				List.iter f ty_arg_list
// 		| TArrow(param_ty_list, return_ty) ->
// 				List.iter f param_ty_list ;
// 				f return_ty
// 		| TConst _ -> ()
// 	in
// 	f ty
/// This function makes sure that the type variable being unified
/// doesn't occur within the type it is being unified with. This
/// prevents the algorithm from inferring recursive types, which
/// could cause naively-implemented type checking to diverge.
/// While traversing the type tree, this function also takes care
/// of updating the levels of the type variables appearing within
/// the type, thus ensuring the type will be correctly generalized.
///
fn update_levels(typ: &Type, own_level: usize, own_id: usize) -> Result<(), UnifyError> {
    // TODO: move this into the match block
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

            TypeVar::Generic { .. } => {
                panic!("Generic type var should not be passed to update_levels")
            }
        };

        if let Some(t) = new_value {
            *typ.borrow_mut() = t;
        }
        return Ok(());
    }

    match typ {
        Type::Const { .. } => Ok(()),

        Type::App { args, .. } => {
            for arg in args.iter() {
                update_levels(arg, own_level, own_id)?
            }
            Ok(())
        }

        Type::Tuple { elems, .. } => {
            for elem in elems.iter() {
                update_levels(elem, own_level, own_id)?
            }
            Ok(())
        }

        Type::Fn { args, retrn } => {
            for arg in args.iter() {
                update_levels(arg, own_level, own_id)?;
            }
            update_levels(retrn, own_level, own_id)
        }

        Type::Record { row, .. } => update_levels(row, own_level, own_id),

        Type::Module { row, .. } => update_levels(row, own_level, own_id),

        Type::Var { .. } => unreachable!(),

        Type::RowCons { head, tail, .. } => {
            update_levels(head, own_level, own_id)?;
            update_levels(tail, own_level, own_id)
        }

        Type::RowNil { .. } => Ok(()),
    }
}

struct NotFnError {
    pub expected: usize,
    pub given: usize,
}

// let rec match_fun_ty num_params = function
// 	| TArrow(param_ty_list, return_ty) ->
// 			if List.length param_ty_list <> num_params then
// 				error "unexpected number of arguments"
// 			else
// 				param_ty_list, return_ty
// 	| TVar {contents = Link ty} -> match_fun_ty num_params ty
// 	| TVar ({contents = Unbound(id, level)} as tvar) ->
// 			let param_ty_list =
// 				let rec f = function
// 					| 0 -> []
// 					| n -> new_var level :: f (n - 1)
// 				in
// 				f num_params
// 			in
// 			let return_ty = new_var level in
// 			tvar := Link (TArrow(param_ty_list, return_ty)) ;
// 			param_ty_list, return_ty
// 	| _ -> error "expected a function"
fn match_fun_type(
    typ: &Type,
    arity: usize,
    env: &mut Env,
) -> Result<(Vec<Type>, Type), NotFnError> {
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
            Err(NotFnError {
                expected: args.len(),
                given: arity,
            })
        } else {
            Ok((args.clone(), (**retrn).clone()))
        };
    }

    unimplemented!()
}

fn convert_not_fun_error(e: NotFnError, meta: &Meta) -> Error {
    Error::IncorrectArity {
        meta: meta.clone(),
        expected: e.expected,
        given: e.given,
    }
}

// let rec generalize level = function
// 	| TVar {contents = Unbound(id, other_level)} when other_level > level ->
// 			TVar (ref (Generic id))
// 	| TApp(ty, ty_arg_list) ->
// 			TApp(generalize level ty, List.map (generalize level) ty_arg_list)
// 	| TArrow(param_ty_list, return_ty) ->
// 			TArrow(List.map (generalize level) param_ty_list, generalize level return_ty)
// 	| TVar {contents = Link ty} -> generalize level ty
// 	| TVar {contents = Generic _} | TVar {contents = Unbound _} | TConst _ as ty -> ty
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

        Type::Const { .. } => t,

        Type::Record { row } => Type::Record {
            row: Box::new(generalise(*row, ctx_level)),
        },

        Type::Module { .. } => unimplemented!(),

        Type::RowCons { label, head, tail } => Type::RowCons {
            label,
            head: Box::new(generalise(*head, ctx_level)),
            tail: Box::new(generalise(*tail, ctx_level)),
        },

        Type::RowNil => t,
    }
}

pub fn int() -> Type {
    Type::Const {
        public: true,
        name: "Int".to_string(),
        module: "".to_string(),
    }
}

pub fn float() -> Type {
    Type::Const {
        public: true,
        name: "Float".to_string(),
        module: "".to_string(),
    }
}

pub fn bool() -> Type {
    Type::Const {
        public: true,
        name: "Bool".to_string(),
        module: "".to_string(),
    }
}

pub fn string() -> Type {
    Type::Const {
        public: true,
        name: "String".to_string(),
        module: "".to_string(),
    }
}

pub fn list(t: Type) -> Type {
    Type::App {
        public: true,
        name: "List".to_string(),
        module: "".to_string(),
        args: vec![t],
    }
}

pub fn record_nil() -> Type {
    Type::Record {
        row: Box::new(Type::RowNil),
    }
}

#[test]
fn infer_test() {
    struct Case {
        src: &'static str,
        typ: &'static str,
    }
    let cases = [
        Case {
            src: "True",
            typ: "Bool",
        },
        Case {
            src: "False",
            typ: "Bool",
        },
        Case {
            src: "1",
            typ: "Int",
        },
        Case {
            src: "-2",
            typ: "Int",
        },
        Case {
            src: "1.0",
            typ: "Float",
        },
        Case {
            src: "-8.0",
            typ: "Float",
        },
        Case {
            src: "\"ok\"",
            typ: "String",
        },
        Case {
            src: "\"ok\"",
            typ: "String",
        },
        Case {
            src: "1 2.0",
            typ: "Float",
        },
        Case {
            src: "[]",
            typ: "List(a)",
        },
        /* Assignments

        */
        Case {
            src: "let x = 1 2",
            typ: "Int",
        },
        Case {
            src: "let x = 1 x",
            typ: "Int",
        },
        Case {
            src: "let x = 2.0 x",
            typ: "Float",
        },
        Case {
            src: "let x = 2 let y = x y",
            typ: "Int",
        },
        /* Lists

        */
        Case {
            src: "[]",
            typ: "List(a)",
        },
        Case {
            src: "[1]",
            typ: "List(Int)",
        },
        Case {
            src: "[1, 2, 3]",
            typ: "List(Int)",
        },
        Case {
            src: "[[]]",
            typ: "List(List(a))",
        },
        Case {
            src: "[[1.0, 2.0]]",
            typ: "List(List(Float))",
        },
        Case {
            src: "[fn(x) { x }]",
            typ: "List(fn(a) -> a)",
        },
        Case {
            src: "[fn(x) { x + 1 }]",
            typ: "List(fn(Int) -> Int)",
        },
        Case {
            src: "[{[], []}]",
            typ: "List({List(a), List(b)})",
        },
        Case {
            src: "[fn(x) { x }, fn(x) { x + 1 }]",
            typ: "List(fn(Int) -> Int)",
        },
        Case {
            src: "[fn(x) { x + 1 }, fn(x) { x }]",
            typ: "List(fn(Int) -> Int)",
        },
        Case {
            src: "[[], []]",
            typ: "List(List(a))",
        },
        Case {
            src: "[[], [1]]",
            typ: "List(List(Int))",
        },
        Case {
            src: "[1 | [2 | []]]",
            typ: "List(Int)",
        },
        Case {
            src: "[fn(x) { x } | []]",
            typ: "List(fn(a) -> a)",
        },
        Case {
            src: "let f = fn(x) { x } [f, f]",
            typ: "List(fn(a) -> a)",
        },
        Case {
            src: "let x = [1 | []] [2 | x]",
            typ: "List(Int)",
        },
        /* Tuples

        */
        Case {
            src: "{1}",
            typ: "{Int}",
        },
        Case {
            src: "{1, 2.0}",
            typ: "{Int, Float}",
        },
        Case {
            src: "{1, 2.0, 3}",
            typ: "{Int, Float, Int}",
        },
        Case {
            src: "{1, 2.0, {1, 1}}",
            typ: "{Int, Float, {Int, Int}}",
        },
        /* Fns

        */
        Case {
            src: "fn(x) { x }",
            typ: "fn(a) -> a",
        },
        Case {
            src: "fn(x) { x }",
            typ: "fn(a) -> a",
        },
        Case {
            src: "fn(x, y) { x }",
            typ: "fn(a, b) -> a",
        },
        Case {
            src: "fn(x, y) { [] }",
            typ: "fn(a, b) -> List(c)",
        },
        Case {
            src: "let x = 1.0 1",
            typ: "Int",
        },
        Case {
            src: "let id = fn(x) { x } id(1)",
            typ: "Int",
        },
        Case {
            src: "let x = fn() { 1.0 } x()",
            typ: "Float",
        },
        Case {
            src: "fn(x) { x }(1)",
            typ: "Int",
        },
        Case {
            src: "fn() { 1 }",
            typ: "fn() -> Int",
        },
        Case {
            src: "fn() { 1.1 }",
            typ: "fn() -> Float",
        },
        Case {
            src: "fn(x) { 1.1 }",
            typ: "fn(a) -> Float",
        },
        Case {
            src: "fn(x) { x }",
            typ: "fn(a) -> a",
        },
        Case {
            src: "let x = fn(x) { 1.1 } x",
            typ: "fn(a) -> Float",
        },
        Case {
            src: "fn(x, y, z) { 1 }",
            typ: "fn(a, b, c) -> Int",
        },
        Case {
            src: "fn(x) { let y = x y }",
            typ: "fn(a) -> a",
        },
        Case {
            src: "fn(x) { {1, x} }",
            typ: "fn(a) -> {Int, a}",
        },
        Case {
            src: "let id = fn(x) { x } id(1)",
            typ: "Int",
        },
        Case {
            src: "let const = fn(x) { fn(y) { x } } let one = const(1) one(2.0)",
            typ: "Int",
        },
        Case {
            src: "fn(f) { f(1) }",
            typ: "fn(fn(Int) -> a) -> a",
        },
        Case {
            src: "fn(f, x) { f(x) }",
            typ: "fn(fn(a) -> b, a) -> b",
        },
        Case {
            src: "fn(f) { fn(x) { f(x) } }",
            typ: "fn(fn(a) -> b) -> fn(a) -> b",
        },
        Case {
            src: "fn(f) { fn(x) { fn(y) { f(x, y) } } }",
            typ: "fn(fn(a, b) -> c) -> fn(a) -> fn(b) -> c",
        },
        Case {
            src: "fn(f) { fn(x, y) { f(x)(y) } }",
            typ: "fn(fn(a) -> fn(b) -> c) -> fn(a, b) -> c",
        },
        Case {
            src: "fn(f) { fn(x) { let ff = f ff(x) } }",
            typ: "fn(fn(a) -> b) -> fn(a) -> b",
        },
        Case {
            src: "fn(f) { fn(x, y) { let ff = f(x) ff(y) } }",
            typ: "fn(fn(a) -> fn(b) -> c) -> fn(a, b) -> c",
        },
        Case {
            src: "fn(x) { fn(y) { x } }",
            typ: "fn(a) -> fn(b) -> a",
        },
        Case {
            src: "fn(f) { f() }",
            typ: "fn(fn() -> a) -> a",
        },
        Case {
            src: "fn(f, x) { f(f(x)) }",
            typ: "fn(fn(a) -> a, a) -> a",
        },
        Case {
            src: "fn(x, y) { {x, y} }",
            typ: "fn(a, b) -> {a, b}",
        },
        Case {
            src: "fn(x) { {x, x} }",
            typ: "fn(a) -> {a, a}",
        },
        Case {
            src: "let id = fn(a) { a } fn(x) { x(id) }",
            typ: "fn(fn(fn(a) -> a) -> b) -> b",
        },
        Case {
            src: "let add = fn(x, y) { x + y } add(_, 2)",
            typ: "fn(Int) -> Int",
        },
        /* Records

        */
        Case {
            src: "{}",
            typ: "{}",
        },
        Case {
            src: "{{} | a = 1}",
            typ: "{ a = Int }",
        },
        Case {
            src: "{a = 1}",
            typ: "{ a = Int }",
        },
        Case {
            src: "{a = 1, b = 2}",
            typ: "{ a = Int, b = Int }",
        },
        Case {
            src: "{a = 1, b = 2.0, c = -1}",
            typ: "{ a = Int, b = Float, c = Int }",
        },
        Case {
            src: "{a = {a = 1}}",
            typ: "{ a = { a = Int } }",
        },
        Case {
            src: "{} == {}",
            typ: "Bool",
        },
        Case {
            src: "{a = 1} == {a = 2}",
            typ: "Bool",
        },
        Case {
            src: "{a = 1, b = 1} == {a = 1, b = 1}",
            typ: "Bool",
        },
        Case {
            src: "{a = fn(x) { x }} == {a = fn(a) { a }}",
            typ: "Bool",
        },
        Case {
            src: "{b = 1, a = 1} == {a = 1, b = 1}",
            typ: "Bool",
        },
        Case {
            src: "{{a = 1.0} | a = 1}",
            typ: "{ a = Int }",
        },
        Case {
            src: "let a = {} {a | b = 1}",
            typ: "{ b = Int }",
        },
        Case {
            src: "fn(r) { { r | x = 1 } }",
            typ: "fn({ a |  }) -> { a | x = Int }",
        },
        Case {
            src: "{{} | a = 1}",
            typ: "{ a = Int }",
        },
        /* case

        */
        Case {
            src: "case 1 { | a -> 1 }",
            typ: "Int",
        },
        Case {
            src: "case 1 { | a -> 1.0 | b -> 2.0 | c -> 3.0 }",
            typ: "Float",
        },
        Case {
            src: "case 1 { | a -> a }",
            typ: "Int",
        },
        Case {
            src: "case 1 { | 1 -> 10 | 2 -> 20 | x -> x * 10 }",
            typ: "Int",
        },
        Case {
            src: "case 2.0 { | 2.0 -> 1 | x -> 0 }",
            typ: "Int",
        },
        Case {
            src: r#"case "ok" { | "ko" -> 1 | x -> 0 }"#,
            typ: "Int",
        },
        /* let

        */
        Case {
            src: "let {tag, x} = {1.0, 1} x",
            typ: "Int",
        },
        Case {
            src: "let [] = [] 1",
            typ: "Int",
        },
        Case {
            src: "let [a] = [1] a",
            typ: "Int",
        },
        Case {
            src: "let [a, 2] = [1] a",
            typ: "Int",
        },
        Case {
            src: "let [a | [b | []]] = [1] a",
            typ: "Int",
        },
        Case {
            src: "fn(x) { let [a] = x a }",
            typ: "fn(List(a)) -> a",
        },
        Case {
            src: "fn(x) { let [a] = x a + 1 }",
            typ: "fn(List(Int)) -> Int",
        },
        Case {
            src: "let _x = 1 2.0",
            typ: "Float",
        },
        Case {
            src: "let _ = 1 2.0",
            typ: "Float",
        },
        /* Record select

        */
        Case {
            src: "{a = 1, b = 2.0}.b",
            typ: "Float",
        },
        Case {
            src: "let r = {a = 1, b = 2} r.a",
            typ: "Int",
        },
        Case {
            src: "let r = {a = 1, b = 2} r.a + r.b",
            typ: "Int",
        },
        Case {
            src: "fn(x) { x.t }",
            typ: "fn({ b | t = a }) -> a",
        },
        Case {
            src: "let f = fn(x) { x } let r = {a = f} r.a",
            typ: "fn(a) -> a",
        },
        Case {
            src: "let r = {a = fn(x) { x }} r.a",
            typ: "fn(a) -> a",
        },
        Case {
            src: "let a = {b = 1} {a | b = 1.0}.b",
            typ: "Float",
        },
        Case {
            src: "let r = {a = fn(x) { x }, b = fn(x) { x }} [r.a, r.b]",
            typ: "List(fn(a) -> a)",
        },
        Case {
            src: "let f = fn(x) { x.t } f({ t = 1 })",
            typ: "Int",
        },
        Case {
            src: "let f = fn(x) { x.t(1) } f({t = fn(x) { x + 1 }})",
            typ: "Int",
        },
        Case {
            src: "{a = 1, b = 2.0}.a",
            typ: "Int",
        },
        Case {
            src: "fn(r) { r.x }",
            typ: "fn({ b | x = a }) -> a",
        },
        Case {
            src: "fn(r) { r.x + 1 }",
            typ: "fn({ a | x = Int }) -> Int",
        },
        /* Module select

        */
        Case {
            src: "fn(x) { x:run() }",
            typ: "fn(module { b | fn run() -> a }) -> a",
        },
        Case {
            src: "fn(x) { x:go }",
            typ: "fn(module { b | const go: a }) -> a",
        },
    ];

    for Case { src, typ } in cases.into_iter() {
        let ast = crate::grammar::ExprParser::new()
            .parse(src)
            .expect("syntax error");
        let result = infer(ast, 1, &mut Env::new()).expect("should successfully infer");
        assert_eq!(
            (
                src,
                result
                    .typ()
                    .to_gleam_doc(&mut hashmap![], &mut 0)
                    .format(80)
            ),
            (src, typ.to_string()),
        );
    }
}

#[test]
fn infer_error_test() {
    struct Case {
        src: &'static str,
        error: Error,
    }

    let cases = [
        Case {
            src: "1 + 1.0",
            error: Error::CouldNotUnify {
                meta: Meta { start: 0, end: 7 },
                expected: int(),
                given: float(),
            },
        },
        Case {
            src: "1 +. 1.0",
            error: Error::CouldNotUnify {
                meta: Meta { start: 0, end: 8 },
                expected: float(),
                given: int(),
            },
        },
        Case {
            src: "1 == 1.0",
            error: Error::CouldNotUnify {
                meta: Meta { start: 0, end: 8 },
                expected: int(),
                given: float(),
            },
        },
        Case {
            src: "x",
            error: Error::UnknownVariable {
                meta: Meta { start: 0, end: 1 },
                name: "x".to_string(),
                variables: Env::new().variables,
            },
        },
        Case {
            src: "x",
            error: Error::UnknownVariable {
                meta: Meta { start: 0, end: 1 },
                name: "x".to_string(),
                variables: Env::new().variables,
            },
        },
        Case {
            src: "let id = fn(x) { x } id()",
            error: Error::IncorrectArity {
                meta: Meta { start: 21, end: 25 },
                expected: 1,
                given: 0,
            },
        },
        Case {
            src: "let id = fn(x) { x } id(1, 2)",
            error: Error::IncorrectArity {
                meta: Meta { start: 21, end: 29 },
                expected: 1,
                given: 2,
            },
        },
        Case {
            src: "case 1 { | a -> 1 | b -> 2.0 }",
            error: Error::CouldNotUnify {
                meta: Meta { start: 25, end: 28 },
                expected: int(),
                given: float(),
            },
        },
        Case {
            src: "case 1.0 { | 1 -> 1 }",
            error: Error::CouldNotUnify {
                meta: Meta { start: 13, end: 14 },
                expected: int(),
                given: float(),
            },
        },
        Case {
            src: "case 1 { | 1.0 -> 1 }",
            error: Error::CouldNotUnify {
                meta: Meta { start: 11, end: 14 },
                expected: float(),
                given: int(),
            },
        },
        Case {
            src: "case 1 { | x -> 1 | 1 -> x }",
            error: Error::UnknownVariable {
                meta: Meta { start: 25, end: 26 },
                name: "x".to_string(),
                variables: Env::new().variables,
            },
        },
        Case {
            src: "let id = fn(x) { x(x) } 1",
            error: Error::RecursiveType {
                meta: Meta { start: 17, end: 21 },
            },
        },
    ];

    for Case { src, error } in cases.into_iter() {
        let ast = crate::grammar::ExprParser::new()
            .parse(src)
            .expect("syntax error");
        let result = infer(ast, 1, &mut Env::new()).expect_err("should infer an error");
        assert_eq!((src, &result), (src, error));
    }
}

#[test]
fn infer_module_test() {
    struct Case {
        src: &'static str,
        typ: &'static str,
    }

    let cases = [
        Case {
            src: "
        pub fn repeat(i, x) {
          case i {
          | 0 -> []
          | i -> [x | repeat(i - 1, x)]
          }
        }",
            typ: "module { fn repeat(Int, a) -> List(a) }",
        },
        Case {
            src: "fn private() { 1 }
                  pub fn public() { 1 }",
            typ: "module { fn public() -> Int }",
        },
        Case {
            src: "fn empty() { {} }
                  pub fn run() { { empty() | level = 1 } }",
            typ: "module { fn run() -> { level = Int } }",
        },
        Case {
            src: "pub fn ok(x) { {1, x} }",
            typ: "module { fn ok(a) -> {Int, a} }",
        },
        Case {
            src: "pub fn empty() {
                    let record = {}
                    record
                  }",
            typ: "module { fn empty() -> {} }",
        },
        Case {
            src: "pub fn add_name(record, name) {
                    let record = { record | name = name }
                    record
                  }",
            typ: "module { fn add_name({ a |  }, b) -> { a | name = b } }",
        },
        Case {
            src: "
                pub enum Is = | Yes | No
                pub fn yes() { Yes }
                pub fn no() { No }",
            typ: "module {
  const No: Is
  const Yes: Is
  fn no() -> Is
  fn yes() -> Is
}",
        },
        Case {
            src: "
                pub enum Num = | I(Int)
                pub fn one() { I(1) }",
            typ: "module {
  fn I(Int) -> Num
  fn one() -> Num
}",
        },
        Case {
            src: "
                pub fn id(x) { x }
                pub fn float() { id(1.0) }
                pub fn int() { id(1) }",
            typ: "module {
  fn float() -> Float
  fn id(a) -> a
  fn int() -> Int
}",
        },
        Case {
            src: "
        pub enum Box(a) = | Box(a)
        pub fn int() { Box(1) }
        pub fn float() { Box(1.0) }",
            typ: "module {
  fn Box(a) -> Box(a)
  fn float() -> Box(Float)
  fn int() -> Box(Int)
}",
        },
        Case {
            src: "
        pub enum Singleton = | Singleton
        pub fn go(x) { let Singleton = x 1 }",
            typ: "module {
  const Singleton: Singleton
  fn go(Singleton) -> Int
}",
        },
        Case {
            src: "
        pub enum Box(a) = | Box(a)
        pub fn unbox(x) { let Box(a) = x a }",
            typ: "module {
  fn Box(a) -> Box(a)
  fn unbox(Box(b)) -> b
}",
        },
        Case {
            src: "
        pub enum I = | I(Int)
        pub fn open(x) { case x { | I(i) -> i  } }",
            typ: "module {
  fn I(Int) -> I
  fn open(I) -> Int
}",
        },
        Case {
            src: "pub fn status() { 1 }
                  pub fn list_of(x) { [x] }
                  pub fn get_age(person) { person.age }
                  test whatever { 1 }",
            typ: "module {
  fn get_age({ b | age = a }) -> a
  fn list_of(c) -> List(c)
  fn status() -> Int
}",
        },
        Case {
            src: "pub external fn go(String) -> String = \"\" \"\"",
            typ: "module { fn go(String) -> String }",
        },
        Case {
            src: "pub external fn go(Int) -> Float = \"\" \"\"",
            typ: "module { fn go(Int) -> Float }",
        },
        Case {
            src: "pub external fn go(Int) -> Int = \"\" \"\"",
            typ: "module { fn go(Int) -> Int }",
        },
        Case {
            src: "external fn go(Int) -> Int = \"\" \"\"",
            typ: "module {  }",
        },
        Case {
            src: "pub external fn ok(Int) -> {Int, Int} = \"\" \"\"",
            typ: "module { fn ok(Int) -> {Int, Int} }",
        },
        Case {
            src: "pub external fn ok() -> fn(Int) -> Int = \"\" \"\"",
            typ: "module { fn ok() -> fn(Int) -> Int }",
        },
        Case {
            src: "pub external fn go(Int) -> b = \"\" \"\"",
            typ: "module { fn go(Int) -> a }",
        },
        Case {
            src: "pub external fn go(Bool) -> b = \"\" \"\"",
            typ: "module { fn go(Bool) -> a }",
        },
        Case {
            src: "pub external fn go(List(a)) -> a = \"\" \"\"",
            typ: "module { fn go(List(a)) -> a }",
        },
        Case {
            src: "pub external fn go({a, c}) -> c = \"\" \"\"",
            typ: "module { fn go({a, b}) -> b }",
        },
        Case {
            src: "
        external fn go(Int) -> b = \"\" \"\"
        pub fn x() {
          go(1)
        }",
            typ: "module { fn x() -> a }",
        },
        Case {
            src: "
        external fn id(a) -> a = \"\" \"\"
        pub fn i(x) { id(x) }
        pub fn a() { id(1) }
        pub fn b() { id(1.0) }",
            typ: "module {
  fn a() -> Int
  fn b() -> Float
  fn i(a) -> a
}",
        },
        Case {
            src: "pub external fn len(List(a)) -> Int = \"\" \"\"",
            typ: "module { fn len(List(a)) -> Int }",
        },
        Case {
            src: "
        pub external type Connection\n
        pub external fn is_open(Connection) -> Bool = \"\" \"\"",
            typ: "module { fn is_open(Connection) -> Bool }",
        },
        Case {
            src: "
        pub external type Pair(thing, thing)\n
        pub external fn pair(a) -> Pair(a, a) = \"\" \"\"",
            typ: "module { fn pair(a) -> Pair(a, a) }",
        },
        Case {
            src: "
pub fn one() { 1 }
pub fn zero() { one() - 1 }
pub fn two() { one() + zero() }",
            typ: "module {
  fn one() -> Int
  fn two() -> Int
  fn zero() -> Int
}",
        },
        Case {
            src: "
        pub fn one() { 1 }
        pub fn zero() { one() - 1 }
        pub fn two() { one() + zero() }",
            typ: "module {
  fn one() -> Int
  fn two() -> Int
  fn zero() -> Int
}",
        },
        Case {
            src: "test hello { 1 + 1 }",
            typ: "module {  }",
        },
        // // Type annotations
        // Case {
        //     src: "
        // type Html = String
        // pub fn go() { 1 }",
        //     typ: "module { fn go() -> Int }",
        // },
        // Case {
        //     src: "pub fn go(x: Int) { x }",
        //     typ: "module { fn go(Int) -> Int }",
        // },
        // Case {
        //     src: "pub fn go(x: List(a)) { x }",
        //     typ: "module { fn go(List(a)) -> List(a) }",
        // },
        // Case {
        //     src: "pub fn go(x: List(String)) { x }",
        //     typ: "module { fn go(List(String)) -> List(String) }",
        // },
        // Case {
        //     src: "pub fn go(x: b, y: c) { x }",
        //     typ: "module { fn go(a, b) -> a }",
        // },
        // Case {
        //     src: "pub fn go(x: Int) { x + 1 }",
        //     typ: "module { fn go(Int) -> Int }",
        // },
        Case {
            src: "pub fn length(list) {
                    case list {
                    | [] -> 0
                    | [x | xs] -> length(xs) + 1
                    }
                  }",
            typ: "module { fn length(List(a)) -> Int }",
        },
        // % TODO: mutual recursion
        //    // % {
        //    // %pub fn length(list) {\n
        //    // %  case list {\n
        //    // %  | [] -> 0\n
        //    // %  | _ :: tail -> helper_length(tail) + 1\n
        //    // %  }\n
        //    // %}
        //    // %fn helper_length(list) { length(list) }
        //    // %   ,
        //    // %module {
        //    // % fn length(List(a)) -> Int
        //    // %}
        //    // % }
    ];

    for Case { src, typ } in cases.into_iter() {
        let ast = crate::grammar::ModuleParser::new()
            .parse(src)
            .expect("syntax error");
        let result = infer_module(ast, &std::collections::HashMap::new())
            .expect("should successfully infer");
        assert_eq!(
            (
                src,
                result.typ.to_gleam_doc(&mut hashmap![], &mut 0).format(80)
            ),
            (src, typ.to_string()),
        );
    }
}

#[test]
fn infer_module_error_test() {
    struct Case {
        src: &'static str,
        error: Error,
    }

    let cases = [
        Case {
            src: "test go { 1 + 2.0 }",
            error: Error::CouldNotUnify {
                meta: Meta { start: 10, end: 17 }, // TODO: FIXME: This should specify just the RHS
                expected: int(),
                given: float(),
            },
        },
        Case {
            src: "external fn go(List(a, b)) -> a = \"\" \"\"",
            error: Error::IncorrectTypeArity {
                meta: Meta { start: 15, end: 25 },
                name: "List".to_string(),
                expected: 1,
                given: 2,
            },
        },
        Case {
            src: "fn dupe() { 1 }
                  fn dupe() { 2 }",
            error: Error::DuplicateName {
                meta: Meta { start: 34, end: 49 },
                name: "dupe".to_string(),
            },
        },
        Case {
            src: "fn dupe() { 1 }
                  fn dupe(x) { x }",
            error: Error::DuplicateName {
                meta: Meta { start: 34, end: 50 },
                name: "dupe".to_string(),
            },
        },
        Case {
            src: r#"external type PrivateType
                    pub external fn leak_type() -> PrivateType = "" """#,
            error: Error::PrivateTypeLeak {
                meta: Meta { start: 46, end: 96 },
                leaked: Type::Const {
                    public: false,
                    module: "".to_string(),
                    name: "PrivateType".to_string(),
                },
            },
        },
        Case {
            src: r#"external type PrivateType
                    external fn go() -> PrivateType = "" ""
                    pub fn leak_type() { go() }"#,
            error: Error::PrivateTypeLeak {
                meta: Meta {
                    start: 106,
                    end: 133,
                },
                leaked: Type::Const {
                    public: false,
                    module: "".to_string(),
                    name: "PrivateType".to_string(),
                },
            },
        },
        Case {
            src: r#"external type PrivateType
                    external fn go() -> PrivateType = "" ""
                    pub fn leak_type() { [go()] }"#,
            error: Error::PrivateTypeLeak {
                meta: Meta {
                    start: 106,
                    end: 135,
                },
                leaked: Type::Const {
                    public: false,
                    module: "".to_string(),
                    name: "PrivateType".to_string(),
                },
            },
        },
        Case {
            src: r#"external type PrivateType
                    pub external fn go(PrivateType) -> Int = "" """#,
            error: Error::PrivateTypeLeak {
                meta: Meta { start: 46, end: 92 },
                leaked: Type::Const {
                    public: false,
                    module: "".to_string(),
                    name: "PrivateType".to_string(),
                },
            },
        },
    ];

    for Case { src, error } in cases.into_iter() {
        let ast = crate::grammar::ModuleParser::new()
            .parse(src)
            .expect("syntax error");
        let result = infer_module(ast, &std::collections::HashMap::new())
            .expect_err("should infer an error");
        assert_eq!((src, error), (src, &result));
    }
}
