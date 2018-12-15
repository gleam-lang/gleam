#![allow(dead_code)] // TODO

use crate::ast::{Expr, Meta, Pattern};
use crate::grammar;
use crate::pretty::*;
use im::hashmap::HashMap;
use itertools::Itertools;
use std::cell::RefCell;
use std::rc::Rc;

const INDENT: isize = 2;

#[derive(Debug, Clone)]
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

    Fun {
        args: Vec<Type>,
        retrn: Box<Type>,
    },

    Record {
        row: Row,
    },

    Module {
        row: Row,
    },

    Var {
        typ: Rc<RefCell<TypeVar>>,
    },
}

impl Type {
    pub fn to_gleam_doc(&self, uid: &mut usize) -> Document {
        match self {
            Type::Const { name, .. } => name.clone().to_doc(),

            Type::App { name, args, .. } => name
                .clone()
                .to_doc()
                .append("(")
                .append(args_to_gleam_doc(args, uid))
                .append(")"),

            Type::Fun { args, retrn } => "fn("
                .to_doc()
                .append(args_to_gleam_doc(args, uid))
                .append(") -> ")
                .append(retrn.to_gleam_doc(uid)),

            Type::Record { .. } => unimplemented!(),

            Type::Module { .. } => unimplemented!(),

            Type::Var { typ, .. } => match *typ.borrow() {
                TypeVar::Link { ref typ, .. } => typ.to_gleam_doc(uid),
                TypeVar::Unbound { .. } => next_letter(uid).to_doc(),
                TypeVar::Generic { .. } => next_letter(uid).to_doc(),
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

fn args_to_gleam_doc(args: &Vec<Type>, uid: &mut usize) -> Document {
    args.iter()
        .map(|t| t.to_gleam_doc(uid).group())
        .intersperse(break_(",", ", "))
        .collect::<Vec<_>>()
        .to_doc()
        .nest(INDENT)
        .append(break_(",", ""))
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
            Type::Fun {
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
            Type::Fun {
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
            Type::Fun {
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
        assert_eq!(s.to_string(), typ.to_gleam_doc(&mut 0).format(80));
    }
}

#[derive(Debug, Clone)]
pub enum TypeVar {
    Unbound { id: usize, level: usize },
    Link { typ: Box<Type> },
    Generic { id: usize },
}

#[derive(Debug, Clone)]
pub enum Row {
    Nil,

    Cons {
        label: String,
        head: Box<Type>,
        tail: Box<Row>,
    },
}

#[derive(Debug, Clone, Default)]
pub struct Env {
    uid: usize,
    variables: HashMap<String, Type>,
}

impl Env {
    fn next_uid(&mut self) -> usize {
        let i = self.uid;
        self.uid += 1;
        i
    }

    /// Create a new unbound type that is a specific type, we just don't
    /// know which one yet.
    ///
    pub fn new_var(&mut self, level: usize) -> Type {
        Type::Var {
            typ: Rc::new(RefCell::new(TypeVar::Unbound {
                id: self.next_uid(),
                level: level,
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

    /// Record the type of a variable in the environment.
    ///
    pub fn put_variable(&mut self, name: String, typ: Type) {
        self.variables.insert(name, typ);
    }

    /// Record the type of a variable in the environment.
    ///
    pub fn get_variable(&mut self, name: &String) -> Option<&Type> {
        self.variables.get(name)
    }
}

#[derive(Debug)]
pub enum Error {
    UnknownVariable {
        meta: Meta,
        name: String,
        variables: HashMap<String, Type>,
    },
}

// If we later decide we want to annotate the AST with types we could add a Typed
// variant to the Expr enum
// https://github.com/purescript/purescript/blob/04d53f293b665715d61788213bd0d714c1d08778/src/Language/PureScript/AST/Declarations.hs#L770
//
/// Crawl the AST, annotating each node with the inferred type.
///
pub fn infer(expr: &Expr, level: usize, env: &mut Env) -> Result<Type, Error> {
    match expr {
        Expr::Int { .. } => Ok(int()),

        Expr::Seq { first, then, .. } => {
            infer(first, level, env)?;
            infer(then, level, env)
        }

        Expr::Var { meta, name, .. } => {
            env.get_variable(name)
                .map(|v| (*v).clone())
                .ok_or_else(|| Error::UnknownVariable {
                    meta: (*meta).clone(),
                    name: name.to_string(),
                    variables: env.variables.clone(),
                })
        }

        Expr::Fun { .. } => unimplemented!(),
        Expr::Nil { .. } => unimplemented!(),

        Expr::Let {
            pattern: Pattern::Var { name, .. },
            value,
            then,
            ..
        } => {
            let value_type = infer(value, level + 1, env)?;
            let value_type = generalise(value_type, level);
            env.put_variable(name.to_string(), value_type);
            infer(then, level, env)
        }

        // TODO: Support non var patterns by modifying the previous clause
        Expr::Let { .. } => unimplemented!(),

        Expr::Atom { .. } => Ok(atom()),
        Expr::Case { .. } => unimplemented!(),
        Expr::Cons { .. } => unimplemented!(),
        Expr::Call { .. } => unimplemented!(),
        Expr::Tuple { .. } => unimplemented!(),
        Expr::Float { .. } => Ok(float()),
        Expr::BinOp { .. } => unimplemented!(),
        Expr::String { .. } => Ok(string()),
        Expr::RecordNil { .. } => unimplemented!(),
        Expr::RecordCons { .. } => unimplemented!(),
        Expr::Constructor { .. } => unimplemented!(),
        Expr::RecordSelect { .. } => unimplemented!(),
        Expr::ModuleSelect { .. } => unimplemented!(),
    }
}

#[test]
fn infer_test() {
    let cases = [
        ("1", "Int"),
        ("-2", "Int"),
        ("1.0", "Float"),
        ("-8.0", "Float"),
        ("'hello'", "Atom"),
        ("\"ok\"", "String"),
        ("\"ok\"", "String"),
        ("1 2.0", "Float"),
        ("x = 1 2", "Int"),
        ("x = 1 x", "Int"),
        ("x = 'ok' x", "Atom"),
        ("x = 'ok' y = x y", "Atom"),
    ];

    for (src, typ) in cases.into_iter() {
        let ast = grammar::ExprParser::new().parse(src).unwrap();
        let typed = infer(&ast, 1, &mut Env::default());
        assert_eq!(
            typ.to_string(),
            typed
                .expect("should successfully infer")
                .to_gleam_doc(&mut 0)
                .format(80)
        );
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
fn generalise(typ: Type, level: usize) -> Type {
    match typ {
        Type::Var { .. } => unimplemented!(),
        Type::App { .. } => unimplemented!(),
        Type::Fun { .. } => unimplemented!(),
        Type::Const { .. } => typ,
        Type::Record { .. } => unimplemented!(),
        Type::Module { .. } => unimplemented!(),
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

pub fn atom() -> Type {
    Type::Const {
        public: true,
        name: "Atom".to_string(),
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
