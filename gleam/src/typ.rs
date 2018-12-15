use crate::ast::Expr;
use crate::grammar;
use crate::pretty::*;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum Type {
    Const {
        public: bool,
        name: String,
        module: String,
    },

    App {
        public: bool,
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
        typ: Rc<TypeVar>,
    },
}

impl Type {
    pub fn to_gleam_doc(&self, uid: usize) -> Document {
        match self {
            Type::Const { name, .. } => name.clone().to_doc(),
            Type::App { .. } => unimplemented!(),
            Type::Fun { .. } => unimplemented!(),
            Type::Record { .. } => unimplemented!(),
            Type::Module { .. } => unimplemented!(),
            Type::Var { .. } => unimplemented!(),
        }
    }
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
            Type::Const {
                module: "".to_string(),
                name: "Float".to_string(),
                public: false,
            },
            "Float",
        ),
    ];

    for (typ, s) in cases.into_iter() {
        assert_eq!(s.to_string(), typ.to_gleam_doc(0).format(80));
    }
}

#[derive(Debug)]
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
}

impl Env {
    fn next_uid(&mut self) -> usize {
        let i = self.uid;
        self.uid += 1;
        i
    }

    pub fn new_var(&mut self, level: usize) -> Type {
        Type::Var {
            typ: Rc::new(TypeVar::Unbound {
                id: self.next_uid(),
                level: level,
            }),
        }
    }

    pub fn new_generic_var(&mut self) -> Type {
        Type::Var {
            typ: Rc::new(TypeVar::Generic {
                id: self.next_uid(),
            }),
        }
    }
}

pub enum Error {
    Nope,
}

pub fn infer(expr: Expr<()>, mut env: &Env) -> Expr<Type> {
    match expr {
        Expr::Int { meta, value } => Expr::Int { meta, value },
        Expr::Seq { .. } => unimplemented!(),
        Expr::Var { .. } => unimplemented!(),
        Expr::Fun { .. } => unimplemented!(),
        Expr::Nil { .. } => unimplemented!(),
        Expr::Let { .. } => unimplemented!(),
        Expr::Atom { meta, value } => Expr::Atom { meta, value },
        Expr::Case { .. } => unimplemented!(),
        Expr::Cons { .. } => unimplemented!(),
        Expr::Call { .. } => unimplemented!(),
        Expr::Tuple { .. } => unimplemented!(),
        Expr::Float { meta, value } => Expr::Float { meta, value },
        Expr::BinOp { .. } => unimplemented!(),
        Expr::String { meta, value } => Expr::String { meta, value },
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
    ];

    for (src, typ) in cases.into_iter() {
        let ast = grammar::ExprParser::new().parse(src).unwrap();
        let typed = infer(ast, &mut Env::default());
        assert_eq!(typ.to_string(), typed.typ().to_gleam_doc(0).format(80));
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
