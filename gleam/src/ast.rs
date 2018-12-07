#![allow(dead_code)]

use super::pattern::Pattern;
use super::typ;

#[derive(Debug)]
pub struct Module<T> {
    pub name: String,
    pub statements: Vec<Statement<T>>,
}

#[derive(Debug)]
pub struct Arg {
    pub name: String,
}

#[derive(Debug)]
pub enum Type {
    Constructor {
        meta: Meta,
        name: String,
        args: Vec<Type>,
    },

    Var {
        meta: Meta,
        name: String,
    },
}

#[derive(Debug)]
pub enum Statement<T> {
    Fun {
        meta: Meta,
        name: String,
        args: Vec<Arg>,
        body: Expr<T>,
        public: bool,
    },

    Test {
        meta: Meta,
        name: String,
        body: Expr<T>,
    },

    Enum {
        meta: Meta,
        name: String,
        args: Vec<String>,
        public: bool,
        constructors: Vec<Type>,
    },

    ExternalFun {
        meta: Meta,
        public: bool,
        args: Vec<Type>,
        retrn: Type,
        module: String,
        fun: String,
    },

    ExternalType {
        meta: Meta,
        public: bool,
        name: String,
    },

    Import {
        meta: Meta,
        module: String,
    },
}

impl<T> Statement<T> {
    pub fn meta(&self) -> &Meta {
        match self {
            Statement::Fun { meta, .. } => meta,
            Statement::Test { meta, .. } => meta,
            Statement::Enum { meta, .. } => meta,
            Statement::ExternalFun { meta, .. } => meta,
            Statement::ExternalType { meta, .. } => meta,
            Statement::Import { meta, .. } => meta,
        }
    }
}

#[derive(Debug)]
pub enum BinOp {
    Pipe,
    Lt,
    LtEq,
    Eq,
    GtEq,
    Gt,
    AddInt,
    AddFloat,
    SubInt,
    SubFloat,
    MultInt,
    MultFloat,
    DivInt,
    DivFloat,
}

#[derive(Debug)]
pub enum Scope<T> {
    Local,
    Module,
    Constant { value: Box<Expr<T>> },
}

#[derive(Debug)]
pub enum Expr<T> {
    Int {
        meta: Meta,
        value: i64,
    },

    Float {
        meta: Meta,
        value: f64,
    },

    Atom {
        meta: Meta,
        value: String,
    },

    String {
        meta: Meta,
        value: String,
    },

    Tuple {
        meta: Meta,
        typ: T,
        elems: Vec<Expr<T>>,
    },

    Seq {
        meta: Meta,
        first: Box<Expr<T>>,
        then: Box<Expr<T>>,
    },

    Var {
        meta: Meta,
        typ: T,
        scope: Scope<T>,
        name: String,
    },

    Fun {
        meta: Meta,
        typ: T,
        args: Vec<Arg>,
        body: Box<Expr<T>>,
    },

    Nil {
        meta: Meta,
        typ: T,
    },

    Cons {
        meta: Meta,
        typ: T,
        head: Box<Expr<T>>,
        tail: Box<Expr<T>>,
    },

    Call {
        meta: Meta,
        typ: T,
        fun: Box<Expr<T>>,
        args: Vec<Expr<T>>,
    },

    BinOp {
        meta: Meta,
        typ: T,
        name: BinOp,
        left: Box<Expr<T>>,
        right: Box<Expr<T>>,
    },

    Let {
        meta: Meta,
        typ: T,
        pattern: Pattern,
        left: Box<Expr<T>>,
        right: Box<Expr<T>>,
    },

    Enum {
        meta: Meta,
        typ: T,
        args: Vec<Expr<T>>,
    },

    Case {
        meta: Meta,
        typ: T,
        subject: Box<Expr<T>>,
        clauses: Vec<Clause<T>>,
    },

    RecordNil {
        meta: Meta,
    },

    RecordCons {
        meta: Meta,
        typ: T,
        label: String,
        value: Box<Expr<T>>,
        tail: Box<Expr<T>>,
    },

    RecordSelect {
        meta: Meta,
        typ: T,
        label: String,
        record: Box<Expr<T>>,
    },

    ModuleSelect {
        meta: Meta,
        typ: T,
        label: String,
        module: Box<Expr<T>>,
    },
}

impl<T> Expr<T> {
    pub fn meta(&self) -> &Meta {
        match self {
            Expr::Int { meta, .. } => meta,
            Expr::Float { meta, .. } => meta,
            Expr::Atom { meta, .. } => meta,
            Expr::String { meta, .. } => meta,
            Expr::Tuple { meta, .. } => meta,
            Expr::Seq { meta, .. } => meta,
            Expr::Var { meta, .. } => meta,
            Expr::Fun { meta, .. } => meta,
            Expr::Nil { meta, .. } => meta,
            Expr::Cons { meta, .. } => meta,
            Expr::Call { meta, .. } => meta,
            Expr::BinOp { meta, .. } => meta,
            Expr::Let { meta, .. } => meta,
            Expr::Enum { meta, .. } => meta,
            Expr::Case { meta, .. } => meta,
            Expr::RecordNil { meta, .. } => meta,
            Expr::RecordCons { meta, .. } => meta,
            Expr::RecordSelect { meta, .. } => meta,
            Expr::ModuleSelect { meta, .. } => meta,
        }
    }
}

impl Expr<typ::Type> {
    pub fn typ(&self) -> typ::Type {
        match self {
            Expr::Int { .. } => typ::int(),
            Expr::Float { .. } => typ::float(),
            Expr::Atom { .. } => typ::atom(),
            Expr::String { .. } => typ::string(),
            Expr::Seq { .. } => unimplemented!(),
            Expr::Tuple { typ, .. } => (*typ).clone(),
            Expr::Var { typ, .. } => (*typ).clone(),
            Expr::Fun { typ, .. } => (*typ).clone(),
            Expr::Nil { typ, .. } => (*typ).clone(),
            Expr::Cons { typ, .. } => (*typ).clone(),
            Expr::Call { typ, .. } => (*typ).clone(),
            Expr::BinOp { typ, .. } => (*typ).clone(),
            Expr::Let { typ, .. } => (*typ).clone(),
            Expr::Enum { typ, .. } => (*typ).clone(),
            Expr::Case { typ, .. } => (*typ).clone(),
            Expr::RecordNil { .. } => typ::Type::Record { row: typ::Row::Nil },
            Expr::RecordCons { typ, .. } => (*typ).clone(),
            Expr::RecordSelect { typ, .. } => (*typ).clone(),
            Expr::ModuleSelect { typ, .. } => (*typ).clone(),
        }
    }
}

#[derive(Debug)]
pub struct Clause<T> {
    meta: Meta,
    typ: T,
    pattern: Box<Pattern>,
    body: Box<Expr<T>>,
}

#[derive(Debug)]
pub struct Meta {}
