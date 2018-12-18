use crate::typ;

#[derive(Debug, PartialEq)]
pub struct Module {
    pub name: String,
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct Arg {
    pub name: String,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Statement {
    Fun {
        meta: Meta,
        name: String,
        args: Vec<Arg>,
        body: Expr,
        public: bool,
    },

    Test {
        meta: Meta,
        name: String,
        body: Expr,
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
        name: String,
        retrn: Type,
        module: String,
        fun: String,
    },

    ExternalType {
        meta: Meta,
        public: bool,
        name: String,
        args: Vec<String>,
    },

    Import {
        meta: Meta,
        module: String,
    },
}

impl Statement {
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

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Scope {
    Local,
    Module { arity: usize },
    Constant { value: Box<Expr> },
}

// TODO: Ideally we would parameterise the typ instead of using Option<Type>
// as then we can use the type system to ensure that the AST has the correct
// information before we pass it to the codegen function.
// I don't know how update a struct in place in a fashion that changes its type
// though, and reconstructing the term to copy across the values is cumbersome.
//
#[derive(Debug, PartialEq)]
pub enum Expr {
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
        typ: Option<typ::Type>,
        elems: Vec<Expr>,
    },

    Seq {
        meta: Meta,
        typ: Option<typ::Type>,
        first: Box<Expr>,
        then: Box<Expr>,
    },

    Var {
        meta: Meta,
        typ: Option<typ::Type>,
        scope: Scope,
        name: String,
    },

    Fun {
        meta: Meta,
        typ: Option<typ::Type>,
        args: Vec<Arg>,
        body: Box<Expr>,
    },

    Nil {
        meta: Meta,
        typ: Option<typ::Type>,
    },

    Cons {
        meta: Meta,
        typ: Option<typ::Type>,
        head: Box<Expr>,
        tail: Box<Expr>,
    },

    Call {
        meta: Meta,
        typ: Option<typ::Type>,
        fun: Box<Expr>,
        args: Vec<Expr>,
    },

    BinOp {
        meta: Meta,
        typ: Option<typ::Type>,
        name: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    Let {
        meta: Meta,
        typ: Option<typ::Type>,
        value: Box<Expr>,
        pattern: Pattern,
        then: Box<Expr>,
    },

    Constructor {
        meta: Meta,
        typ: Option<typ::Type>,
        name: String,
    },

    Case {
        meta: Meta,
        typ: Option<typ::Type>,
        subject: Box<Expr>,
        clauses: Vec<Clause>,
    },

    RecordNil {
        meta: Meta,
    },

    RecordCons {
        meta: Meta,
        typ: Option<typ::Type>,
        label: String,
        value: Box<Expr>,
        tail: Box<Expr>,
    },

    RecordSelect {
        meta: Meta,
        typ: Option<typ::Type>,
        label: String,
        record: Box<Expr>,
    },

    ModuleSelect {
        meta: Meta,
        typ: Option<typ::Type>,
        label: String,
        module: Box<Expr>,
    },
}

impl Expr {
    pub fn meta(&self) -> &Meta {
        match self {
            Expr::Int { meta, .. } => meta,
            Expr::Seq { meta, .. } => meta,
            Expr::Var { meta, .. } => meta,
            Expr::Fun { meta, .. } => meta,
            Expr::Nil { meta, .. } => meta,
            Expr::Let { meta, .. } => meta,
            Expr::Atom { meta, .. } => meta,
            Expr::Case { meta, .. } => meta,
            Expr::Cons { meta, .. } => meta,
            Expr::Call { meta, .. } => meta,
            Expr::Tuple { meta, .. } => meta,
            Expr::Float { meta, .. } => meta,
            Expr::BinOp { meta, .. } => meta,
            Expr::String { meta, .. } => meta,
            Expr::RecordNil { meta, .. } => meta,
            Expr::RecordCons { meta, .. } => meta,
            Expr::Constructor { meta, .. } => meta,
            Expr::RecordSelect { meta, .. } => meta,
            Expr::ModuleSelect { meta, .. } => meta,
        }
    }

    // TODO: Reference here? Need to work out how lifetimes work for the
    // variants with no type value within.
    pub fn typ(&self) -> Option<typ::Type> {
        match self {
            Expr::Int { .. } => Some(typ::int()),
            Expr::Float { .. } => Some(typ::float()),
            Expr::Atom { .. } => Some(typ::atom()),
            Expr::String { .. } => Some(typ::string()),
            Expr::Seq { then, .. } => then.typ(),
            Expr::Tuple { typ, .. } => (*typ).clone(),
            Expr::Var { typ, .. } => (*typ).clone(),
            Expr::Fun { typ, .. } => (*typ).clone(),
            Expr::Nil { typ, .. } => (*typ).clone(),
            Expr::Cons { typ, .. } => (*typ).clone(),
            Expr::Call { typ, .. } => (*typ).clone(),
            Expr::BinOp { typ, .. } => (*typ).clone(),
            Expr::Let { typ, .. } => (*typ).clone(),
            Expr::Case { typ, .. } => (*typ).clone(),
            Expr::RecordNil { .. } => Some(typ::Type::Record { row: typ::Row::Nil }),
            Expr::RecordCons { typ, .. } => (*typ).clone(),
            Expr::Constructor { typ, .. } => (*typ).clone(),
            Expr::RecordSelect { typ, .. } => (*typ).clone(),
            Expr::ModuleSelect { typ, .. } => (*typ).clone(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Clause {
    pub meta: Meta,
    pub pattern: Pattern,
    pub then: Box<Expr>,
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct Meta {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, PartialEq)]
pub enum Pattern {
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

    Var {
        meta: Meta,
        name: String,
    },

    Tuple {
        meta: Meta,
        elems: Vec<Pattern>,
    },

    Nil {
        meta: Meta,
    },

    List {
        meta: Meta,
        elems: Vec<Pattern>,
        tail: Option<Box<Pattern>>,
    },

    Record {
        meta: Meta,
        label: String,
        fields: Vec<(Pattern, Pattern)>,
    },

    Enum {
        meta: Meta,
        name: String,
        args: Vec<Pattern>,
    },
}

impl Pattern {
    pub fn meta(&self) -> &Meta {
        match self {
            Pattern::Int { meta, .. } => meta,
            Pattern::Var { meta, .. } => meta,
            Pattern::Nil { meta, .. } => meta,
            Pattern::List { meta, .. } => meta,
            Pattern::Atom { meta, .. } => meta,
            Pattern::Enum { meta, .. } => meta,
            Pattern::Float { meta, .. } => meta,
            Pattern::Tuple { meta, .. } => meta,
            Pattern::String { meta, .. } => meta,
            Pattern::Record { meta, .. } => meta,
        }
    }
}
