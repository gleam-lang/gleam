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
        elems: Vec<Expr>,
    },

    Seq {
        meta: Meta,
        first: Box<Expr>,
        then: Box<Expr>,
    },

    Var {
        meta: Meta,
        scope: Scope,
        name: String,
    },

    Fun {
        meta: Meta,
        args: Vec<Arg>,
        body: Box<Expr>,
    },

    Nil {
        meta: Meta,
    },

    Cons {
        meta: Meta,
        head: Box<Expr>,
        tail: Box<Expr>,
    },

    Call {
        meta: Meta,
        fun: Box<Expr>,
        args: Vec<Expr>,
    },

    BinOp {
        meta: Meta,
        name: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },

    Let {
        meta: Meta,
        value: Box<Expr>,
        pattern: Pattern,
        then: Box<Expr>,
    },

    Constructor {
        meta: Meta,
        name: String,
    },

    Case {
        meta: Meta,
        subject: Box<Expr>,
        clauses: Vec<Clause>,
    },

    RecordNil {
        meta: Meta,
    },

    RecordCons {
        meta: Meta,
        label: String,
        value: Box<Expr>,
        tail: Box<Expr>,
    },

    RecordSelect {
        meta: Meta,
        label: String,
        record: Box<Expr>,
    },

    ModuleSelect {
        meta: Meta,
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
