use crate::typ;

pub type TypedModule = Module<typ::Type>;

pub type UntypedModule = Module<()>;

#[derive(Debug, PartialEq)]
pub struct Module<T> {
    pub name: String,
    pub statements: Vec<Statement<T>>,
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

pub type TypedStatement = Statement<typ::Type>;

pub type UntypedStatement = Statement<()>;

#[derive(Debug, PartialEq)]
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

pub type TypedScope = Scope<typ::Type>;

// TODO: make only exist for TypedExpr
#[derive(Debug, PartialEq)]
pub enum Scope<T> {
    Local,
    Module { arity: usize },
    Constant { value: Box<Expr<T>> },
}

pub type TypedExpr = Expr<typ::Type>;

pub type UntypedExpr = Expr<()>;

// TODO: Ideally we would parameterise the typ instead of using Option<Type>
// as then we can use the type system to ensure that the AST has the correct
// information before we pass it to the codegen function.
// I don't know how update a struct in place in a fashion that changes its type
// though, and reconstructing the term to copy across the values is cumbersome.
//
#[derive(Debug, PartialEq)]
pub enum Expr<T> {
    Int {
        meta: Meta,
        typ: T,
        value: i64,
    },

    Float {
        meta: Meta,
        typ: T,
        value: f64,
    },

    Atom {
        meta: Meta,
        typ: T,
        value: String,
    },

    String {
        meta: Meta,
        typ: T,
        value: String,
    },

    Tuple {
        meta: Meta,
        typ: T,
        elems: Vec<Expr<T>>,
    },

    Seq {
        meta: Meta,
        typ: T,
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
        value: Box<Expr<T>>,
        pattern: Pattern,
        then: Box<Expr<T>>,
    },

    Constructor {
        meta: Meta,
        typ: T,
        name: String,
    },

    Case {
        meta: Meta,
        typ: T,
        subject: Box<Expr<T>>,
        clauses: Vec<Clause<T>>,
    },

    RecordNil {
        meta: Meta,
        typ: T,
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

impl TypedExpr {
    pub fn typ<'a>(&'a self) -> &'a typ::Type {
        match self {
            Expr::Int { typ, .. } => typ,
            Expr::Float { typ, .. } => typ,
            Expr::Atom { typ, .. } => typ,
            Expr::String { typ, .. } => typ,
            Expr::Seq { then, .. } => then.typ(),
            Expr::Tuple { typ, .. } => typ,
            Expr::Var { typ, .. } => typ,
            Expr::Fun { typ, .. } => typ,
            Expr::Nil { typ, .. } => typ,
            Expr::Cons { typ, .. } => typ,
            Expr::Call { typ, .. } => typ,
            Expr::BinOp { typ, .. } => typ,
            Expr::Let { typ, .. } => typ,
            Expr::Case { typ, .. } => typ,
            Expr::RecordNil { typ, .. } => typ,
            Expr::RecordCons { typ, .. } => typ,
            Expr::Constructor { typ, .. } => typ,
            Expr::RecordSelect { typ, .. } => typ,
            Expr::ModuleSelect { typ, .. } => typ,
        }
    }

    pub fn typ_mut<'a>(&'a mut self) -> &'a mut typ::Type {
        match self {
            Expr::Int { typ, .. } => typ,
            Expr::Float { typ, .. } => typ,
            Expr::Atom { typ, .. } => typ,
            Expr::String { typ, .. } => typ,
            Expr::Seq { then, .. } => then.typ_mut(),
            Expr::Tuple { typ, .. } => typ,
            Expr::Var { typ, .. } => typ,
            Expr::Fun { typ, .. } => typ,
            Expr::Nil { typ, .. } => typ,
            Expr::Cons { typ, .. } => typ,
            Expr::Call { typ, .. } => typ,
            Expr::BinOp { typ, .. } => typ,
            Expr::Let { typ, .. } => typ,
            Expr::Case { typ, .. } => typ,
            Expr::RecordNil { typ, .. } => typ,
            Expr::RecordCons { typ, .. } => typ,
            Expr::Constructor { typ, .. } => typ,
            Expr::RecordSelect { typ, .. } => typ,
            Expr::ModuleSelect { typ, .. } => typ,
        }
    }
}

pub type TypedClause = Clause<typ::Type>;

#[derive(Debug, PartialEq)]
pub struct Clause<T> {
    pub meta: Meta,
    pub pattern: Pattern,
    pub then: Box<Expr<T>>,
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
