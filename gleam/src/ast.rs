use crate::typ;

pub type TypedModule = Module<Scope<typ::Type>, typ::Type>;

pub type UntypedModule = Module<(), ()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Module<S, T> {
    pub name: String,
    pub typ: T,
    pub statements: Vec<Statement<S, T>>,
}

impl<S, T> Module<S, T> {
    pub fn dependancies(&self) -> Vec<String> {
        self.statements
            .iter()
            .flat_map(|s| match s {
                Statement::Import { module, .. } => Some(module.clone()),
                _ => None,
            })
            .collect()
    }
}

#[test]
fn module_dependencies_test() {
    assert_eq!(
        vec!["foo".to_string(), "bar".to_string(), "foo_bar".to_string()],
        crate::grammar::ModuleParser::new()
            .parse("import foo import bar import foo_bar")
            .expect("syntax error")
            .dependancies()
    );
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arg {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumConstructor {
    pub meta: Meta,
    pub name: String,
    pub args: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Constructor {
        meta: Meta,
        name: String,
        args: Vec<Type>,
    },

    Fn {
        meta: Meta,
        args: Vec<Type>,
        retrn: Box<Type>,
    },

    Var {
        meta: Meta,
        name: String,
    },

    Tuple {
        meta: Meta,
        elems: Vec<Type>,
    },
}

pub type TypedStatement = Statement<Scope<typ::Type>, typ::Type>;

pub type UntypedStatement = Statement<(), ()>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<S, T> {
    Fn {
        meta: Meta,
        name: String,
        args: Vec<Arg>,
        body: Expr<S, T>,
        public: bool,
    },

    Test {
        meta: Meta,
        name: String,
        body: Expr<S, T>,
    },

    Enum {
        meta: Meta,
        name: String,
        args: Vec<String>,
        public: bool,
        constructors: Vec<EnumConstructor>,
    },

    ExternalFn {
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

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Pipe,
    And,
    Or,
    Lt,
    LtEq,
    Eq,
    NotEq,
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
    ModuloInt,
    ModuloFloat,
}

pub type TypedScope = Scope<typ::Type>;

#[derive(Debug, Clone, PartialEq)]
pub enum Scope<T> {
    Local,
    Module { arity: usize },
    Import { module: String },
    Constant { value: Box<Expr<Scope<T>, T>> },
}

pub type TypedExpr = Expr<Scope<typ::Type>, typ::Type>;

pub type UntypedExpr = Expr<(), ()>;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<S, T> {
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

    String {
        meta: Meta,
        typ: T,
        value: String,
    },

    Tuple {
        meta: Meta,
        typ: T,
        elems: Vec<Expr<S, T>>,
    },

    Seq {
        meta: Meta,
        typ: T,
        first: Box<Expr<S, T>>,
        then: Box<Expr<S, T>>,
    },

    Var {
        meta: Meta,
        typ: T,
        scope: S,
        name: String,
    },

    Fn {
        meta: Meta,
        typ: T,
        args: Vec<Arg>,
        body: Box<Expr<S, T>>,
    },

    Nil {
        meta: Meta,
        typ: T,
    },

    Cons {
        meta: Meta,
        typ: T,
        head: Box<Expr<S, T>>,
        tail: Box<Expr<S, T>>,
    },

    Call {
        meta: Meta,
        typ: T,
        fun: Box<Expr<S, T>>,
        args: Vec<Expr<S, T>>,
    },

    BinOp {
        meta: Meta,
        typ: T,
        name: BinOp,
        left: Box<Expr<S, T>>,
        right: Box<Expr<S, T>>,
    },

    Let {
        meta: Meta,
        typ: T,
        value: Box<Expr<S, T>>,
        pattern: Pattern,
        then: Box<Expr<S, T>>,
    },

    Constructor {
        meta: Meta,
        typ: T,
        name: String,
    },

    Case {
        meta: Meta,
        typ: T,
        subject: Box<Expr<S, T>>,
        clauses: Vec<Clause<S, T>>,
    },

    RecordNil {
        meta: Meta,
        typ: T,
    },

    RecordCons {
        meta: Meta,
        typ: T,
        label: String,
        value: Box<Expr<S, T>>,
        tail: Box<Expr<S, T>>,
    },

    RecordSelect {
        meta: Meta,
        typ: T,
        label: String,
        record: Box<Expr<S, T>>,
    },

    ModuleSelect {
        meta: Meta,
        typ: T,
        label: String,
        module: Box<Expr<S, T>>,
    },
}

impl<S, T> Expr<S, T> {
    pub fn meta(&self) -> &Meta {
        match self {
            Expr::Int { meta, .. } => meta,
            Expr::Seq { meta, .. } => meta,
            Expr::Var { meta, .. } => meta,
            Expr::Fn { meta, .. } => meta,
            Expr::Nil { meta, .. } => meta,
            Expr::Let { meta, .. } => meta,
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
            Expr::String { typ, .. } => typ,
            Expr::Seq { then, .. } => then.typ(),
            Expr::Tuple { typ, .. } => typ,
            Expr::Var { typ, .. } => typ,
            Expr::Fn { typ, .. } => typ,
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

pub type TypedClause = Clause<Scope<typ::Type>, typ::Type>;

pub type UntypedClause = Clause<(), ()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Clause<S, T> {
    pub meta: Meta,
    pub pattern: Pattern,
    // TODO: Unbox this
    pub then: Box<Expr<S, T>>,
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct Meta {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Int {
        meta: Meta,
        value: i64,
    },

    Float {
        meta: Meta,
        value: f64,
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

    Cons {
        meta: Meta,
        head: Box<Pattern>,
        tail: Box<Pattern>,
    },

    //     Record {
    //         meta: Meta,
    //         label: String,
    //         fields: Vec<(Pattern, Pattern)>,
    //     },
    Constructor {
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
            Pattern::Cons { meta, .. } => meta,
            Pattern::Tuple { meta, .. } => meta,
            Pattern::Float { meta, .. } => meta,
            // Pattern::Record { meta, .. } => meta,
            Pattern::String { meta, .. } => meta,
            Pattern::Constructor { meta, .. } => meta,
        }
    }
}
