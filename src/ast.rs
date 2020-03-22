mod typed;
mod untyped;

pub use self::typed::TypedExpr;
pub use self::untyped::UntypedExpr;

use crate::typ::{self, ModuleValueConstructor, PatternConstructor, ValueConstructor};
use std::sync::Arc;

pub type TypedModule = Module<TypedExpr, typ::Module>;

pub type UntypedModule = Module<UntypedExpr, ()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Module<Expr, Info> {
    pub name: Vec<String>,
    pub type_info: Info,
    pub statements: Vec<Statement<Expr>>,
}

impl<A, B> Module<A, B> {
    pub fn name_string(&self) -> String {
        self.name.join("/")
    }

    pub fn dependencies(&self) -> Vec<(String, Meta)> {
        self.statements
            .iter()
            .flat_map(|s| match s {
                Statement::Import { module, meta, .. } => Some((module.join("/"), meta.clone())),
                _ => None,
            })
            .collect()
    }
}

#[test]
fn module_dependencies_test() {
    assert_eq!(
        vec![
            ("foo".to_string(), Meta { start: 7, end: 10 }),
            ("bar".to_string(), Meta { start: 18, end: 21 }),
            ("foo_bar".to_string(), Meta { start: 29, end: 36 }),
        ],
        crate::grammar::ModuleParser::new()
            .parse("import foo import bar import foo_bar")
            .expect("syntax error")
            .dependencies()
    );
}

#[derive(Debug, Clone, PartialEq)]
pub struct Arg {
    pub names: ArgNames,
    pub meta: Meta,
    pub annotation: Option<TypeAst>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArgNames {
    Discard,
    LabelledDiscard { label: String },
    Named { name: String },
    NamedLabelled { name: String, label: String },
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordConstructor {
    pub meta: Meta,
    pub name: String,
    pub args: Vec<(Option<String>, TypeAst)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeAst {
    Constructor {
        meta: Meta,
        module: Option<String>,
        name: String,
        args: Vec<Self>,
    },

    Fn {
        meta: Meta,
        args: Vec<Self>,
        retrn: Box<Self>,
    },

    Var {
        meta: Meta,
        name: String,
    },

    Tuple {
        meta: Meta,
        elems: Vec<Self>,
    },
}

impl TypeAst {
    pub fn meta(&self) -> &Meta {
        match self {
            TypeAst::Fn { meta, .. } => meta,
            TypeAst::Var { meta, .. } => meta,
            TypeAst::Tuple { meta, .. } => meta,
            TypeAst::Constructor { meta, .. } => meta,
        }
    }
}

pub type TypedStatement = Statement<TypedExpr>;
pub type UntypedStatement = Statement<UntypedExpr>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<Expr> {
    Fn {
        meta: Meta,
        name: String,
        args: Vec<Arg>,
        body: Expr,
        public: bool,
        return_annotation: Option<TypeAst>,
    },

    TypeAlias {
        meta: Meta,
        alias: String,
        args: Vec<String>,
        resolved_type: TypeAst,
        public: bool,
    },

    CustomType {
        meta: Meta,
        name: String,
        args: Vec<String>,
        public: bool,
        constructors: Vec<RecordConstructor>,
    },

    ExternalFn {
        meta: Meta,
        public: bool,
        args: Vec<ExternalFnArg>,
        name: String,
        retrn: TypeAst,
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
        module: Vec<String>,
        as_name: Option<String>,
        unqualified: Vec<UnqualifiedImport>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnqualifiedImport {
    pub meta: Meta,
    pub name: String,
    pub as_name: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternalFnArg {
    pub label: Option<String>,
    pub typ: TypeAst,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    And,
    Or,
    LtInt,
    LtEqInt,
    LtFloat,
    LtEqFloat,
    Eq,
    NotEq,
    GtEqInt,
    GtInt,
    GtEqFloat,
    GtFloat,
    AddInt,
    AddFloat,
    SubInt,
    SubFloat,
    MultInt,
    MultFloat,
    DivInt,
    DivFloat,
    ModuloInt,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallArg<A> {
    pub label: Option<String>,
    pub meta: Meta,
    pub value: A,
}

pub type MultiPattern<PatternConstructor> = Vec<Pattern<PatternConstructor>>;
pub type UntypedMultiPattern = MultiPattern<()>;
pub type TypedMultiPattern = MultiPattern<PatternConstructor>;

pub type TypedClause = Clause<TypedExpr, PatternConstructor, Arc<typ::Type>>;

pub type UntypedClause = Clause<UntypedExpr, (), ()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Clause<Expr, PatternConstructor, Type> {
    pub meta: Meta,
    pub pattern: MultiPattern<PatternConstructor>,
    pub alternative_patterns: Vec<MultiPattern<PatternConstructor>>,
    pub guard: Option<ClauseGuard<Type>>,
    pub then: Expr,
}

pub type UntypedClauseGuard = ClauseGuard<()>;
pub type TypedClauseGuard = ClauseGuard<Arc<typ::Type>>;

#[derive(Debug, PartialEq, Clone)]
pub enum ClauseGuard<Type> {
    Equals {
        meta: Meta,
        typ: Type,
        left: Box<Self>,
        right: Box<Self>,
    },

    NotEquals {
        meta: Meta,
        typ: Type,
        left: Box<Self>,
        right: Box<Self>,
    },

    Or {
        meta: Meta,
        typ: Type,
        left: Box<Self>,
        right: Box<Self>,
    },

    And {
        meta: Meta,
        typ: Type,
        left: Box<Self>,
        right: Box<Self>,
    },

    Var {
        meta: Meta,
        typ: Type,
        name: String,
    },
}

impl<A> ClauseGuard<A> {
    pub fn meta(&self) -> &Meta {
        match self {
            ClauseGuard::Or { meta, .. } => meta,
            ClauseGuard::And { meta, .. } => meta,
            ClauseGuard::Var { meta, .. } => meta,
            ClauseGuard::Equals { meta, .. } => meta,
            ClauseGuard::NotEquals { meta, .. } => meta,
        }
    }
}

impl TypedClauseGuard {
    pub fn typ(&self) -> Arc<typ::Type> {
        match self {
            ClauseGuard::Or { typ, .. } => typ.clone(),
            ClauseGuard::And { typ, .. } => typ.clone(),
            ClauseGuard::Var { typ, .. } => typ.clone(),
            ClauseGuard::Equals { typ, .. } => typ.clone(),
            ClauseGuard::NotEquals { typ, .. } => typ.clone(),
        }
    }
}

#[derive(Debug, PartialEq, Default, Clone)]
pub struct Meta {
    pub start: usize,
    pub end: usize,
}

pub type UntypedPattern = Pattern<()>;
pub type TypedPattern = Pattern<PatternConstructor>;

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern<Constructor> {
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

    Let {
        name: String,
        pattern: Box<Self>,
    },

    Discard {
        meta: Meta,
    },

    Nil {
        meta: Meta,
    },

    Cons {
        meta: Meta,
        head: Box<Self>,
        tail: Box<Self>,
    },

    Constructor {
        meta: Meta,
        name: String,
        args: Vec<CallArg<Self>>,
        module: Option<String>,
        constructor: Constructor,
    },

    Tuple {
        meta: Meta,
        elems: Vec<Self>,
    },
}

impl<A> Pattern<A> {
    pub fn meta(&self) -> &Meta {
        match self {
            Pattern::Let { pattern, .. } => pattern.meta(),
            Pattern::Int { meta, .. } => meta,
            Pattern::Var { meta, .. } => meta,
            Pattern::Nil { meta, .. } => meta,
            Pattern::Cons { meta, .. } => meta,
            Pattern::Float { meta, .. } => meta,
            Pattern::Discard { meta, .. } => meta,
            Pattern::String { meta, .. } => meta,
            Pattern::Tuple { meta, .. } => meta,
            Pattern::Constructor { meta, .. } => meta,
        }
    }
}
