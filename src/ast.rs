use crate::typ::{self, ModuleValueConstructor, PatternConstructor, ValueConstructor};

pub type TypedModule =
    Module<ValueConstructor, ModuleValueConstructor, PatternConstructor, typ::Type, typ::Module>;

pub type UntypedModule = Module<(), (), (), (), ()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Module<ValueConstructor, ModuleValueConstructor, PatternConstructor, Type, Info> {
    pub name: Vec<String>,
    pub type_info: Info,
    pub statements:
        Vec<Statement<ValueConstructor, ModuleValueConstructor, PatternConstructor, Type>>,
}

impl<A, B, C, D, E> Module<A, B, C, D, E> {
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

pub type TypedStatement =
    Statement<ValueConstructor, ModuleValueConstructor, PatternConstructor, typ::Type>;

pub type UntypedStatement = Statement<(), (), (), ()>;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<ValueConstructor, ModuleValueConstructor, PatternConstructor, Type> {
    Fn {
        meta: Meta,
        name: String,
        args: Vec<Arg>,
        body: Expr<ValueConstructor, ModuleValueConstructor, PatternConstructor, Type>,
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
    Pipe,
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

pub type TypedExpr = Expr<ValueConstructor, ModuleValueConstructor, PatternConstructor, typ::Type>;

pub type UntypedExpr = Expr<(), (), (), ()>;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr<ValueConstructor, ModuleValueConstructor, PatternConstructor, Type> {
    Int {
        meta: Meta,
        typ: Type,
        value: i64,
    },

    Float {
        meta: Meta,
        typ: Type,
        value: f64,
    },

    String {
        meta: Meta,
        typ: Type,
        value: String,
    },

    Seq {
        typ: Type,
        first: Box<Self>,
        then: Box<Self>,
    },

    Var {
        meta: Meta,
        constructor: ValueConstructor,
        name: String,
    },

    Fn {
        meta: Meta,
        typ: Type,
        is_capture: bool,
        args: Vec<Arg>,
        body: Box<Self>,
        return_annotation: Option<TypeAst>,
    },

    Nil {
        meta: Meta,
        typ: Type,
    },

    Cons {
        meta: Meta,
        typ: Type,
        head: Box<Self>,
        tail: Box<Self>,
    },

    Call {
        meta: Meta,
        typ: Type,
        fun: Box<Self>,
        args: Vec<CallArg<Self>>,
    },

    BinOp {
        meta: Meta,
        typ: Type,
        name: BinOp,
        left: Box<Self>,
        right: Box<Self>,
    },

    Let {
        meta: Meta,
        typ: Type,
        value: Box<Self>,
        pattern: Pattern<PatternConstructor>,
        then: Box<Self>,
    },

    Case {
        meta: Meta,
        typ: Type,
        subjects: Vec<Self>,
        clauses: Vec<Clause<ValueConstructor, ModuleValueConstructor, PatternConstructor, Type>>,
    },

    FieldSelect {
        meta: Meta,
        typ: Type,
        label: String,
        container: Box<Self>,
    },

    ModuleSelect {
        meta: Meta,
        typ: Type,
        label: String,
        module_name: Vec<String>,
        module_alias: String,
        constructor: ModuleValueConstructor,
    },

    Tuple {
        meta: Meta,
        typ: Type,
        elems: Vec<Self>,
    },
}

impl<A, B, C, D> Expr<A, B, C, D> {
    pub fn meta(&self) -> &Meta {
        match self {
            Expr::Fn { meta, .. } => meta,
            Expr::Int { meta, .. } => meta,
            Expr::Seq { then, .. } => then.meta(),
            Expr::Var { meta, .. } => meta,
            Expr::Nil { meta, .. } => meta,
            Expr::Let { then, .. } => then.meta(),
            Expr::Case { meta, .. } => meta,
            Expr::Cons { meta, .. } => meta,
            Expr::Call { meta, .. } => meta,
            Expr::Float { meta, .. } => meta,
            Expr::BinOp { meta, .. } => meta,
            Expr::String { meta, .. } => meta,
            Expr::Tuple { meta, .. } => meta,
            Expr::FieldSelect { meta, .. } => meta,
            Expr::ModuleSelect { meta, .. } => meta,
        }
    }
}

impl TypedExpr {
    pub fn typ(&self) -> &typ::Type {
        match self {
            Expr::Fn { typ, .. } => typ,
            Expr::Nil { typ, .. } => typ,
            Expr::Let { typ, .. } => typ,
            Expr::Int { typ, .. } => typ,
            Expr::Case { typ, .. } => typ,
            Expr::Cons { typ, .. } => typ,
            Expr::Call { typ, .. } => typ,
            Expr::Seq { then, .. } => then.typ(),
            Expr::Float { typ, .. } => typ,
            Expr::BinOp { typ, .. } => typ,
            Expr::Tuple { typ, .. } => typ,
            Expr::String { typ, .. } => typ,
            Expr::Var { constructor, .. } => &constructor.typ,
            Expr::FieldSelect { typ, .. } => typ,
            Expr::ModuleSelect { typ, .. } => typ,
        }
    }
}

pub type MultiPattern<PatternConstructor> = Vec<Pattern<PatternConstructor>>;
pub type UntypedMultiPattern = MultiPattern<()>;
pub type TypedMultiPattern = MultiPattern<PatternConstructor>;

pub type TypedClause =
    Clause<ValueConstructor, ModuleValueConstructor, PatternConstructor, typ::Type>;

pub type UntypedClause = Clause<(), (), (), ()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Clause<ValueConstructor, ModuleValueConstructor, PatternConstructor, Type> {
    pub meta: Meta,
    pub pattern: MultiPattern<PatternConstructor>,
    pub alternative_patterns: Vec<MultiPattern<PatternConstructor>>,
    pub guard: Option<ClauseGuard<Type>>,
    pub then: Expr<ValueConstructor, ModuleValueConstructor, PatternConstructor, Type>,
}

pub type TypedClauseGuard = ClauseGuard<typ::Type>;

pub type UntypedClauseGuard = ClauseGuard<()>;

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
    pub fn typ(&self) -> &typ::Type {
        match self {
            ClauseGuard::Or { typ, .. } => typ,
            ClauseGuard::And { typ, .. } => typ,
            ClauseGuard::Var { typ, .. } => typ,
            ClauseGuard::Equals { typ, .. } => typ,
            ClauseGuard::NotEquals { typ, .. } => typ,
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
