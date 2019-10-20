use crate::typ::{self, ModuleValueConstructor, PatternConstructor, ValueConstructor};

pub type TypedModule = Module<
    ValueConstructor,
    ModuleValueConstructor,
    PatternConstructor,
    typ::Type,
    typ::ModuleTypeInfo,
>;

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
            ("foo".to_string(), Meta { start: 0, end: 10 }),
            ("bar".to_string(), Meta { start: 11, end: 21 }),
            ("foo_bar".to_string(), Meta { start: 22, end: 36 }),
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
    Named { name: String },
    NamedLabelled { name: String, label: String },
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumConstructor {
    pub meta: Meta,
    pub name: String,
    pub args: Vec<TypeAst>,
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
    },

    Struct {
        meta: Meta,
        name: String,
        type_args: Vec<String>,
        public: bool,
        fields: Vec<StructField>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExternalFnArg {
    pub label: Option<String>,
    pub typ: TypeAst,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub label: String,
    pub meta: Meta,
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
        subject: Box<Self>,
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
            Expr::FieldSelect { meta, .. } => meta,
            Expr::ModuleSelect { meta, .. } => meta,
        }
    }
}

impl TypedExpr {
    pub fn typ(&self) -> &typ::Type {
        match self {
            Expr::Int { typ, .. } => typ,
            Expr::Float { typ, .. } => typ,
            Expr::String { typ, .. } => typ,
            Expr::Seq { then, .. } => then.typ(),
            Expr::Var { constructor, .. } => &constructor.typ,
            Expr::Fn { typ, .. } => typ,
            Expr::Nil { typ, .. } => typ,
            Expr::Cons { typ, .. } => typ,
            Expr::Call { typ, .. } => typ,
            Expr::BinOp { typ, .. } => typ,
            Expr::Let { typ, .. } => typ,
            Expr::Case { typ, .. } => typ,
            Expr::FieldSelect { typ, .. } => typ,
            Expr::ModuleSelect { typ, .. } => typ,
        }
    }
}

pub type TypedClause =
    Clause<ValueConstructor, ModuleValueConstructor, PatternConstructor, typ::Type>;

pub type UntypedClause = Clause<(), (), (), ()>;

#[derive(Debug, Clone, PartialEq)]
pub struct Clause<ValueConstructor, ModuleValueConstructor, PatternConstructor, Type> {
    pub meta: Meta,
    pub pattern: Pattern<PatternConstructor>,
    pub then: Expr<ValueConstructor, ModuleValueConstructor, PatternConstructor, Type>,
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
}

impl<A> Pattern<A> {
    pub fn meta(&self) -> &Meta {
        match self {
            Pattern::Int { meta, .. } => meta,
            Pattern::Var { meta, .. } => meta,
            Pattern::Nil { meta, .. } => meta,
            Pattern::Cons { meta, .. } => meta,
            Pattern::Float { meta, .. } => meta,
            Pattern::Discard { meta, .. } => meta,
            Pattern::String { meta, .. } => meta,
            Pattern::Constructor { meta, .. } => meta,
        }
    }
}
