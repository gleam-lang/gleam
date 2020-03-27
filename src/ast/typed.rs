use super::*;
use crate::typ::Type;

#[derive(Debug, PartialEq, Clone)]
pub enum TypedExpr {
    Int {
        meta: Meta,
        typ: Arc<Type>,
        value: i64,
    },

    Float {
        meta: Meta,
        typ: Arc<Type>,
        value: f64,
    },

    String {
        meta: Meta,
        typ: Arc<Type>,
        value: String,
    },

    Seq {
        typ: Arc<Type>,
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
        typ: Arc<Type>,
        is_capture: bool,
        args: Vec<Arg>,
        body: Box<Self>,
        return_annotation: Option<TypeAst>,
    },

    ListNil {
        meta: Meta,
        typ: Arc<Type>,
    },

    ListCons {
        meta: Meta,
        typ: Arc<Type>,
        head: Box<Self>,
        tail: Box<Self>,
    },

    Call {
        meta: Meta,
        typ: Arc<Type>,
        fun: Box<Self>,
        args: Vec<CallArg<Self>>,
    },

    BinOp {
        meta: Meta,
        typ: Arc<Type>,
        name: BinOp,
        left: Box<Self>,
        right: Box<Self>,
    },

    Pipe {
        meta: Meta,
        typ: Arc<Type>,
        left: Box<Self>,
        right: Box<Self>,
    },

    Let {
        meta: Meta,
        typ: Arc<Type>,
        value: Box<Self>,
        pattern: Pattern<PatternConstructor>,
        then: Box<Self>,
    },

    Case {
        meta: Meta,
        typ: Arc<Type>,
        subjects: Vec<Self>,
        clauses: Vec<Clause<Self, PatternConstructor, Arc<Type>>>,
    },

    RecordAccess {
        meta: Meta,
        typ: Arc<Type>,
        label: String,
        index: u64,
        record: Box<Self>,
    },

    ModuleSelect {
        meta: Meta,
        typ: Arc<Type>,
        label: String,
        module_name: Vec<String>,
        module_alias: String,
        constructor: ModuleValueConstructor,
    },

    Tuple {
        meta: Meta,
        typ: Arc<Type>,
        elems: Vec<Self>,
    },

    TupleIndex {
        meta: Meta,
        typ: Arc<Type>,
        index: u64,
        tuple: Box<Self>,
    },

    Todo {
        meta: Meta,
        typ: Arc<Type>,
    },
}

impl TypedExpr {
    pub fn meta(&self) -> &Meta {
        match self {
            Self::Fn { meta, .. } => meta,
            Self::Int { meta, .. } => meta,
            Self::Seq { then, .. } => then.meta(),
            Self::Var { meta, .. } => meta,
            Self::ListNil { meta, .. } => meta,
            Self::Let { then, .. } => then.meta(),
            Self::Todo { meta, .. } => meta,
            Self::Case { meta, .. } => meta,
            Self::ListCons { meta, .. } => meta,
            Self::Call { meta, .. } => meta,
            Self::Pipe { meta, .. } => meta,
            Self::Float { meta, .. } => meta,
            Self::BinOp { meta, .. } => meta,
            Self::Tuple { meta, .. } => meta,
            Self::String { meta, .. } => meta,
            Self::TupleIndex { meta, .. } => meta,
            Self::ModuleSelect { meta, .. } => meta,
            Self::RecordAccess { meta, .. } => meta,
        }
    }

    pub fn typ(&self) -> Arc<typ::Type> {
        match self {
            Self::Fn { typ, .. } => typ.clone(),
            Self::ListNil { typ, .. } => typ.clone(),
            Self::Let { typ, .. } => typ.clone(),
            Self::Int { typ, .. } => typ.clone(),
            Self::Seq { then, .. } => then.typ(),
            Self::Todo { typ, .. } => typ.clone(),
            Self::Case { typ, .. } => typ.clone(),
            Self::ListCons { typ, .. } => typ.clone(),
            Self::Call { typ, .. } => typ.clone(),
            Self::Pipe { typ, .. } => typ.clone(),
            Self::Float { typ, .. } => typ.clone(),
            Self::BinOp { typ, .. } => typ.clone(),
            Self::Tuple { typ, .. } => typ.clone(),
            Self::String { typ, .. } => typ.clone(),
            Self::TupleIndex { typ, .. } => typ.clone(),
            Self::Var { constructor, .. } => constructor.typ.clone(),
            Self::ModuleSelect { typ, .. } => typ.clone(),
            Self::RecordAccess { typ, .. } => typ.clone(),
        }
    }
}
