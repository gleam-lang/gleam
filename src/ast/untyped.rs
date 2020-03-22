use super::*;

#[derive(Debug, PartialEq, Clone)]
pub enum UntypedExpr {
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

    Seq {
        first: Box<Self>,
        then: Box<Self>,
    },

    Var {
        meta: Meta,
        name: String,
    },

    Fn {
        meta: Meta,
        is_capture: bool,
        args: Vec<Arg>,
        body: Box<Self>,
        return_annotation: Option<TypeAst>,
    },

    Nil {
        meta: Meta,
    },

    Cons {
        meta: Meta,
        head: Box<Self>,
        tail: Box<Self>,
    },

    Call {
        meta: Meta,
        fun: Box<Self>,
        args: Vec<CallArg<Self>>,
    },

    BinOp {
        meta: Meta,
        name: BinOp,
        left: Box<Self>,
        right: Box<Self>,
    },

    Pipe {
        meta: Meta,
        left: Box<Self>,
        right: Box<Self>,
    },

    Let {
        meta: Meta,
        value: Box<Self>,
        pattern: Pattern<()>,
        then: Box<Self>,
    },

    Case {
        meta: Meta,
        subjects: Vec<Self>,
        clauses: Vec<Clause<Self, (), ()>>,
    },

    FieldAccess {
        meta: Meta,
        label: String,
        container: Box<Self>,
    },

    Tuple {
        meta: Meta,
        elems: Vec<Self>,
    },

    TupleIndex {
        meta: Meta,
        index: u64,
        tuple: Box<Self>,
    },

    Todo {
        meta: Meta,
    },
}

impl UntypedExpr {
    pub fn meta(&self) -> &Meta {
        match self {
            Self::Fn { meta, .. } => meta,
            Self::Int { meta, .. } => meta,
            Self::Seq { then, .. } => then.meta(),
            Self::Var { meta, .. } => meta,
            Self::Nil { meta, .. } => meta,
            Self::Let { then, .. } => then.meta(),
            Self::Todo { meta, .. } => meta,
            Self::Case { meta, .. } => meta,
            Self::Cons { meta, .. } => meta,
            Self::Call { meta, .. } => meta,
            Self::Pipe { meta, .. } => meta,
            Self::Float { meta, .. } => meta,
            Self::BinOp { meta, .. } => meta,
            Self::String { meta, .. } => meta,
            Self::Tuple { meta, .. } => meta,
            Self::TupleIndex { meta, .. } => meta,
            Self::FieldAccess { meta, .. } => meta,
        }
    }
}
