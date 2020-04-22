use super::*;

#[derive(Debug, PartialEq, Clone)]
pub enum UntypedExpr {
    Int {
        location: SrcSpan,
        value: String,
    },

    Float {
        location: SrcSpan,
        value: String,
    },

    String {
        location: SrcSpan,
        value: String,
    },

    Seq {
        first: Box<Self>,
        then: Box<Self>,
    },

    Var {
        location: SrcSpan,
        name: String,
    },

    Fn {
        location: SrcSpan,
        is_capture: bool,
        args: Vec<Arg<()>>,
        body: Box<Self>,
        return_annotation: Option<TypeAst>,
    },

    ListNil {
        location: SrcSpan,
    },

    ListCons {
        location: SrcSpan,
        head: Box<Self>,
        tail: Box<Self>,
    },

    Call {
        location: SrcSpan,
        fun: Box<Self>,
        args: Vec<CallArg<Self>>,
    },

    BinOp {
        location: SrcSpan,
        name: BinOp,
        left: Box<Self>,
        right: Box<Self>,
    },

    Pipe {
        location: SrcSpan,
        left: Box<Self>,
        right: Box<Self>,
    },

    Let {
        location: SrcSpan,
        value: Box<Self>,
        pattern: Pattern<()>,
        then: Box<Self>,
        assert: bool,
    },

    Case {
        location: SrcSpan,
        subjects: Vec<Self>,
        clauses: Vec<Clause<Self, (), ()>>,
    },

    FieldAccess {
        location: SrcSpan,
        label: String,
        container: Box<Self>,
    },

    Tuple {
        location: SrcSpan,
        elems: Vec<Self>,
    },

    TupleIndex {
        location: SrcSpan,
        index: u64,
        tuple: Box<Self>,
    },

    Todo {
        location: SrcSpan,
    },
}

impl UntypedExpr {
    pub fn location(&self) -> &SrcSpan {
        match self {
            Self::Fn { location, .. } => location,
            Self::Int { location, .. } => location,
            Self::Seq { then, .. } => then.location(),
            Self::Var { location, .. } => location,
            Self::ListNil { location, .. } => location,
            Self::Let { then, .. } => then.location(),
            Self::Todo { location, .. } => location,
            Self::Case { location, .. } => location,
            Self::ListCons { location, .. } => location,
            Self::Call { location, .. } => location,
            Self::Pipe { location, .. } => location,
            Self::Float { location, .. } => location,
            Self::BinOp { location, .. } => location,
            Self::String { location, .. } => location,
            Self::Tuple { location, .. } => location,
            Self::TupleIndex { location, .. } => location,
            Self::FieldAccess { location, .. } => location,
        }
    }

    pub fn start_byte_index(&self) -> usize {
        match self {
            Self::Seq { first, .. } => first.location().start,
            Self::Let { location, .. } => location.start,
            _ => self.location().start,
        }
    }
}
