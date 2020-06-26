use super::*;
use crate::typ::Type;

#[derive(Debug, PartialEq, Clone)]
pub enum TypedExpr {
    Int {
        location: SrcSpan,
        typ: Arc<Type>,
        value: String,
    },

    Float {
        location: SrcSpan,
        typ: Arc<Type>,
        value: String,
    },

    String {
        location: SrcSpan,
        typ: Arc<Type>,
        value: String,
    },

    Seq {
        typ: Arc<Type>,
        first: Box<Self>,
        then: Box<Self>,
    },

    Var {
        location: SrcSpan,
        constructor: ValueConstructor,
        name: String,
    },

    Fn {
        location: SrcSpan,
        typ: Arc<Type>,
        is_capture: bool,
        args: Vec<Arg<Arc<Type>>>,
        body: Box<Self>,
        return_annotation: Option<TypeAst>,
    },

    ListNil {
        location: SrcSpan,
        typ: Arc<Type>,
    },

    ListCons {
        location: SrcSpan,
        typ: Arc<Type>,
        head: Box<Self>,
        tail: Box<Self>,
    },

    Call {
        location: SrcSpan,
        typ: Arc<Type>,
        fun: Box<Self>,
        args: Vec<CallArg<Self>>,
    },

    BinOp {
        location: SrcSpan,
        typ: Arc<Type>,
        name: BinOp,
        left: Box<Self>,
        right: Box<Self>,
    },

    Pipe {
        location: SrcSpan,
        typ: Arc<Type>,
        left: Box<Self>,
        right: Box<Self>,
    },

    Let {
        location: SrcSpan,
        typ: Arc<Type>,
        value: Box<Self>,
        pattern: Pattern<PatternConstructor, Arc<Type>>,
        then: Box<Self>,
        kind: BindingKind,
    },

    Case {
        location: SrcSpan,
        typ: Arc<Type>,
        subjects: Vec<Self>,
        clauses: Vec<Clause<Self, PatternConstructor, Arc<Type>>>,
    },

    RecordAccess {
        location: SrcSpan,
        typ: Arc<Type>,
        label: String,
        index: u64,
        record: Box<Self>,
    },

    ModuleSelect {
        location: SrcSpan,
        typ: Arc<Type>,
        label: String,
        module_name: Vec<String>,
        module_alias: String,
        constructor: ModuleValueConstructor,
    },

    Tuple {
        location: SrcSpan,
        typ: Arc<Type>,
        elems: Vec<Self>,
    },

    TupleIndex {
        location: SrcSpan,
        typ: Arc<Type>,
        index: u64,
        tuple: Box<Self>,
    },

    Todo {
        location: SrcSpan,
        label: Option<String>,
        typ: Arc<Type>,
    },

    BitString {
        location: SrcSpan,
        typ: Arc<Type>,
        elems: Vec<TypedExprBinSegment>,
    },
}

impl TypedExpr {
    pub fn location(&self) -> &SrcSpan {
        match self {
            Self::Let { then, .. } => then.location(),
            Self::Seq { then, .. } => then.location(),
            Self::Fn { location, .. } => location,
            Self::Int { location, .. } => location,
            Self::Var { location, .. } => location,
            Self::Todo { location, .. } => location,
            Self::Case { location, .. } => location,
            Self::Call { location, .. } => location,
            Self::Pipe { location, .. } => location,
            Self::Float { location, .. } => location,
            Self::BinOp { location, .. } => location,
            Self::Tuple { location, .. } => location,
            Self::String { location, .. } => location,
            Self::ListNil { location, .. } => location,
            Self::ListCons { location, .. } => location,
            Self::TupleIndex { location, .. } => location,
            Self::ModuleSelect { location, .. } => location,
            Self::RecordAccess { location, .. } => location,
            Self::BitString { location, .. } => location,
        }
    }

    pub fn try_binding_location(&self) -> &SrcSpan {
        match self {
            Self::Let {
                kind: BindingKind::Try,
                location,
                ..
            } => location,

            Self::Let { then, .. } => then.try_binding_location(),
            Self::Seq { then, .. } => then.try_binding_location(),
            Self::Fn { location, .. } => location,
            Self::Int { location, .. } => location,
            Self::Var { location, .. } => location,
            Self::Todo { location, .. } => location,
            Self::Case { location, .. } => location,
            Self::Call { location, .. } => location,
            Self::Pipe { location, .. } => location,
            Self::Float { location, .. } => location,
            Self::BinOp { location, .. } => location,
            Self::Tuple { location, .. } => location,
            Self::String { location, .. } => location,
            Self::ListNil { location, .. } => location,
            Self::ListCons { location, .. } => location,
            Self::TupleIndex { location, .. } => location,
            Self::ModuleSelect { location, .. } => location,
            Self::RecordAccess { location, .. } => location,
            Self::BitString { location, .. } => location,
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
            Self::BitString { typ, .. } => typ.clone(),
        }
    }
}
