use super::*;
use crate::type_::{HasType, Type};

use lazy_static::lazy_static;

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

    Sequence {
        location: SrcSpan,
        expressions: Vec<Self>,
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

    List {
        location: SrcSpan,
        typ: Arc<Type>,
        elements: Vec<Self>,
        tail: Option<Box<Self>>,
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

    PipeLast {
        location: SrcSpan,
        typ: Arc<Type>,
        left: Box<Self>,
        right: Box<Self>,
    },

    Assignment {
        location: SrcSpan,
        typ: Arc<Type>,
        value: Box<Self>,
        pattern: Pattern<PatternConstructor, Arc<Type>>,
        kind: AssignmentKind,
    },

    Try {
        location: SrcSpan,
        typ: Arc<Type>,
        value: Box<Self>,
        then: Box<Self>,
        pattern: Pattern<PatternConstructor, Arc<Type>>,
    },

    Case {
        location: SrcSpan,
        typ: Arc<Type>,
        subjects: Vec<Self>,
        clauses: Vec<Clause<Self, PatternConstructor, Arc<Type>, String>>,
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
        segments: Vec<TypedExprBitStringSegment>,
    },

    RecordUpdate {
        location: SrcSpan,
        typ: Arc<Type>,
        spread: Box<Self>,
        args: Vec<TypedRecordUpdateArg>,
    },
}

impl TypedExpr {
    pub fn non_zero_compile_time_number(&self) -> bool {
        use regex::Regex;
        lazy_static! {
            static ref NON_ZERO: Regex = Regex::new(r"[1-9]").expect("NON_ZERO regex");
        }

        matches!(
            self,
            Self::Int{ value, .. } | Self::Float { value, .. } if NON_ZERO.is_match(value)
        )
    }

    pub fn location(&self) -> SrcSpan {
        match self {
            Self::Try { then, .. } => then.location(),
            Self::Fn { location, .. }
            | Self::Int { location, .. }
            | Self::Var { location, .. }
            | Self::Todo { location, .. }
            | Self::Case { location, .. }
            | Self::Call { location, .. }
            | Self::PipeLast { location, .. }
            | Self::List { location, .. }
            | Self::Float { location, .. }
            | Self::BinOp { location, .. }
            | Self::Tuple { location, .. }
            | Self::String { location, .. }
            | Self::Sequence { location, .. }
            | Self::BitString { location, .. }
            | Self::Assignment { location, .. }
            | Self::TupleIndex { location, .. }
            | Self::ModuleSelect { location, .. }
            | Self::RecordAccess { location, .. }
            | Self::RecordUpdate { location, .. } => *location,
        }
    }

    /// Returns `true` if the typed expr is [`Assignment`].
    pub fn is_assignment(&self) -> bool {
        matches!(self, Self::Assignment { .. })
    }
}

impl HasLocation for TypedExpr {
    fn location(&self) -> SrcSpan {
        self.location()
    }
}

impl TypedExpr {
    fn type_(&self) -> Arc<Type> {
        match self {
            Self::Var { constructor, .. } => constructor.type_.clone(),
            Self::Try { then, .. } => then.type_(),
            Self::Fn { typ, .. } => typ.clone(),
            Self::Int { typ, .. } => typ.clone(),
            Self::Todo { typ, .. } => typ.clone(),
            Self::Case { typ, .. } => typ.clone(),
            Self::List { typ, .. } => typ.clone(),
            Self::Call { typ, .. } => typ.clone(),
            Self::PipeLast { typ, .. } => typ.clone(),
            Self::Float { typ, .. } => typ.clone(),
            Self::BinOp { typ, .. } => typ.clone(),
            Self::Tuple { typ, .. } => typ.clone(),
            Self::String { typ, .. } => typ.clone(),
            Self::TupleIndex { typ, .. } => typ.clone(),
            Self::Assignment { typ, .. } => typ.clone(),
            Self::ModuleSelect { typ, .. } => typ.clone(),
            Self::RecordAccess { typ, .. } => typ.clone(),
            Self::BitString { typ, .. } => typ.clone(),
            Self::RecordUpdate { typ, .. } => typ.clone(),
            Self::Sequence { expressions, .. } => expressions
                .last()
                .map(TypedExpr::type_)
                .unwrap_or_else(type_::nil),
        }
    }

    pub fn is_literal(&self) -> bool {
        matches!(
            self,
            Self::Int { .. }
                | Self::List { .. }
                | Self::Float { .. }
                | Self::Tuple { .. }
                | Self::String { .. }
                | Self::BitString { .. }
        )
    }
}

impl HasType for TypedExpr {
    fn type_(&self) -> Arc<Type> {
        self.type_()
    }
}
