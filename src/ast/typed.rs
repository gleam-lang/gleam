use super::*;
use crate::typ::{HasType, Type};

#[derive(Butcher, Debug, PartialEq, Clone)]
pub enum TypedExpr {
    Int {
        location: SrcSpan,
        #[butcher(copy)]
        typ: Arc<Type>,
        #[butcher(as_deref)]
        value: String,
    },

    Float {
        location: SrcSpan,
        #[butcher(copy)]
        typ: Arc<Type>,
        #[butcher(as_deref)]
        value: String,
    },

    String {
        location: SrcSpan,
        #[butcher(copy)]
        typ: Arc<Type>,
        #[butcher(as_deref)]
        value: String,
    },

    Seq {
        #[butcher(copy)]
        typ: Arc<Type>,
        #[butcher(unbox)]
        first: Box<TypedExpr>,
        #[butcher(unbox)]
        then: Box<TypedExpr>,
    },

    Var {
        location: SrcSpan,
        #[butcher(rebutcher)]
        constructor: ValueConstructor,
        #[butcher(as_deref)]
        name: String,
    },

    Fn {
        location: SrcSpan,
        #[butcher(copy)]
        typ: Arc<Type>,
        #[butcher(copy)]
        is_capture: bool,
        #[butcher(as_deref)]
        args: Vec<Arg<Arc<Type>>>,
        #[butcher(unbox)]
        body: Box<TypedExpr>,
        return_annotation: Option<TypeAst>,
    },

    ListNil {
        location: SrcSpan,
        #[butcher(copy)]
        typ: Arc<Type>,
    },

    ListCons {
        location: SrcSpan,
        #[butcher(copy)]
        typ: Arc<Type>,
        #[butcher(unbox)]
        head: Box<TypedExpr>,
        #[butcher(unbox)]
        tail: Box<TypedExpr>,
    },

    Call {
        location: SrcSpan,
        #[butcher(copy)]
        typ: Arc<Type>,
        #[butcher(unbox)]
        fun: Box<TypedExpr>,
        #[butcher(as_deref)]
        args: Vec<CallArg<TypedExpr>>,
    },

    BinOp {
        location: SrcSpan,
        #[butcher(copy)]
        typ: Arc<Type>,
        name: BinOp,
        #[butcher(unbox)]
        left: Box<TypedExpr>,
        #[butcher(unbox)]
        right: Box<TypedExpr>,
    },

    Pipe {
        location: SrcSpan,
        #[butcher(copy)]
        typ: Arc<Type>,
        #[butcher(unbox)]
        left: Box<TypedExpr>,
        #[butcher(unbox)]
        right: Box<TypedExpr>,
    },

    Let {
        location: SrcSpan,
        #[butcher(copy)]
        typ: Arc<Type>,
        #[butcher(unbox)]
        value: Box<TypedExpr>,
        pattern: Pattern<PatternConstructor, Arc<Type>>,
        #[butcher(unbox)]
        then: Box<TypedExpr>,
        #[butcher(copy)]
        kind: BindingKind,
    },

    Case {
        location: SrcSpan,
        #[butcher(copy)]
        typ: Arc<Type>,
        #[butcher(as_deref)]
        subjects: Vec<TypedExpr>,
        #[butcher(as_deref)]
        clauses: Vec<Clause<TypedExpr, PatternConstructor, Arc<Type>, String>>,
    },

    RecordAccess {
        location: SrcSpan,
        #[butcher(copy)]
        typ: Arc<Type>,
        #[butcher(as_deref)]
        label: String,
        index: u64,
        #[butcher(unbox)]
        record: Box<TypedExpr>,
    },

    ModuleSelect {
        location: SrcSpan,
        #[butcher(copy)]
        typ: Arc<Type>,
        #[butcher(as_deref)]
        label: String,
        #[butcher(as_deref)]
        module_name: Vec<String>,
        #[butcher(as_deref)]
        module_alias: String,
        #[butcher(rebutcher)]
        constructor: ModuleValueConstructor,
    },

    Tuple {
        location: SrcSpan,
        #[butcher(copy)]
        typ: Arc<Type>,
        #[butcher(as_deref)]
        elems: Vec<TypedExpr>,
    },

    TupleIndex {
        location: SrcSpan,
        #[butcher(copy)]
        typ: Arc<Type>,
        index: u64,
        #[butcher(unbox)]
        tuple: Box<TypedExpr>,
    },

    Todo {
        location: SrcSpan,
        label: Option<String>,
        #[butcher(copy)]
        typ: Arc<Type>,
    },

    BitString {
        location: SrcSpan,
        #[butcher(copy)]
        typ: Arc<Type>,
        #[butcher(as_deref)]
        segments: Vec<TypedExprBitStringSegment>,
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
}

impl HasLocation for TypedExpr {
    fn location(&self) -> &SrcSpan {
        self.location()
    }
}

impl TypedExpr {
    fn typ(&self) -> Arc<typ::Type> {
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

impl HasType for TypedExpr {
    fn typ(&self) -> Arc<typ::Type> {
        self.typ()
    }
}
