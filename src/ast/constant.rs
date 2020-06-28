use super::*;

pub type TypedConstant = Constant<Arc<Type>, String>;
pub type UntypedConstant = Constant<(), ()>;

#[derive(Debug, PartialEq, Clone)]
pub enum Constant<T, RecordTag> {
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

    Tuple {
        location: SrcSpan,
        elements: Vec<Self>,
    },

    List {
        location: SrcSpan,
        elements: Vec<Self>,
        typ: T,
    },

    Record {
        location: SrcSpan,
        module: Option<String>,
        name: String,
        args: Vec<CallArg<Self>>,
        tag: RecordTag,
        typ: T,
    },
}

impl TypedConstant {
    pub fn typ(&self) -> Arc<typ::Type> {
        match self {
            Constant::Int { .. } => crate::typ::int(),
            Constant::Float { .. } => crate::typ::float(),
            Constant::String { .. } => crate::typ::string(),
            Constant::List { typ, .. } => typ.clone(),
            Constant::Record { typ, .. } => typ.clone(),
            Constant::Tuple { elements, .. } => {
                crate::typ::tuple(elements.iter().map(|e| e.typ()).collect())
            }
        }
    }
}

impl<A, B> Constant<A, B> {
    pub fn location(&self) -> &SrcSpan {
        match self {
            Constant::Int { location, .. } => location,
            Constant::List { location, .. } => location,
            Constant::Float { location, .. } => location,
            Constant::Tuple { location, .. } => location,
            Constant::String { location, .. } => location,
            Constant::Record { location, .. } => location,
        }
    }
}
