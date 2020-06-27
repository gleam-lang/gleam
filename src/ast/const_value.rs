use super::*;

pub type TypedConstValue = ConstValue<Arc<Type>, String>;
pub type UntypedConstValue = ConstValue<(), ()>;

#[derive(Debug, PartialEq, Clone)]
pub enum ConstValue<T, RecordTag> {
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
        elements: Vec<Self>,
        name: String,
        tag: RecordTag,
        typ: T,
    },
}

impl TypedConstValue {
    pub fn typ(&self) -> Arc<typ::Type> {
        match self {
            ConstValue::Int { .. } => crate::typ::int(),
            ConstValue::Float { .. } => crate::typ::float(),
            ConstValue::String { .. } => crate::typ::string(),
            ConstValue::List { typ, .. } => typ.clone(),
            ConstValue::Record { typ, .. } => typ.clone(),
            ConstValue::Tuple { elements, .. } => {
                crate::typ::tuple(elements.iter().map(|e| e.typ()).collect())
            }
        }
    }
}

impl<T, N> ConstValue<T, N> {
    pub fn location(&self) -> &SrcSpan {
        match self {
            ConstValue::Int { location, .. } => location,
            ConstValue::List { location, .. } => location,
            ConstValue::Float { location, .. } => location,
            ConstValue::Tuple { location, .. } => location,
            ConstValue::String { location, .. } => location,
            ConstValue::Record { location, .. } => location,
        }
    }
}
