use super::*;

pub type TypedConstValue = ConstValue<Arc<Type>>;
pub type UntypedConstValue = ConstValue<()>;
#[derive(Debug, PartialEq, Clone)]
pub enum ConstValue<T> {
    Int {
        location: SrcSpan,
        typ: T,
        value: String,
    },
    Float {
        location: SrcSpan,
        typ: T,
        value: String,
    },
    String {
        location: SrcSpan,
        typ: T,
        value: String,
    },
}

impl TypedConstValue {
    pub fn typ(&self) -> Arc<typ::Type> {
        match self {
            TypedConstValue::Int { typ, .. } => typ.clone(),
            TypedConstValue::Float { typ, .. } => typ.clone(),
            TypedConstValue::String { typ, .. } => typ.clone(),
        }
    }

    pub fn location(&self) -> &SrcSpan {
        match self {
            TypedConstValue::Int { location, .. } => location,
            TypedConstValue::Float { location, .. } => location,
            TypedConstValue::String { location, .. } => location,
        }
    }
}
