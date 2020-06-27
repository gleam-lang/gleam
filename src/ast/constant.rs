use super::*;

pub type TypedConstant = Constant<Arc<Type>>;
pub type UntypedConstant = Constant<()>;
#[derive(Debug, PartialEq, Clone)]
pub enum Constant<T> {
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
}

impl TypedConstant {
    pub fn typ(&self) -> Arc<typ::Type> {
        match self {
            Constant::Int { .. } => crate::typ::int(),
            Constant::Float { .. } => crate::typ::float(),
            Constant::String { .. } => crate::typ::string(),
            Constant::List { typ, .. } => typ.clone(),
            Constant::Tuple { elements, .. } => {
                crate::typ::tuple(elements.iter().map(|e| e.typ()).collect())
            }
        }
    }
}

impl<T> Constant<T> {
    pub fn location(&self) -> &SrcSpan {
        match self {
            Constant::Int { location, .. } => location,
            Constant::Float { location, .. } => location,
            Constant::String { location, .. } => location,
            Constant::List { location, .. } => location,
            Constant::Tuple { location, .. } => location,
        }
    }
}
