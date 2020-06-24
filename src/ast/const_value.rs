use super::*;

pub type TypedConstValue = ConstValue<Arc<Type>>;
pub type UntypedConstValue = ConstValue<()>;
#[derive(Debug, PartialEq, Clone)]
pub enum ConstValue<T> {
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
        typ: std::marker::PhantomData<T>,
        elements: Vec<Self>,
    },
}

impl TypedConstValue {
    pub fn typ(&self) -> Arc<typ::Type> {
        match self {
            ConstValue::Int { .. } => crate::typ::int(),
            ConstValue::Float { .. } => crate::typ::float(),
            ConstValue::String { .. } => crate::typ::string(),
            ConstValue::Tuple { elements, .. } => {
                crate::typ::tuple(elements.iter().map(|e| e.typ()).collect())
            }
        }
    }
}
