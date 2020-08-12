use super::*;
use crate::typ::HasType;

pub type TypedConstant = Constant<Arc<Type>, String>;
pub type UntypedConstant = Constant<(), ()>;

#[derive(Butcher, Debug, PartialEq, Clone)]
pub enum Constant<T, RecordTag> {
    Int {
        location: SrcSpan,
        #[butcher(as_deref)]
        value: String,
    },

    Float {
        location: SrcSpan,
        #[butcher(as_deref)]
        value: String,
    },

    String {
        location: SrcSpan,
        #[butcher(as_deref)]
        value: String,
    },

    Tuple {
        location: SrcSpan,
        #[butcher(as_deref, T: Clone, RecordTag: Clone)]
        elements: Vec<Constant<T, RecordTag>>,
    },

    List {
        location: SrcSpan,
        #[butcher(as_deref, T: Clone, RecordTag: Clone)]
        elements: Vec<Constant<T, RecordTag>>,
        typ: T,
    },

    Record {
        location: SrcSpan,
        module: Option<String>,
        #[butcher(as_deref)]
        name: String,
        #[butcher(as_deref, T: Clone, RecordTag: Clone)]
        args: Vec<CallArg<Constant<T, RecordTag>>>,
        tag: RecordTag,
        typ: T,
    },

    BitString {
        location: SrcSpan,
        #[butcher(as_deref, T: Clone, RecordTag: Clone)]
        segments: Vec<BitStringSegment<Constant<T, RecordTag>, T>>,
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
            Constant::BitString { .. } => crate::typ::bit_string(),
            Constant::Tuple { elements, .. } => {
                crate::typ::tuple(elements.iter().map(|e| e.typ()).collect())
            }
        }
    }
}

impl HasType for TypedConstant {
    fn typ(&self) -> Arc<typ::Type> {
        self.typ()
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
            Constant::BitString { location, .. } => location,
        }
    }

    pub fn is_simple(&self) -> bool {
        match self {
            Self::Int { .. } | Self::Float { .. } | Self::String { .. } => true,
            _ => false,
        }
    }
}

impl<A, B> HasLocation for Constant<A, B> {
    fn location(&self) -> &SrcSpan {
        self.location()
    }
}
