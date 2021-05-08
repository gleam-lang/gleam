use super::*;
use crate::type_::{FieldMap, HasType};

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
        field_map: Option<FieldMap>,
    },

    BitString {
        location: SrcSpan,
        segments: Vec<BitStringSegment<Self, T>>,
    },
}

impl TypedConstant {
    pub fn type_(&self) -> Arc<Type> {
        match self {
            Constant::Int { .. } => crate::type_::int(),
            Constant::Float { .. } => crate::type_::float(),
            Constant::String { .. } => crate::type_::string(),
            Constant::List { typ, .. } => typ.clone(),
            Constant::Record { typ, .. } => typ.clone(),
            Constant::BitString { .. } => crate::type_::bit_string(),
            Constant::Tuple { elements, .. } => {
                crate::type_::tuple(elements.iter().map(|e| e.type_()).collect())
            }
        }
    }
}

impl HasType for TypedConstant {
    fn type_(&self) -> Arc<Type> {
        self.type_()
    }
}

impl<A, B> Constant<A, B> {
    pub fn location(&self) -> SrcSpan {
        match self {
            Constant::Int { location, .. }
            | Constant::List { location, .. }
            | Constant::Float { location, .. }
            | Constant::Tuple { location, .. }
            | Constant::String { location, .. }
            | Constant::Record { location, .. }
            | Constant::BitString { location, .. } => *location,
        }
    }

    pub fn is_simple(&self) -> bool {
        matches!(
            self,
            Self::Int { .. } | Self::Float { .. } | Self::String { .. }
        )
    }
}

impl<A, B> HasLocation for Constant<A, B> {
    fn location(&self) -> SrcSpan {
        self.location()
    }
}
