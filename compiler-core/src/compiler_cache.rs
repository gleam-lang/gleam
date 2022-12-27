use std::iter::Map;

pub struct Module {
    pub name: Vec<String>,
    pub types: Vec<Map<String, TypeConstructor>>,
    pub values: Vec<Map<String, ValueConstructor>>,
    pub accessors: Vec<Map<String, AccessorsMap>>,
    pub package: String,
    pub types_constructors: Vec<Map<String, Vec<String>>>
}

pub enum Type {
    Var(u64),
    Tuple(Vec<Type>),
    App {
        name: String,
        module: Vec<String>,
        parameters: Vec<Type>
    },
    Fn {
        arguments: Vec<Type>,
        _return: Type
    },
}

pub struct TypeConstructor {
    pub _type: Type,
    // TODO: convert this to an int as we only need to reconstruct type vars,
    // not other types
    // TODO: test
    pub parameters: Vec<Type>,
    pub module: Vec<String>
}

pub struct AccessorsMap {
    pub _type: Type,
    pub accessors: Vec<Map<String, RecordAccessor>>
}

pub struct RecordAccessor {
    pub _type: Type,
    pub index: u16,
    pub label: String
}

pub struct ValueConstructor {
    pub _type: Type,
    pub variant: ValueConstructorVariant
}

pub enum ValueConstructorVariant {
    ModuleConstant {
        literal: Constant,
        location: SrcSpan,
        module: String
    },
    ModuleFn {
        name: String,
        field_map: Option<FieldMap>,
        module: Vec<String>,
        arity: u16,
        location: SrcSpan
    },
    Record {
        name: String,
        field_map: Option<FieldMap>,
        module: Vec<String>,
        arity: u16,
        location: SrcSpan,
        constructors_count: u16
    }
}

pub struct SrcSpan {
    pub start: u32,
    pub end: u32
}

pub struct FieldMap {
    pub arity: u16,
    pub fields: Vec<Map<String, u32>>
}

pub enum Constant {
    Int(String),
    Float(String),
    String(String),
    Tuple(Vec<Constant>),
    BitString(Vec<BitStringSegment>),
    List {
        _type: Type,
        elements: Vec<Constant>
    },
    Record {
        args: Vec<Constant>,
        tag: String,
        _type: Type
    },
    Var {
        module: String,
        name: String,
        _type: Type,
        constructor: ValueConstructor
    }
}

pub struct BitStringSegment {
    pub value: Constant,
    pub options: Vec<BitStringSegmentOption>,
    pub _type: Type
}

pub enum BitStringSegmentOption {
    Binary,
    Integer,
    Float,
    BitString,
    Utf8,
    Utf16,
    Utf32,
    Utf8Codepoint,
    Utf16Codepoint,
    Utf32Codepoint,
    Signed,
    Unsigned,
    Big,
    Little,
    Native,
    Size {
        value: Constant,
        short_form: bool
    },
    Unit {
        value: u8,
        short_form: bool
    }
}