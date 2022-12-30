use crate::type_::{Type, ValueConstructorVariant};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Module {
    pub name: Vec<String>,
    pub types: HashMap<String, TypeConstructor>,
    pub values: HashMap<String, ValueConstructor>,
    pub accessors: HashMap<String, AccessorsHashMap>,
    pub package: String,
    pub types_constructors: HashMap<String, Vec<String>>,
}

// #[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
// pub enum Type {
//     Var(u64),
//     Tuple(Vec<Type>),
//     App {
//         name: String,
//         module: Vec<String>,
//         parameters: Vec<Type>
//     },
//     Fn {
//         arguments: Vec<Type>,
//         _return: Type
//     },
// }

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct TypeConstructor {
    pub type_: Arc<Type>,
    // TODO: convert this to an int as we only need to reconstruct type vars,
    // not other types
    // TODO: test
    pub parameters: Vec<Arc<Type>>,
    pub module: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct AccessorsHashMap {
    pub type_: Arc<Type>,
    pub accessors: HashMap<String, RecordAccessor>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct RecordAccessor {
    pub type_: Arc<Type>,
    pub index: u64,
    pub label: String,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct ValueConstructor {
    pub type_: Arc<Type>,
    pub variant: ValueConstructorVariant,
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq)]
pub struct SrcSpan {
    pub start: u32,
    pub end: u32,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct FieldHashMap {
    pub arity: u16,
    pub fields: HashMap<String, u32>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum Constant {
    Int(String),
    Float(String),
    String(String),
    Tuple(Vec<Constant>),
    BitString(Vec<BitStringSegment>),
    List {
        type_: Arc<Type>,
        elements: Vec<Constant>,
    },
    Record {
        args: Vec<Constant>,
        tag: String,
        type_: Arc<Type>,
    },
    Var {
        module: String,
        name: String,
        type_: Arc<Type>,
        constructor: ValueConstructor,
    },
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct BitStringSegment {
    pub value: Constant,
    pub options: Vec<BitStringSegmentOption>,
    pub type_: Arc<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
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
    Size { value: Constant, short_form: bool },
    Unit { value: u8, short_form: bool },
}
