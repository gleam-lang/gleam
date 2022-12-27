use crate::type_::{Type, ValueConstructorVariant};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
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

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct TypeConstructor {
    pub _type: Arc<Type>,
    // TODO: convert this to an int as we only need to reconstruct type vars,
    // not other types
    // TODO: test
    pub parameters: Vec<Arc<Type>>,
    pub module: Vec<String>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AccessorsHashMap {
    pub _type: Arc<Type>,
    pub accessors: HashMap<String, RecordAccessor>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct RecordAccessor {
    pub _type: Arc<Type>,
    pub index: u64,
    pub label: String,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ValueConstructor {
    pub _type: Arc<Type>,
    pub variant: ValueConstructorVariant,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct SrcSpan {
    pub start: u32,
    pub end: u32,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct FieldHashMap {
    pub arity: u16,
    pub fields: HashMap<String, u32>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Constant {
    Int(String),
    Float(String),
    String(String),
    Tuple(Vec<Constant>),
    BitString(Vec<BitStringSegment>),
    List {
        _type: Arc<Type>,
        elements: Vec<Constant>,
    },
    Record {
        args: Vec<Constant>,
        tag: String,
        _type: Arc<Type>,
    },
    Var {
        module: String,
        name: String,
        _type: Arc<Type>,
        constructor: ValueConstructor,
    },
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct BitStringSegment {
    pub value: Constant,
    pub options: Vec<BitStringSegmentOption>,
    pub _type: Arc<Type>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
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
