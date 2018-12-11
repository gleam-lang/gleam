#![allow(dead_code)]

use super::ast::Meta;

#[derive(Debug)]
pub enum Pattern {
    Int {
        meta: Meta,
        value: i64,
    },

    Float {
        meta: Meta,
        value: f64,
    },

    Atom {
        meta: Meta,
        value: String,
    },

    String {
        meta: Meta,
        value: String,
    },

    Var {
        meta: Meta,
        name: String,
    },

    Tuple {
        meta: Meta,
        elems: Vec<Pattern>,
    },

    Nil {
        meta: Meta,
    },

    Cons {
        meta: Meta,
        head: Box<Pattern>,
        tail: Box<Pattern>,
    },

    RecordNil {
        meta: Meta,
    },

    RecordCons {
        meta: Meta,
        label: String,
        value: Box<Pattern>,
        tail: Box<Pattern>,
    },

    Enum {
        meta: Meta,
        name: String,
        args: Vec<Pattern>,
    },
}

impl Pattern {
    pub fn meta(&self) -> &Meta {
        match self {
            Pattern::Int { meta, .. } => meta,
            Pattern::Var { meta, .. } => meta,
            Pattern::Nil { meta, .. } => meta,
            Pattern::Cons { meta, .. } => meta,
            Pattern::Atom { meta, .. } => meta,
            Pattern::Enum { meta, .. } => meta,
            Pattern::Float { meta, .. } => meta,
            Pattern::Tuple { meta, .. } => meta,
            Pattern::String { meta, .. } => meta,
            Pattern::RecordNil { meta, .. } => meta,
            Pattern::RecordCons { meta, .. } => meta,
        }
    }
}
