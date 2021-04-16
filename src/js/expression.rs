use super::*;
use crate::{ast::*, pretty::*, Result};

#[derive(Debug)]
pub struct Generator {
    tail_position: bool,
}

impl Generator {
    pub fn new() -> Self {
        Self {
            tail_position: true,
        }
    }

    pub fn compile<'a>(expression: &'a TypedExpr) -> Result<Document<'a>> {
        Self::new().expression(expression)
    }

    fn expression<'a>(&self, expression: &'a TypedExpr) -> Result<Document<'a>> {
        let document = match expression {
            TypedExpr::String { value, .. } => Ok(string(value)),

            TypedExpr::Int { .. } => unsupported("Integer values"),
            TypedExpr::Float { .. } => unsupported("Float values"),

            TypedExpr::List { .. } => unsupported("List"),

            TypedExpr::Tuple { .. } => unsupported("Tuple"),
            TypedExpr::TupleIndex { .. } => unsupported("Tuple"),

            TypedExpr::Case { .. } => unsupported("Case"),

            TypedExpr::Call { .. } => unsupported("Function"),
            TypedExpr::Fn { .. } => unsupported("Function"),

            TypedExpr::RecordAccess { .. } => unsupported("Custom Record"),
            TypedExpr::RecordUpdate { .. } => unsupported("Function"),

            TypedExpr::Var { .. } => unsupported("Bindings"),
            TypedExpr::Seq { .. } => unsupported("Bindings"),
            TypedExpr::Assignment { .. } => unsupported("Bindings"),

            TypedExpr::BinOp { .. } => unsupported("Binary operation"),

            TypedExpr::Todo { .. } => unsupported("todo keyword"),

            TypedExpr::BitString { .. } => unsupported("Bitstring"),

            TypedExpr::Pipe { .. } => unsupported("Pipe"),

            TypedExpr::ModuleSelect { .. } => unsupported("Module function call"),
        }?;
        Ok(match expression {
            TypedExpr::Seq { .. } | TypedExpr::Assignment { .. } => document,
            _ => match self.tail_position {
                true => docvec!["return ", document, ";"],
                _ => document,
            },
        })
    }
}

fn string(value: &str) -> Document<'_> {
    value.to_doc().surround("\"", "\"")
}
