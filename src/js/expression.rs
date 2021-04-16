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

    fn expression<'a>(&mut self, expression: &'a TypedExpr) -> Result<Document<'a>> {
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

            TypedExpr::Var { .. } => unsupported("Referencing variables"),
            TypedExpr::Seq { first, then, .. } => self.sequence(first, then),
            TypedExpr::Assignment { .. } => unsupported("Assigning variables"),

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

    fn not_in_tail_position<'a, CompileFn>(&mut self, compile: CompileFn) -> Result<Document<'a>>
    where
        CompileFn: Fn(&mut Self) -> Result<Document<'a>>,
    {
        let tail = self.tail_position;
        self.tail_position = false;
        let result = compile(self);
        self.tail_position = tail;
        result
    }

    fn sequence<'a>(&mut self, first: &'a TypedExpr, then: &'a TypedExpr) -> Result<Document<'a>> {
        let first = self.not_in_tail_position(|gen| gen.expression(first))?;
        let then = self.expression(then)?;
        Ok(docvec![first, ";", line(), then])
    }
}

fn string(value: &str) -> Document<'_> {
    value.to_doc().surround("\"", "\"")
}
