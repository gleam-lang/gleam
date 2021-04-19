use super::*;
use crate::{ast::*, pretty::*};

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

    pub fn compile<'a>(expression: &'a TypedExpr) -> Output<'a> {
        Self::new().expression(expression)
    }

    fn expression<'a>(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        let document = match expression {
            TypedExpr::String { value, .. } => Ok(string(value)),

            TypedExpr::Int { value, .. } => Ok(int(value)),
            TypedExpr::Float { value, .. } => Ok(float(value)),

            TypedExpr::List { .. } => unsupported("List"),

            TypedExpr::Tuple { elems, .. } => self.tuple(elems),
            TypedExpr::TupleIndex { .. } => unsupported("Tuple"),

            TypedExpr::Case { .. } => unsupported("Case"),

            TypedExpr::Call { .. } => unsupported("Function"),
            TypedExpr::Fn { .. } => unsupported("Function"),

            TypedExpr::RecordAccess { .. } => unsupported("Custom Record"),
            TypedExpr::RecordUpdate { .. } => unsupported("Function"),

            TypedExpr::Var { .. } => unsupported("Referencing variables"),
            TypedExpr::Seq { first, then, .. } => self.sequence(first, then),
            TypedExpr::Assignment { .. } => unsupported("Assigning variables"),

            TypedExpr::BinOp {
                name, left, right, ..
            } => self.bin_op(name, left, right),

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

    fn not_in_tail_position<'a, CompileFn>(&mut self, compile: CompileFn) -> Output<'a>
    where
        CompileFn: Fn(&mut Self) -> Output<'a>,
    {
        let tail = self.tail_position;
        self.tail_position = false;
        let result = compile(self);
        self.tail_position = tail;
        result
    }

    /// Wrap an expression in an immediately involked function expression if
    /// required due to being a JS statement
    fn wrap_expression<'a>(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        match expression {
            TypedExpr::Seq { .. } | TypedExpr::Assignment { .. } => {
                self.immediately_involked_function_expression(expression)
            }
            _ => self.expression(expression),
        }
    }

    /// Wrap an expression in an immediately involked function expression
    fn immediately_involked_function_expression<'a>(
        &mut self,
        expression: &'a TypedExpr,
    ) -> Output<'a> {
        let tail = self.tail_position;
        self.tail_position = true;
        let result = self.expression(expression);
        self.tail_position = tail;
        Ok(docvec!(
            docvec!("(() => {", break_("", " "), result?)
                .nest(INDENT)
                .group(),
            break_("", " "),
            "})()",
        ))
    }

    fn sequence<'a>(&mut self, first: &'a TypedExpr, then: &'a TypedExpr) -> Output<'a> {
        let first = self.not_in_tail_position(|gen| gen.expression(first))?;
        let then = self.expression(then)?;
        Ok(docvec![force_break(), first, ";", line(), then])
    }

    fn tuple<'a>(&mut self, elements: &'a [TypedExpr]) -> Output<'a> {
        self.not_in_tail_position(|gen| {
            array(elements.iter().map(|element| gen.wrap_expression(element)))
        })
    }

    fn bin_op<'a>(
        &mut self,
        name: &'a BinOp,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
    ) -> Output<'a> {
        match name {
            BinOp::And => unsupported("Boolean operator"),
            BinOp::Or => unsupported("Boolean operator"),
            BinOp::LtInt | BinOp::LtFloat => self.print_bin_op(left, right, "<"),
            BinOp::LtEqInt | BinOp::LtEqFloat => self.print_bin_op(left, right, "<="),
            // https://dmitripavlutin.com/how-to-compare-objects-in-javascript/
            BinOp::Eq => unsupported("Equality operator"),
            BinOp::NotEq => unsupported("Equality operator"),
            BinOp::GtInt | BinOp::GtFloat => self.print_bin_op(left, right, ">"),
            BinOp::GtEqInt | BinOp::GtEqFloat => self.print_bin_op(left, right, ">="),
            BinOp::AddInt | BinOp::AddFloat => self.print_bin_op(left, right, "+"),
            BinOp::SubInt | BinOp::SubFloat => self.print_bin_op(left, right, "-"),
            BinOp::MultInt | BinOp::MultFloat => self.print_bin_op(left, right, "*"),
            BinOp::DivInt => Ok(self.print_bin_op(left, right, "/")?.append(" | 0")),
            BinOp::DivFloat => self.print_bin_op(left, right, "/"),
            BinOp::ModuloInt => self.print_bin_op(left, right, "%"),
        }
    }

    fn print_bin_op<'a>(&mut self, left: &'a TypedExpr, right: &'a TypedExpr, op: &'a str) -> Output<'a> {
        let left = self.not_in_tail_position(|gen| gen.expression(left))?;
        let right = self.not_in_tail_position(|gen| gen.expression(right))?;

        Ok(left
            .append(" ")
            .append(op.to_doc())
            .append(" ")
            .append(right))
    }
}

fn int(value: &str) -> Document<'_> {
    value.to_doc()
}

fn float(value: &str) -> Document<'_> {
    value.to_doc()
}
pub fn constant_expression<'a, T, Y>(expression: &'a Constant<T, Y>) -> Output<'a> {
    match expression {
        Constant::Int { .. } => unsupported("Integer as constant"),
        Constant::Float { .. } => unsupported("Float as constant"),
        Constant::String { value, .. } => Ok(string(&value.as_str())),
        Constant::Tuple { elements, .. } => array(elements.iter().map(|e| constant_expression(&e))),
        Constant::List { .. } => unsupported("List as constant"),
        Constant::Record { .. } => unsupported("Record as constant"),
        Constant::BitString { .. } => unsupported("BitString as constant"),
    }
}

fn string(value: &str) -> Document<'_> {
    value.to_doc().surround("\"", "\"")
}

fn array<'a, Elements: Iterator<Item = Output<'a>>>(elements: Elements) -> Output<'a> {
    let elements = Itertools::intersperse(elements, Ok(break_(",", ", ")))
        .collect::<Result<Vec<_>, _>>()?
        .to_doc();
    Ok(docvec![
        "[",
        docvec![break_("", ""), elements].nest(INDENT),
        break_(",", ""),
        "]"
    ]
    .group())
}
