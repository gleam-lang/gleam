use super::*;
use crate::{
    ast::*,
    pretty::*,
    type_::{ValueConstructor, ValueConstructorVariant},
};

#[derive(Debug)]
pub struct Generator<'module> {
    tail_position: bool,
    // We register whether float division is used within an expression so that
    // the module generator can output a suitable function if it is needed.
    float_division_used: &'module mut bool,
    object_equality_used: &'module mut bool,
}

impl<'module> Generator<'module> {
    pub fn new(float_division_used: &'module mut bool, object_equality_used: &'module mut bool) -> Self {
        Self {
            tail_position: true,
            float_division_used,
            object_equality_used
        }
    }

    pub fn expression<'a>(&mut self, expression: &'a TypedExpr) -> Output<'a> {
        let document = match expression {
            TypedExpr::String { value, .. } => Ok(string(value)),

            TypedExpr::Int { value, .. } => Ok(int(value)),
            TypedExpr::Float { value, .. } => Ok(float(value)),

            TypedExpr::List { .. } => unsupported("List"),

            TypedExpr::Tuple { elems, .. } => self.tuple(elems),
            TypedExpr::TupleIndex { tuple, index, .. } => self.tuple_index(tuple, *index),

            TypedExpr::Case { .. } => unsupported("Case"),

            TypedExpr::Call { .. } => unsupported("Function"),
            TypedExpr::Fn { .. } => unsupported("Function"),

            TypedExpr::RecordAccess { .. } => unsupported("Custom Record"),
            TypedExpr::RecordUpdate { .. } => unsupported("Function"),

            TypedExpr::Var {
                name, constructor, ..
            } => self.variable(name, constructor),
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

    fn variable<'a>(&mut self, _name: &'a str, constructor: &'a ValueConstructor) -> Output<'a> {
        match &constructor.variant {
            ValueConstructorVariant::Record { name, .. }
                if constructor.type_.is_bool() && name == "True" =>
            {
                Ok("true".to_doc())
            }
            ValueConstructorVariant::Record { name, .. }
                if constructor.type_.is_bool() && name == "False" =>
            {
                Ok("false".to_doc())
            }
            ValueConstructorVariant::Record { .. } if constructor.type_.is_nil() => {
                Ok("undefined".to_doc())
            }
            _ => unsupported("Referencing variables"),
        }
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

    fn tuple_index<'a>(&mut self, tuple: &'a TypedExpr, index: u64) -> Output<'a> {
        self.not_in_tail_position(|gen| {
            let tuple = gen.wrap_expression(tuple)?;
            Ok(docvec![tuple, Document::String(format!("[{}]", index))])
        })
    }

    // TODO: handle precedence rules
    fn bin_op<'a>(
        &mut self,
        name: &'a BinOp,
        left: &'a TypedExpr,
        right: &'a TypedExpr,
    ) -> Output<'a> {
        let left = self.not_in_tail_position(|gen| gen.expression(left))?;
        let right = self.not_in_tail_position(|gen| gen.expression(right))?;
        match name {
            BinOp::And => self.print_bin_op(left, right, "&&"),
            BinOp::Or => self.print_bin_op(left, right, "||"),
            BinOp::LtInt | BinOp::LtFloat => self.print_bin_op(left, right, "<"),
            BinOp::LtEqInt | BinOp::LtEqFloat => self.print_bin_op(left, right, "<="),
            BinOp::Eq => {
                use std::iter::once;
                *self.object_equality_used = true;
                Ok(docvec!("$deepEqual", wrap_args(once(left).chain(once(right)))))
            },
            BinOp::NotEq => {
                use std::iter::once;
                *self.object_equality_used = true;
                Ok(docvec!("!$deepEqual", wrap_args(once(left).chain(once(right)))))
            },
            BinOp::GtInt | BinOp::GtFloat => self.print_bin_op(left, right, ">"),
            BinOp::GtEqInt | BinOp::GtEqFloat => self.print_bin_op(left, right, ">="),
            BinOp::AddInt | BinOp::AddFloat => self.print_bin_op(left, right, "+"),
            BinOp::SubInt | BinOp::SubFloat => self.print_bin_op(left, right, "-"),
            BinOp::MultInt | BinOp::MultFloat => self.print_bin_op(left, right, "*"),
            BinOp::DivInt => Ok(self.print_bin_op(left, right, "/")?.append(" | 0")),
            BinOp::ModuloInt => self.print_bin_op(left, right, "%"),
            BinOp::DivFloat => {
                use std::iter::once;
                *self.float_division_used = true;
                Ok(docvec!("$divide", wrap_args(once(left).chain(once(right)))))
            }
        }
    }

    fn print_bin_op<'a>(
        &mut self,
        left: Document<'a>,
        right: Document<'a>,
        op: &'a str,
    ) -> Output<'a> {
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
pub fn constant_expression<'a>(expression: &'a TypedConstant) -> Output<'a> {
    match expression {
        Constant::Int { value, .. } => Ok(int(value)),
        Constant::Float { value, .. } => Ok(float(value)),
        Constant::String { value, .. } => Ok(string(&value.as_str())),
        Constant::Tuple { elements, .. } => array(elements.iter().map(|e| constant_expression(&e))),
        Constant::List { .. } => unsupported("List as constant"),
        Constant::Record { typ, name, .. } if typ.is_bool() && name == "True" => {
            Ok("true".to_doc())
        }
        Constant::Record { typ, name, .. } if typ.is_bool() && name == "False" => {
            Ok("false".to_doc())
        }
        Constant::Record { typ, .. } if typ.is_nil() => Ok("undefined".to_doc()),
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
