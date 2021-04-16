mod expression;
#[cfg(test)]
mod tests;

use crate::{ast::*, fs::Utf8Writer, line_numbers::LineNumbers, pretty::*, type_::Type};
use itertools::Itertools;
use std::sync::Arc;

const INDENT: isize = 2;

pub type Output<'a> = Result<Document<'a>, Error>;

pub fn module(
    module: &TypedModule,
    line_numbers: &LineNumbers,
    writer: &mut impl Utf8Writer,
) -> Result<(), crate::Error> {
    let statements = module
        .statements
        .iter()
        .flat_map(|s| statement(&module.name, s, &module.name, line_numbers));
    let statements = Itertools::intersperse(statements, Ok(lines(2)))
        .collect::<Result<Vec<_>, _>>()
        .map_err(crate::Error::JavaScript)?;
    statements.to_doc().pretty_print(80, writer)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    Unsupported { feature: String },
}

fn unsupported<M: ToString, T>(label: M) -> Result<T, Error> {
    Err(Error::Unsupported {
        feature: label.to_string(),
    })
}

pub fn statement<'a>(
    _current_module: &'a [String],
    statement: &'a TypedStatement,
    module: &'a [String],
    line_numbers: &'a LineNumbers,
) -> Option<Output<'a>> {
    match statement {
        Statement::TypeAlias { .. } => None,
        Statement::CustomType { .. } => None,
        Statement::Import { .. } => Some(unsupported("Importing modules")),
        Statement::ExternalType { .. } => None,
        Statement::ModuleConstant {
            public,
            name,
            value,
            ..
        } => Some(module_constant(*public, name, value)),
        Statement::Fn {
            arguments,
            name,
            body,
            return_type,
            public,
            ..
        } => Some(module_function(
            *public,
            name,
            arguments,
            body,
            module,
            return_type,
            line_numbers,
        )),
        Statement::ExternalFn { .. } => Some(unsupported("Using an external function")),
    }
}

fn module_constant<'a, T, Y>(public: bool, name: &'a str, value: &'a Constant<T, Y>) -> Output<'a> {
    let head = if public { "export const " } else { "const " };
    Ok(docvec![
        head,
        name,
        " = ",
        expression::constant_expression(value)?,
        ";",
    ])
}

fn module_function<'a>(
    public: bool,
    name: &'a str,
    args: &'a [TypedArg],
    body: &'a TypedExpr,
    _module: &'a [String],
    _return_type: &'a Arc<Type>,
    _line_numbers: &'a LineNumbers,
) -> Output<'a> {
    let head = if public {
        "export function "
    } else {
        "function "
    };
    Ok(docvec![
        head,
        name,
        fun_args(args),
        " {",
        docvec![line(), expression::Generator::compile(body)?]
            .nest(INDENT)
            .group(),
        line(),
        "}",
    ])
}

fn fun_args<'a>(args: &'a [TypedArg]) -> Document<'a> {
    wrap_args(args.iter().map(|a| match &a.names {
        ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => "_".to_doc(),
        ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => name.to_doc(),
    }))
}

fn wrap_args<'a, I>(args: I) -> Document<'a>
where
    I: Iterator<Item = Document<'a>>,
{
    break_("", "")
        .append(concat(Itertools::intersperse(args, break_(",", ", "))))
        .nest(INDENT)
        .append(break_("", ""))
        .surround("(", ")")
        .group()
}
