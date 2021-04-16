mod expression;
#[cfg(test)]
mod tests;

use crate::{
    ast::*, fs::Utf8Writer, line_numbers::LineNumbers, pretty::*, type_::Type, Error, Result,
    Target,
};
use itertools::Itertools;
use std::sync::Arc;

const INDENT: isize = 2;

pub fn module(
    module: &TypedModule,
    line_numbers: &LineNumbers,
    writer: &mut impl Utf8Writer,
) -> Result<()> {
    let rendered = module
        .statements
        .iter()
        .flat_map(|s| statement(&module.name, s, &module.name, line_numbers));
    let rendered: Result<Vec<Document<'_>>> = rendered.collect();
    let rendered = rendered?;
    let statements = concat(Itertools::intersperse(rendered.into_iter(), lines(2)));

    statements.pretty_print(80, writer)
}

fn unsupported<M: ToString, T>(label: M) -> Result<T> {
    Err(Error::UnsupportedFeature {
        target: Target::JavaScript,
        feature: label.to_string(),
    })
}

pub fn statement<'a>(
    _current_module: &'a [String],
    statement: &'a TypedStatement,
    module: &'a [String],
    line_numbers: &'a LineNumbers,
) -> Option<Result<Document<'a>>> {
    match statement {
        Statement::TypeAlias { .. } => None,
        Statement::CustomType { .. } => None,
        Statement::Import { .. } => Some(unsupported("Importing modules")),
        Statement::ExternalType { .. } => None,
        Statement::ModuleConstant { .. } => Some(unsupported("Adding a module constant")),
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

fn module_function<'a>(
    public: bool,
    name: &'a str,
    args: &'a [TypedArg],
    body: &'a TypedExpr,
    _module: &'a [String],
    _return_type: &'a Arc<Type>,
    _line_numbers: &'a LineNumbers,
) -> Result<Document<'a>> {
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
