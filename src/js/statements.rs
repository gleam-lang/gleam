use super::*;
use crate::{
    ast::*, error::Error::UnsupportedFeature, line_numbers::LineNumbers, pretty::*, type_::Type,
    Result, Target,
};
use itertools::Itertools;
use std::sync::Arc;

fn unsupported<M: ToString, T>(label: M) -> Result<T> {
    Err(UnsupportedFeature {
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
        Statement::Import { .. } => Some(unsupported("Importing modules ")),
        Statement::ExternalType { .. } => None,
        Statement::ModuleConstant { .. } => Some(unsupported("Adding a module constant ")),
        Statement::Fn {
            arguments,
            name,
            body,
            return_type,
            public,
            ..
        } => Some(mod_fun(
            public,
            name,
            arguments,
            body,
            module,
            return_type,
            line_numbers,
        )),
        Statement::ExternalFn { .. } => Some(unsupported("Using an external function ")),
    }
}

fn mod_fun<'a>(
    public: &bool,
    name: &'a str,
    args: &'a [TypedArg],
    body: &'a TypedExpr,
    _module: &'a [String],
    _return_type: &'a Arc<Type>,
    _line_numbers: &'a LineNumbers,
) -> Result<Document<'a>> {
    let env = Env {
        return_last: &true,
        semicolon: &true,
    };
    let body = expr(body, &env)?;
    Ok(if *public {
        "export function "
    } else {
        "function "
    }
    .to_doc()
    .append(Document::String(name.to_string()))
    .append(fun_args(args))
    .append(" {")
    .append(line().append(body).nest(INDENT).group())
    .append(line())
    .append("}"))
}

fn fun_args<'a>(args: &'a [TypedArg]) -> Document<'a> {
    wrap_args(args.iter().map(|a| match &a.names {
        ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => "_".to_doc(),
        ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
            Document::String(name.to_string())
        }
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

fn expr<'a>(expression: &'a TypedExpr, env: &Env<'a>) -> Result<Document<'a>> {
    let rendered = match expression {
        TypedExpr::String { value, .. } => Ok(string(value)),

        TypedExpr::Int { .. } => unsupported("Integer values "),
        TypedExpr::Float { .. } => unsupported("Float values"),

        TypedExpr::List { .. } => unsupported("List "),

        TypedExpr::Tuple { .. } => unsupported("Tuple "),
        TypedExpr::TupleIndex { .. } => unsupported("Tuple "),

        TypedExpr::Case { .. } => unsupported("Case "),

        TypedExpr::Call { .. } => unsupported("Function "),
        TypedExpr::Fn { .. } => unsupported("Function "),

        TypedExpr::RecordAccess { .. } => unsupported("Custom Record "),
        TypedExpr::RecordUpdate { .. } => unsupported("Function "),

        TypedExpr::Var { .. } => unsupported("Bindings "),
        TypedExpr::Seq { .. } => unsupported("Bindings "),
        TypedExpr::Assignment { .. } => unsupported("Bindings "),

        TypedExpr::BinOp { .. } => unsupported("Binary operation "),

        TypedExpr::Todo { .. } => unsupported("todo keyword "),

        TypedExpr::BitString { .. } => unsupported("Bitstring "),

        TypedExpr::Pipe { .. } => unsupported("Pipe "),

        TypedExpr::ModuleSelect { .. } => unsupported("Module function call "),
    }?;
    Ok(match expression {
        TypedExpr::Seq { .. } | TypedExpr::Assignment { .. } => rendered,
        _ => match env.return_last {
            true => "return ",
            _ => "",
        }
        .to_doc()
        .append(rendered)
        .append(match env.semicolon {
            true => ";",
            false => "",
        }),
    })
}

fn string(value: &str) -> Document<'_> {
    value.to_doc().surround("\"", "\"")
}
