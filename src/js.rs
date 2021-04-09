#[cfg(test)]
mod tests;

use crate::{
    ast::*,
    error::GleamExpect,
    fs::{OutputFile, Utf8Writer},
    line_numbers::LineNumbers,
    pretty::*,
    project::{self, Analysed},
    type_::{
        ModuleValueConstructor, PatternConstructor, Type, TypeVar, ValueConstructor,
        ValueConstructorVariant,
    },
    Result,
};
use itertools::Itertools;
use std::sync::Arc;

const INDENT: isize = 4;

pub fn module(
    module: &TypedModule,
    line_numbers: &LineNumbers,
    writer: &mut impl Utf8Writer,
) -> Result<()> {
    let statements = concat(Itertools::intersperse(
        module
            .statements
            .iter()
            .flat_map(|s| statement(&module.name, s, &module.name, line_numbers)),
        lines(2),
    ));

    statements.pretty_print(80, writer)
}

fn statement<'a>(
    current_module: &'a [String],
    statement: &'a TypedStatement,
    module: &'a [String],
    line_numbers: &'a LineNumbers,
) -> Option<Document<'a>> {
    match statement {
        Statement::TypeAlias { .. } => None,
        Statement::CustomType { .. } => None,
        Statement::Import { .. } => None,
        Statement::ExternalType { .. } => None,
        Statement::ModuleConstant { .. } => None,

        Statement::Fn {
            arguments: args,
            name,
            body,
            return_type,
            ..
        } => Some(mod_fun(name, args, body, module, return_type, line_numbers)),
        _ => unimplemented!()

        // Statement::ExternalFn { public: false, .. } => None,
        // Statement::ExternalFn {
        //     fun,
        //     module,
        //     arguments: args,
        //     name,
        //     return_type,
        //     ..
        // } => Some(external_fun(
        //     current_module,
        //     name,
        //     module,
        //     fun,
        //     args,
        //     return_type,
        // )),
    }
}

fn mod_fun<'a>(
    name: &'a str,
    args: &'a [TypedArg],
    body: &'a TypedExpr,
    module: &'a [String],
    return_type: &'a Arc<Type>,
    line_numbers: &'a LineNumbers,
) -> Document<'a> {
    "function ".to_doc()
    .append(Document::String(name.to_string()))
    .append(fun_args(args))
    .append(" {")
    .append(line().append(expr(body)).nest(INDENT).group())
    .append(line())
    .append("}")
}

fn fun_args<'a>(args: &'a [TypedArg]) -> Document<'a> {
    wrap_args(args.iter().map(|a| match &a.names {
        ArgNames::Discard { .. } | ArgNames::LabelledDiscard { .. } => "_".to_doc(),
        ArgNames::Named { name } | ArgNames::NamedLabelled { name, .. } => {
            // TODO add these named variables to an env somewhere
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

fn expr<'a>(expression: &'a TypedExpr) -> Document<'a> {
    match expression {
        TypedExpr::Int { value, .. } => int(value),
        TypedExpr::Float { value, .. } => float(value),
        TypedExpr::String { value, .. } => string(value),
        TypedExpr::Seq { first, then, .. } => seq(first, then),
        TypedExpr::BinOp {
            name, left, right, ..
        } => bin_op(name, left, right),
        _ => {
            println!("expression: {:?}", expression);
            unimplemented!("expr")
        }
    }
}

fn int<'a>(value: &str) -> Document<'a> {
    Document::String(value.to_string())
}

fn float<'a>(value: &str) -> Document<'a> {
    Document::String(value.to_string())
}

fn string(value: &str) -> Document<'_> {
    value.to_doc().surround("\"", "\"")
}

fn seq<'a>(first: &'a TypedExpr, then: &'a TypedExpr) -> Document<'a> {
    force_break()
        .append(expr(first))
        .append(line())
        .append(expr(then))
}

fn bin_op<'a>(
    name: &'a BinOp,
    left: &'a TypedExpr,
    right: &'a TypedExpr,
) -> Document<'a> {
    // let div_zero = match name {
    //     BinOp::DivInt | BinOp::ModuloInt => Some("0"),
    //     BinOp::DivFloat => Some("0.0"),
    //     _ => None,
    // };
    match name {
        // BinOp::And => "andalso",
        // BinOp::Or => "orelse",
        BinOp::LtInt | BinOp::LtFloat => print_bin_op(left, right, "<"),
        BinOp::LtEqInt | BinOp::LtEqFloat => print_bin_op(left, right, "<="),
        // BinOp::Eq => "=:=",
        // BinOp::NotEq => "/=",
        BinOp::GtInt | BinOp::GtFloat => print_bin_op(left, right, ">"),
        BinOp::GtEqInt | BinOp::GtEqFloat => print_bin_op(left, right, ">="),
        BinOp::AddInt | BinOp::AddFloat => print_bin_op(left, right, "+"),
        BinOp::SubInt | BinOp::SubFloat => print_bin_op(left, right, "-"),
        BinOp::MultInt | BinOp::MultFloat => print_bin_op(left, right, "*"),
        BinOp::DivInt => Document::String("Math.floor".to_string())
            .append(print_bin_op(left, right, "/").surround("(", ")")),
        BinOp::DivFloat => print_bin_op(left, right, "/"),
        BinOp::ModuloInt => print_bin_op(left, right, "%"),
        _ => {
            println!("name: {:?}", name);
            unimplemented!("binop")
        }
    }
}

fn print_bin_op<'a>(
    left: &'a TypedExpr,
    right: &'a TypedExpr,
    op: &str
) -> Document<'a> {
        let left_expr = match left {
        TypedExpr::BinOp { .. } => expr(left).surround("(", ")"),
        _ => expr(left),
    };

    let right_expr = match right {
        TypedExpr::BinOp { .. } => expr(right).surround("(", ")"),
        _ => expr(right),
    };

    left_expr
        // TODO what is break_
        .append(" ")
        .append(Document::String(op.to_string()))
        .append(" ")
        .append(right_expr)
}