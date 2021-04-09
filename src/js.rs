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
        Statement::Import { module, as_name, .. } => {
            println!("import: {:?}", statement);
            

            let as_name = as_name.clone().unwrap_or(module.join("_"));

            let line = "import * as "
                .to_doc()
                .append(Document::String(as_name))
                .append(" from ".to_doc())
                .append(Document::String(module.join("/")));
            Some(line)
        },
        Statement::ExternalType { .. } => None,
        Statement::ModuleConstant { .. } => None,

        Statement::Fn {
            arguments: args,
            name,
            body,
            return_type,
            public,
            ..
        } => Some(mod_fun(public, name, args, body, module, return_type, line_numbers)),
        _ => unimplemented!()

        // TODO What's the difference between Fn and external Fn when public is a key
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
    public: &bool,
    name: &'a str,
    args: &'a [TypedArg],
    body: &'a TypedExpr,
    module: &'a [String],
    return_type: &'a Arc<Type>,
    line_numbers: &'a LineNumbers,
) -> Document<'a> {

    if &true == public {
        "export "
    } else {
        ""
    }.to_doc()
    .append("function ")
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

        TypedExpr::ListNil { .. } => "[]".to_doc(),
        TypedExpr::ListCons { head, tail, .. } => expr_list_cons(head, tail),

        // What's the difference between iter and into_iter
        TypedExpr::Tuple { elems, .. } => tuple(elems.iter().map(maybe_block_expr)),
        TypedExpr::TupleIndex { tuple, index, .. } => tuple_index(tuple, *index),

        TypedExpr::Call { fun, args, .. } => call(fun, args),

        // TODO is this always an anonymous fn
        TypedExpr::Fn { args, body, .. } => fun(args, body),
        
        TypedExpr::Seq { first, then, .. } => seq(first, then),
        TypedExpr::Var {
            name, constructor, ..
        } => var(name, constructor),
        TypedExpr::Let {
            value,
            pattern,
            then,
            kind: BindingKind::Let,
            ..
        } => let_(value, pattern, then),
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

fn expr_list_cons<'a>(head: &'a TypedExpr, tail: &'a TypedExpr) -> Document<'a> {
    let mut elements = vec![head];

    let final_tail = collect_cons(tail, &mut elements);
    let elements = elements.into_iter().map(maybe_block_expr);
    let content = concat(Itertools::intersperse(elements, break_(",", ", ")));
    let content = if let Some(final_tail) = final_tail {
        content.append(Document::String(", ...".to_string()).append(maybe_block_expr(final_tail)))
    } else {
        content
    };
    content.surround("[", "]")
}

fn collect_cons<'a>(tail: &'a TypedExpr, elements: &mut Vec<&'a TypedExpr>) -> Option<&'a TypedExpr> {
    match tail {
        TypedExpr::ListNil { .. } => None,
        TypedExpr::ListCons { head, tail, .. } => {
            elements.push(head);
            collect_cons(tail, elements)
        }
        // TODO is it possible to write improper lists in Gleam
        other => Some(other)
    }
}

fn tuple<'a>(elems: impl Iterator<Item = Document<'a>>) -> Document<'a> {
    concat(Itertools::intersperse(elems, break_(",", ", ")))
        .nest_current()
        .surround("[", "]")
        .group()
}

fn tuple_index<'a>(tuple: &'a TypedExpr, index: u64) -> Document<'a> {
    expr(tuple).
    append(index.to_doc().surround("[", "]"))
}

fn call<'a>(fun: &'a TypedExpr, args: &'a [CallArg<TypedExpr>]) -> Document<'a> {
    let args = args.into_iter().map(|arg| maybe_block_expr(&arg.value));
    let args = concat(Itertools::intersperse(args, break_(",", ", ")));
    match fun {
        TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::ModuleFn { module, name, .. },
                    ..
                },
            ..
        } => {
            // TODO self module or not
            Document::String(name.to_string())
        }
        TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::LocalVariable,
                    ..
                },
            name,
            ..
        } => {
            // TODO self module or not
            Document::String(name.to_string())
        }

        _ => {
            println!("fun: {:?}", fun);

            unimplemented!("todo in the call handling")}
    }
    .append(args.surround("(", ")"))
    // wrap_args(
    //     args.iter()
    //         .map(|arg| maybe_block_expr(&arg.value))
    //         .collect(),
    // )
    
}

fn fun<'a>(args: &'a [TypedArg], body: &'a TypedExpr) -> Document<'a> {
    let doc = "function"
        .to_doc()
        .append(fun_args(args))
        .append(" {")
        .append(line().append(expr(body)).nest(INDENT).group())
        .append(line())
        .append("}");
    doc
}


fn seq<'a>(first: &'a TypedExpr, then: &'a TypedExpr) -> Document<'a> {
    force_break()
        .append(expr(first))
        .append(line())
        .append(expr(then))
}

fn var<'a>(name: &'a str, constructor: &'a ValueConstructor) -> Document<'a> {
    match &constructor.variant {
        // ValueConstructorVariant::Record {
        //     name: record_name, ..
        // } => match &*constructor.type_ {
        //     Type::Fn { args, .. } => {
        //         let chars = incrementing_args_list(args.len());
        //         "fun("
        //             .to_doc()
        //             .append(Document::String(chars.clone()))
        //             .append(") -> {")
        //             .append(Document::String(record_name.to_snake_case()))
        //             .append(", ")
        //             .append(Document::String(chars))
        //             .append("} end")
        //     }
        //     _ => atom(record_name.to_snake_case()),
        // },

        ValueConstructorVariant::LocalVariable => name.to_doc(),
        // ValueConstructorVariant::ModuleConstant { literal } => const_inline(literal, env),

        ValueConstructorVariant::ModuleFn {
            arity, ref module, ..
        }  => Document::String(name.to_string()),
        // TODO if module == env.module

        // ValueConstructorVariant::ModuleFn {
        //     arity,
        //     module,
        //     name,
        //     ..
        // } => "fun "
        //     .to_doc()
        //     .append(module_name_join(module))
        //     .append(":")
        //     .append(atom(name.to_string()))
        //     .append("/")
        //     .append(*arity),
        _ => {
            println!("name: {:?}", name);
            println!("constructor: {:?}", constructor);

            unimplemented!("var")
        }

    }
}

fn let_<'a>(
    value: &'a TypedExpr,
    pat: &'a TypedPattern,
    then: &'a TypedExpr,
) -> Document<'a> {
    let body = maybe_block_expr(value);
    pattern(pat)
        .append(" = ")
        .append(body)
        .append(line())
        .append(expr(then))
}

fn pattern<'a>(p: &'a TypedPattern) -> Document<'a> {
    Document::String("let ".to_string()).append(match p {
        // Pattern::Nil { .. } => "[]".to_doc(),
        Pattern::Var { name, .. } => name.to_doc(),
        _ => {
            println!("p: {:?}", p);
            unimplemented!("pattern")
        }
    })
}

fn maybe_block_expr<'a>(expression: &'a TypedExpr) -> Document<'a> {
    match &expression {
        TypedExpr::Seq { .. } | TypedExpr::Let { .. } => force_break()
            .append("{")
            .append(line().append(expr(expression)).nest(INDENT).group())
            .append(line())
            .append("}"),
        _ => expr(expression),
    }
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