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

#[derive(Debug, Clone)]
struct Env<'a> {
    return_last: &'a bool
    // module: &'a [String],
    // function: &'a str,
    // line_numbers: &'a LineNumbers,
    // current_scope_vars: im::HashMap<String, usize>,
    // erl_function_scope_vars: im::HashMap<String, usize>,
}

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
        Statement::ModuleConstant { public, name, value, .. } => {
            // I don't know what I'm doing here. 
            let value = match &**value {
                Constant::Int {value, ..} => int(value),
                _ => unimplemented!("consts")
            };
            
            let rendered = if *public {
                "export "
            } else {
                ""
            }.to_doc()
            .append("const ")
            .append(Document::String(name.to_string()))
            .append(" = ")
            .append(value);
            Some(rendered)
        },
        

        Statement::Fn {
            arguments: args,
            name,
            body,
            return_type,
            public,
            ..
        } => Some(mod_fun(public, name, args, body, module, return_type, line_numbers)),

        // Statement::ExternalFn { public: false, .. } => None,
        Statement::ExternalFn {
            public,
            fun,
            module,
            arguments: args,
            name,
            return_type,
            ..
        } => {
            let arg_string = wrap_args(args.iter().enumerate().map(|a| match a {
                (index, ExternalFnArg{label, ..}) => {
                    let arg_name = label.clone().unwrap_or(format!("arg{}", index));
                    println!("a: {:?}", a);
                    // "f".to_doc()
                    Document::String(arg_name)
                }
            }));
            let rendered = if &true == public {
                "export "
            } else {
                ""
            }.to_doc()
            .append("function ")
            .append(Document::String(name.to_string()))
            // take label or index
            .append(arg_string.clone())
            .append(" {")
            .append(line()
                .append(Document::String(module.clone()))
                .append(".")
                .append(Document::String(fun.clone()))
                .append(arg_string)
                .nest(INDENT).group())
            .append(line())
            .append("}");
            Some(rendered)
        },
        _ => unimplemented!("statements needed")

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
    println!("body: {:?}", body);

    let env = Env{return_last: &true};
    if &true == public {
        "export "
    } else {
        ""
    }.to_doc()
    .append("function ")
    .append(Document::String(name.to_string()))
    .append(fun_args(args))
    .append(" {")
    .append(line().append(expr(body, &env)).nest(INDENT).group())
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

fn expr<'a>(expression: &'a TypedExpr, env: &Env<'a>) -> Document<'a> {
    // println!("expr: {:?}", expression);
    // println!("env: {:?}", env);

    let rendered = match expression {
        TypedExpr::Int { value, .. } => int(value),
        TypedExpr::Float { value, .. } => float(value),
        TypedExpr::String { value, .. } => string(value),

        TypedExpr::ListNil { .. } => "[]".to_doc(),
        TypedExpr::ListCons { head, tail, .. } => expr_list_cons(head, tail),

        // What's the difference between iter and into_iter
        TypedExpr::Tuple { elems, .. } => tuple(elems.iter().map(|e|
            maybe_block_expr(e, env)
        )),
        TypedExpr::TupleIndex { tuple, index, .. } => tuple_index(tuple, *index),

        TypedExpr::Call { fun, args, .. } => call(fun, args),

        // TODO is this always an anonymous fn
        TypedExpr::Fn { args, body, .. } => fun(args, body),
        
        TypedExpr::Seq { first, then, .. } => seq(first, then, env),
        TypedExpr::Var {
            name, constructor, ..
        } => var(name, constructor),
        TypedExpr::Let {
            value,
            pattern,
            then,
            kind: BindingKind::Let,
            ..
        } => let_(value, pattern, then, env),
        TypedExpr::BinOp {
            name, left, right, ..
        } => bin_op(name, left, right, &Env{return_last: &false}),
        TypedExpr::Todo {
            label, location, ..
        } => {
            println!("expression: {:?}", expression);

            todo(label, *location)
        },
        _ => {
            println!("expression: {:?}", expression);
            unimplemented!("expr")
        }
    };
    match expression {
        // I would have thought let would be inside a sequence?
        TypedExpr::Seq { .. } | TypedExpr::Let { .. } => "",
        _ => match env.return_last {
            true => "return ",
            _ => ""
        }
    }.to_doc().append(rendered)
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
    let env = Env{ return_last: &false};
    let mut elements = vec![head];

    let final_tail = collect_cons(tail, &mut elements);
    let elements = elements.into_iter().map(|e| 
        maybe_block_expr(e, &env)
    );
    let content = concat(Itertools::intersperse(elements, break_(",", ", ")));
    let content = if let Some(final_tail) = final_tail {
        content.append(Document::String(", ...".to_string()).append(maybe_block_expr(final_tail, &env)))
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
    let env = Env{ return_last: &false};
    expr(tuple, &env)
        .append(index.to_doc().surround("[", "]"))
}

fn call<'a>(fun: &'a TypedExpr, args: &'a [CallArg<TypedExpr>]) -> Document<'a> {
    let args = args.into_iter().map(|arg| 
        maybe_block_expr(&arg.value, &Env{ return_last: &false})
    );
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
            .append(args.surround("(", ")"))

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
            .append(args.surround("(", ")"))
        }
        TypedExpr::Var {
            constructor:
                ValueConstructor {
                    variant: ValueConstructorVariant::Record { name, .. },
                    ..
                },
            ..
        } => {
            println!("args: {:?}", args);
            "{gleam_record: ".to_doc()
            .append(Document::String(name.clone()))
            .append(args)
            .append("}".to_doc())
        },

        _ => {
            println!("fun: {:?}", fun);

            unimplemented!("todo in the call handling")}
    }
    
}

fn fun<'a>(args: &'a [TypedArg], body: &'a TypedExpr) -> Document<'a> {
    let env = Env{ return_last: &true};
    let doc = "function"
        .to_doc()
        .append(fun_args(args))
        .append(" {")
        .append(line().append(expr(body, &env)).nest(INDENT).group())
        .append(line())
        .append("}");
    doc
}


fn seq<'a>(
    first: &'a TypedExpr, 
    then: &'a TypedExpr, 
    env: &Env<'a>
) -> Document<'a> {
    force_break()
        .append(expr(first, &Env{ return_last: &false}))
        .append(line())
        .append(expr(then, env))
}

fn var<'a>(name: &'a str, constructor: &'a ValueConstructor) -> Document<'a> {
    match &constructor.variant {
        ValueConstructorVariant::Record {
            name: record_name, ..
        } => match &*constructor.type_ {
            // Type::Fn { args, .. } => {
            //     let chars = incrementing_args_list(args.len());
            //     "fun("
            //         .to_doc()
            //         .append(Document::String(chars.clone()))
            //         .append(") -> {")
            //         .append(Document::String(record_name.to_snake_case()))
            //         .append(", ")
            //         .append(Document::String(chars))
            //         .append("} end")
            // }
            _ => {
                // atom(record_name.to_snake_case())
                // TODO handle non boolean literals
                // Document::String(record_name.clone())
                match record_name.as_ref() {
                    "True" => "true".to_doc(),
                    "False" => "false".to_doc(),
                    _ => unimplemented!("single Record variant")
                }
            },
        },

        ValueConstructorVariant::LocalVariable => name.to_doc(),
        ValueConstructorVariant::ModuleConstant { .. } =>  {           
        name.to_doc()},

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
    env: &Env<'a>
) -> Document<'a> {
    let body = maybe_block_expr(value, &Env{return_last: &false});
    pattern(pat)
        .append(" = ")
        .append(body)
        .append(line())
        .append(expr(then, env))
}

fn pattern<'a>(p: &'a TypedPattern) -> Document<'a> {
    Document::String("let ".to_string()).append(match p {
        // Pattern::Nil { .. } => "[]".to_doc(),
        Pattern::Var { name, .. } => name.to_doc(),
        // In erl/pattern.rs
        // Uses collect_cons, but for a pattern type
        _ => {
            println!("p: {:?}", p);
            unimplemented!("pattern")
        }
    })
}

fn maybe_block_expr<'a>(
    expression: &'a TypedExpr, 
    env: &Env<'a>
) -> Document<'a> {
    match &expression {
        // TODO this handled a let in the erlang version
        //  | TypedExpr::Let { value: first, then, .. }
        TypedExpr::Seq { first, then, .. } => force_break()
            .append("(")
            .append(line().append(sequence_expr(first, then, env)).nest(INDENT).group())
            .append(line())
            .append(")"),
        _ => expr(expression, env),
    }
}

fn sequence_expr<'a>(
    first: &'a TypedExpr, 
    then: &'a TypedExpr, 
    env: &Env<'a>
) -> Document<'a> {
    // TODO merge value into env
    expr(first, &Env{return_last: &false})
        .append(",")
        .append(line())
        .append(match then {
            TypedExpr::Seq { first, then, .. } => sequence_expr(first, then, env),
            _ => expr(then, env)
        })
}

fn bin_op<'a>(
    name: &'a BinOp,
    left: &'a TypedExpr,
    right: &'a TypedExpr,
    env: &Env<'a>
) -> Document<'a> {
    // let div_zero = match name {
    //     BinOp::DivInt | BinOp::ModuloInt => Some("0"),
    //     BinOp::DivFloat => Some("0.0"),
    //     _ => None,
    // };
    match name {
        BinOp::And => print_bin_op(left, right, "&&", env),
        BinOp::Or => print_bin_op(left, right, "||", env),
        BinOp::LtInt | BinOp::LtFloat => print_bin_op(left, right, "<", env),
        BinOp::LtEqInt | BinOp::LtEqFloat => print_bin_op(left, right, "<=", env),
        // https://dmitripavlutin.com/how-to-compare-objects-in-javascript/
        // BinOp::Eq => "=:=",
        // BinOp::NotEq => "/=",
        BinOp::GtInt | BinOp::GtFloat => print_bin_op(left, right, ">", env),
        BinOp::GtEqInt | BinOp::GtEqFloat => print_bin_op(left, right, ">=", env),
        BinOp::AddInt | BinOp::AddFloat => print_bin_op(left, right, "+", env),
        BinOp::SubInt | BinOp::SubFloat => print_bin_op(left, right, "-", env),
        BinOp::MultInt | BinOp::MultFloat => print_bin_op(left, right, "*", env),
        BinOp::DivInt => Document::String("Math.floor".to_string())
            .append(print_bin_op(left, right, "/", env).surround("(", ")")),
        BinOp::DivFloat => print_bin_op(left, right, "/", env),
        BinOp::ModuloInt => print_bin_op(left, right, "%", env),
        _ => {
            println!("name: {:?}", name);
            unimplemented!("binop")
        }
    }
}

fn print_bin_op<'a>(
    left: &'a TypedExpr,
    right: &'a TypedExpr,
    op: &str,
    env: &Env<'a>
) -> Document<'a> {
        let left_expr = match left {
        TypedExpr::BinOp { .. } => expr(left, env).surround("(", ")"),
        _ => expr(left, env),
    };

    let right_expr = match right {
        TypedExpr::BinOp { .. } => expr(right, env).surround("(", ")"),
        _ => expr(right, env),
    };

    left_expr
        // TODO what is break_
        .append(" ")
        .append(Document::String(op.to_string()))
        .append(" ")
        .append(right_expr)
}

fn todo<'a>(message: &'a Option<String>, location: SrcSpan) -> Document<'a> {
    let message = match message {
        Some(message) => message,
        None => "This has not yet been implemented",
    };

    println!("location: {:?}", location);
    "throw Object.assign(new Error("
        .to_doc()
        .append(Document::String(message.to_string()).surround("\"", "\""))
        .append("), {line: ")
        .append(Document::String(format!("{}", location.start)))
        .append("})")
}
