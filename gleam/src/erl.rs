#![allow(dead_code)]

#[cfg(test)]
use crate::ast::{Meta, Type};

use crate::ast::{Arg, BinOp, Expr, Module, Scope, Statement};
use crate::pretty::*;

use heck::{CamelCase, SnakeCase};
use itertools::Itertools;
use std::char;

const INDENT: isize = 4;

#[derive(Debug, Clone, Default)]
struct ExprEnv {}

pub fn module<T>(module: Module<T>) -> String {
    format!("-module({}).", module.name)
        .to_doc()
        .append(line())
        .append(
            module
                .statements
                .into_iter()
                .map(statement)
                .collect::<Vec<_>>(),
        )
        .format(80)
}

fn statement<T>(statement: Statement<T>) -> Document {
    match statement {
        Statement::Test { name, body, .. } => test(name, body),
        Statement::Enum { .. } => nil(),
        Statement::Import { .. } => nil(),
        Statement::ExternalType { .. } => nil(),
        Statement::Fun {
            args,
            public,
            name,
            body,
            ..
        } => mod_fun(public, name, args, body),
        Statement::ExternalFun {
            fun,
            module,
            args,
            public,
            name,
            ..
        } => external_fun(public, name, module, fun, args.len()),
    }
}

fn mod_fun<T>(public: bool, name: String, args: Vec<Arg>, body: Expr<T>) -> Document {
    let args_doc = args
        .iter()
        .map(|a| a.name.to_camel_case().to_doc())
        .intersperse(delim(","))
        .collect::<Vec<_>>()
        .to_doc()
        .nest_current()
        .surround("(", ")")
        .group();

    let body_doc = expr(body, &mut ExprEnv::default());

    export(public, &name, args.len())
        .append(line())
        .append(name)
        .append(args_doc)
        .append(" ->")
        .append(line().append(body_doc).nest(INDENT))
        .append(".")
        .append(line())
}

fn test<T>(name: String, body: Expr<T>) -> Document {
    let body_doc = expr(body, &mut ExprEnv::default());
    line()
        .append("-ifdef(TEST).")
        .append(line())
        .append(name)
        .append("_test() ->")
        .append(line().append(body_doc).nest(INDENT))
        .append(".")
        .append(line())
        .append("-endif.")
        .append(line())
}

fn export(public: bool, name: &String, arity: usize) -> Document {
    if public {
        format!("-export([{}/{}]).", name, arity)
            .to_doc()
            .append(line())
    } else {
        nil()
    }
}

// TODO: Escape
fn atom(value: String) -> Document {
    value.to_doc().surround("'", "'")
}

// TODO: Escape
fn string(value: String) -> Document {
    value.to_doc().surround("<<\"", "\">>")
}

// TODO: Wrap elem in `begin end` if it is a `Seq`
fn tuple<T>(elems: Vec<Expr<T>>, mut env: &ExprEnv) -> Document {
    elems
        .into_iter()
        .map(|e| expr(e, &mut env))
        .intersperse(delim(","))
        .collect::<Vec<_>>()
        .to_doc()
        .nest_current()
        .surround("{", "}")
        .group()
}

fn seq<T>(first: Expr<T>, then: Expr<T>, mut env: &ExprEnv) -> Document {
    expr(first, &mut env)
        .append(",")
        .append(line())
        .append(expr(then, &mut env))
}

// TODO: Surround left or right in parens if required
// TODO: Group nested bin_ops i.e. a |> b |> c
fn bin_op<T>(name: BinOp, left: Expr<T>, right: Expr<T>, mut env: &ExprEnv) -> Document {
    let op = match name {
        BinOp::Pipe => "|>", // TODO: This is wrong.
        BinOp::Lt => "<",
        BinOp::LtEq => "=<",
        BinOp::Eq => "=:=",
        BinOp::GtEq => ">=",
        BinOp::Gt => ">",
        BinOp::AddInt => "+",
        BinOp::AddFloat => "+",
        BinOp::SubInt => "-",
        BinOp::SubFloat => "-",
        BinOp::MultInt => "*",
        BinOp::MultFloat => "*",
        BinOp::DivInt => "div",
        BinOp::DivFloat => "/",
    };

    expr(left, &mut env)
        .append(break_("", " "))
        .append(op)
        .append(" ")
        .append(expr(right, &mut env))
}

fn var(name: String) -> Document {
    name.to_camel_case().to_doc()
}

fn expr<T>(expression: Expr<T>, mut env: &ExprEnv) -> Document {
    match expression {
        Expr::Int { value, .. } => value.to_doc(),
        Expr::Float { value, .. } => value.to_doc(),
        Expr::Atom { value, .. } => atom(value),
        Expr::String { value, .. } => string(value),
        Expr::Tuple { elems, .. } => tuple(elems, &mut env),
        Expr::Seq { first, then, .. } => seq(*first, *then, &mut env),
        Expr::Var { name, .. } => var(name),
        Expr::Fun { .. } => unimplemented!(),
        Expr::Nil { .. } => "[]".to_doc(),
        Expr::Cons { .. } => unimplemented!(),
        Expr::Call { .. } => unimplemented!(),
        Expr::BinOp {
            name, left, right, ..
        } => bin_op(name, *left, *right, &mut env),
        Expr::Let { .. } => unimplemented!(),
        Expr::Enum { name, args, .. } => {
            if args.len() == 0 {
                atom(name.to_snake_case())
            } else {
                atom(name.to_snake_case())
                    .append(delim(","))
                    .append(
                        args.into_iter()
                            .map(|e| expr(e, &mut env))
                            .intersperse(delim(","))
                            .collect::<Vec<_>>(),
                    )
                    .nest_current()
                    .surround("{", "}")
            }
        }
        Expr::Case { .. } => unimplemented!(),
        Expr::RecordNil { .. } => unimplemented!(),
        Expr::RecordCons { .. } => unimplemented!(),
        Expr::RecordSelect { .. } => unimplemented!(),
        Expr::ModuleSelect { .. } => unimplemented!(),
    }
}

fn external_fun(public: bool, name: String, module: String, fun: String, arity: usize) -> Document {
    let chars: String = (65..(65 + arity))
        .map(|x| x as u8 as char)
        .map(|c| c.to_string())
        .intersperse(", ".to_string())
        .collect();

    let header = format!("{}({}) ->", name, chars).to_doc();
    let body = format!("{}:{}({}).", module, fun, chars).to_doc();

    line()
        .to_doc()
        .append(export(public, &name, arity))
        .append(header)
        .append(line().append(body).nest(INDENT))
        .append(line())
}

#[test]
fn module_test() {
    let m: Module<()> = Module {
        name: "magic".to_string(),
        statements: vec![
            Statement::ExternalType {
                meta: Meta {},
                public: true,
                name: "Any".to_string(),
            },
            Statement::Enum {
                meta: Meta {},
                public: true,
                name: "Any".to_string(),
                args: vec![],
                constructors: vec![Type::Constructor {
                    meta: Meta {},
                    args: vec![],
                    name: "Ok".to_string(),
                }],
            },
            Statement::Import {
                meta: Meta {},
                module: "result".to_string(),
            },
            Statement::ExternalFun {
                meta: Meta {},
                args: vec![
                    Type::Constructor {
                        meta: Meta {},
                        args: vec![],
                        name: "Int".to_string(),
                    },
                    Type::Constructor {
                        meta: Meta {},
                        args: vec![],
                        name: "Int".to_string(),
                    },
                ],
                name: "add_ints".to_string(),
                fun: "add".to_string(),
                module: "int".to_string(),
                public: false,
                retrn: Type::Constructor {
                    meta: Meta {},
                    args: vec![],
                    name: "Int".to_string(),
                },
            },
            Statement::ExternalFun {
                meta: Meta {},
                args: vec![],
                name: "map".to_string(),
                fun: "new".to_string(),
                module: "maps".to_string(),
                public: true,
                retrn: Type::Constructor {
                    meta: Meta {},
                    args: vec![],
                    name: "Map".to_string(),
                },
            },
        ],
    };
    let expected = "-module(magic).

add_ints(A, B) ->
    int:add(A, B).

-export([map/0]).
map() ->
    maps:new().
"
    .to_string();
    assert_eq!(expected, module(m));
}

#[test]
fn expr_test() {
    let m: Module<()> = Module {
        name: "term".to_string(),
        statements: vec![
            Statement::Fun {
                meta: Meta {},
                public: false,
                args: vec![],
                name: "atom".to_string(),
                body: Expr::Atom {
                    meta: Meta {},
                    value: "ok".to_string(),
                },
            },
            Statement::Fun {
                meta: Meta {},
                public: false,
                args: vec![],
                name: "int".to_string(),
                body: Expr::Int {
                    meta: Meta {},
                    value: 176,
                },
            },
            Statement::Fun {
                meta: Meta {},
                public: false,
                args: vec![],
                name: "float".to_string(),
                body: Expr::Float {
                    meta: Meta {},
                    value: 11177.324401,
                },
            },
            Statement::Fun {
                meta: Meta {},
                public: false,
                args: vec![],
                name: "nil".to_string(),
                body: Expr::Nil {
                    meta: Meta {},
                    typ: (),
                },
            },
            Statement::Fun {
                meta: Meta {},
                public: false,
                args: vec![],
                name: "tup".to_string(),
                body: Expr::Tuple {
                    meta: Meta {},
                    typ: (),
                    elems: vec![
                        Expr::Int {
                            meta: Meta {},
                            value: 1,
                        },
                        Expr::Float {
                            meta: Meta {},
                            value: 2.0,
                        },
                    ],
                },
            },
            Statement::Fun {
                meta: Meta {},
                public: false,
                args: vec![],
                name: "string".to_string(),
                body: Expr::String {
                    meta: Meta {},
                    value: "Hello there!".to_string(),
                },
            },
            Statement::Fun {
                meta: Meta {},
                public: false,
                args: vec![],
                name: "seq".to_string(),
                body: Expr::Seq {
                    meta: Meta {},
                    first: Box::new(Expr::Int {
                        meta: Meta {},
                        value: 1,
                    }),
                    then: Box::new(Expr::Int {
                        meta: Meta {},
                        value: 2,
                    }),
                },
            },
            Statement::Fun {
                meta: Meta {},
                public: false,
                args: vec![],
                name: "bin_op".to_string(),
                body: Expr::BinOp {
                    meta: Meta {},
                    typ: (),
                    name: BinOp::AddInt,
                    left: Box::new(Expr::Int {
                        meta: Meta {},
                        value: 1,
                    }),
                    right: Box::new(Expr::Int {
                        meta: Meta {},
                        value: 2,
                    }),
                },
            },
            Statement::Fun {
                meta: Meta {},
                public: false,
                args: vec![],
                name: "enum1".to_string(),
                body: Expr::Enum {
                    meta: Meta {},
                    name: "Nil".to_string(),
                    typ: (),
                    args: vec![],
                },
            },
            Statement::Fun {
                meta: Meta {},
                public: false,
                args: vec![],
                name: "enum2".to_string(),
                body: Expr::Enum {
                    meta: Meta {},
                    name: "Ok".to_string(),
                    typ: (),
                    args: vec![
                        Expr::Int {
                            meta: Meta {},
                            value: 1,
                        },
                        Expr::Float {
                            meta: Meta {},
                            value: 2.0,
                        },
                    ],
                },
            },
            Statement::Fun {
                meta: Meta {},
                public: false,
                args: vec![Arg {
                    name: "some_arg".to_string(),
                }],
                name: "arg".to_string(),
                body: Expr::Var {
                    meta: Meta {},
                    name: "some_arg".to_string(),
                    typ: (),
                    scope: Scope::Local,
                },
            },
        ],
    };
    let expected = "-module(term).

atom() ->
    'ok'.

int() ->
    176.

float() ->
    11177.324401.

nil() ->
    [].

tup() ->
    {1, 2.0}.

string() ->
    <<\"Hello there!\">>.

seq() ->
    1,
    2.

bin_op() ->
    1 + 2.

enum1() ->
    'nil'.

enum2() ->
    {'ok', 1, 2.0}.

arg(SomeArg) ->
    SomeArg.
"
    .to_string();
    assert_eq!(expected, module(m));
}

#[test]
fn args_test() {
    let m: Module<()> = Module {
        name: "term".to_string(),
        statements: vec![Statement::Fun {
            meta: Meta {},
            public: false,
            name: "some_function".to_string(),
            args: vec![
                Arg {
                    name: "arg_one".to_string(),
                },
                Arg {
                    name: "arg_two".to_string(),
                },
                Arg {
                    name: "arg_3".to_string(),
                },
                Arg {
                    name: "arg4".to_string(),
                },
                Arg {
                    name: "arg_four".to_string(),
                },
                Arg {
                    name: "arg__five".to_string(),
                },
                Arg {
                    name: "arg_six".to_string(),
                },
                Arg {
                    name: "arg_that_is_long".to_string(),
                },
            ],
            body: Expr::Atom {
                meta: Meta {},
                value: "ok".to_string(),
            },
        }],
    };
    let expected = "-module(term).

some_function(ArgOne,
              ArgTwo,
              Arg3,
              Arg4,
              ArgFour,
              ArgFive,
              ArgSix,
              ArgThatIsLong) ->
    'ok'.
"
    .to_string();
    assert_eq!(expected, module(m));
}

#[test]
fn test_test() {
    let m: Module<()> = Module {
        name: "term".to_string(),
        statements: vec![Statement::Test {
            meta: Meta {},
            name: "bang".to_string(),
            body: Expr::Atom {
                meta: Meta {},
                value: "ok".to_string(),
            },
        }],
    };
    let expected = "-module(term).

-ifdef(TEST).
bang_test() ->
    'ok'.
-endif.
"
    .to_string();
    assert_eq!(expected, module(m));
}
