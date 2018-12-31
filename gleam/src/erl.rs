use crate::ast::{
    Arg, BinOp, Clause, Expr, Module, Pattern, Scope, Statement, Type, TypedClause, TypedExpr,
    TypedModule, TypedScope, TypedStatement,
};
use crate::pretty::*;
use crate::typ;

use heck::{CamelCase, SnakeCase};
use itertools::Itertools;
use std::char;
use std::default::Default;

const INDENT: isize = 4;

fn default<T>() -> T
where
    T: Default,
{
    Default::default()
}

#[derive(Debug, Clone, Default)]
struct Env {}

pub fn module(module: TypedModule) -> String {
    format!("-module({}).", module.name)
        .to_doc()
        .append(lines(2))
        .append(
            module
                .statements
                .into_iter()
                .flat_map(statement)
                .intersperse(lines(2))
                .collect::<Vec<_>>(),
        )
        .append(line())
        .format(80)
}

fn statement(statement: TypedStatement) -> Option<Document> {
    match statement {
        Statement::Test { name, body, .. } => Some(test(name, body)),
        Statement::Enum { .. } => None,
        Statement::Import { .. } => None,
        Statement::ExternalType { .. } => None,
        Statement::Fun {
            args,
            public,
            name,
            body,
            ..
        } => Some(mod_fun(public, name, args, body)),
        Statement::ExternalFun {
            fun,
            module,
            args,
            public,
            name,
            ..
        } => Some(external_fun(public, name, module, fun, args.len())),
    }
}

fn mod_fun(public: bool, name: String, args: Vec<Arg>, body: TypedExpr) -> Document {
    let body_doc = expr(body, &mut Env::default());

    export(public, &name, args.len())
        .append(name)
        .append(fun_args(args))
        .append(" ->")
        .append(line().append(body_doc).nest(INDENT).group())
        .append(".")
}

fn fun_args(args: Vec<Arg>) -> Document {
    wrap_args(args.into_iter().map(|a| a.name.to_camel_case().to_doc()))
}

fn call_args(args: Vec<TypedExpr>, env: &mut Env) -> Document {
    wrap_args(args.into_iter().map(|e| expr(e, env)))
}

fn wrap_args<I>(args: I) -> Document
where
    I: Iterator<Item = Document>,
{
    args.intersperse(delim(","))
        .collect::<Vec<_>>()
        .to_doc()
        .nest_current()
        .surround("(", ")")
        .group()
}

fn test(name: String, body: TypedExpr) -> Document {
    let body_doc = expr(body, &mut Env::default());
    "-ifdef(TEST)."
        .to_doc()
        .append(line())
        .append(name)
        .append("_test() ->")
        .append(line().append(body_doc).nest(INDENT))
        .append(".")
        .append(line())
        .append("-endif.")
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

fn tuple<F, E>(f: F, elems: Vec<E>, env: &mut Env) -> Document
where
    F: Fn(E, &mut Env) -> Document,
{
    elems
        .into_iter()
        .map(|e| f(e, env))
        .intersperse(delim(","))
        .collect::<Vec<_>>()
        .to_doc()
        .nest_current()
        .surround("{", "}")
        .group()
}

fn seq(first: TypedExpr, then: TypedExpr, env: &mut Env) -> Document {
    expr(first, env)
        .append(",")
        .append(line())
        .append(expr(then, env))
}

// TODO: Surround left or right in parens if required
// TODO: Group nested bin_ops i.e. a |> b |> c
fn bin_op(name: BinOp, left: TypedExpr, right: TypedExpr, env: &mut Env) -> Document {
    let op = match name {
        BinOp::Pipe => "|>", // TODO: This is wrong.
        BinOp::Lt => "<",
        BinOp::LtEq => "=<",
        BinOp::Eq => "=:=",
        BinOp::NotEq => "/=",
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

    expr(left, env)
        .append(break_("", " "))
        .append(op)
        .append(" ")
        .append(expr(right, env))
}

fn let_(value: TypedExpr, pat: Pattern, then: TypedExpr, env: &mut Env) -> Document {
    pattern(pat, env)
        .append(" =")
        .append(break_("", " "))
        .append(expr(value, env).nest(INDENT))
        .append(",")
        .append(line())
        .append(expr(then, env))
}

fn pattern(p: Pattern, env: &mut Env) -> Document {
    match p {
        Pattern::Nil { .. } => "[]".to_doc(),
        Pattern::List { .. } => unimplemented!(),
        Pattern::Record { .. } => unimplemented!(),
        Pattern::Var { name, .. } => var(name, Scope::Local, env),
        Pattern::Int { value, .. } => value.to_doc(),
        Pattern::Atom { value, .. } => atom(value),
        Pattern::Float { value, .. } => value.to_doc(),
        Pattern::Tuple { elems, .. } => tuple(pattern, elems, env),
        Pattern::String { value, .. } => string(value),
        Pattern::Enum { name, args, .. } => enum_(pattern_atom, pattern, name, args, env),
    }
}

fn cons(head: TypedExpr, tail: TypedExpr, env: &mut Env) -> Document {
    // TODO: Flatten nested cons into a list i.e. [1, 2, 3 | X] or [1, 2, 3, 4]
    // TODO: Break, indent, etc
    expr(head, env)
        .append(" | ")
        .append(expr(tail, env))
        .surround("[", "]")
}

fn expr_record_cons(label: String, value: TypedExpr, tail: TypedExpr, env: &mut Env) -> Document {
    record_cons(expr, "=>".to_string(), label, value, tail, env)
}

fn record_cons<F, E>(f: F, sep: String, label: String, value: E, tail: E, env: &mut Env) -> Document
where
    F: Fn(E, &mut Env) -> Document,
{
    // TODO: Flatten nested cons into a map i.e. X#{a=>1, b=>2}
    // TODO: Break, indent, etc
    f(tail, env)
        .append("#{")
        .append(atom(label))
        .append(" ")
        .append(sep)
        .append(" ")
        .append(f(value, env))
        .append("}")
}

fn var(name: String, scope: TypedScope, env: &mut Env) -> Document {
    match scope {
        Scope::Local => name.to_camel_case().to_doc(),
        Scope::Constant { value } => expr(*value, env),
        Scope::Module { arity, .. } => "fun ".to_doc().append(name).append("/").append(arity),
    }
}

fn enum_<H, F, E>(h: H, to_doc: F, name: String, mut args: Vec<E>, env: &mut Env) -> Document
where
    H: Fn(String) -> E,
    F: Fn(E, &mut Env) -> Document,
{
    if args.len() == 0 {
        to_doc(h(name.to_snake_case()), env)
    } else {
        args.insert(0, h(name.to_snake_case()));
        tuple(to_doc, args, env)
    }
}

fn clause(clause: TypedClause, env: &mut Env) -> Document {
    pattern(clause.pattern, env)
        .append(" ->")
        .append(line().append(expr(*clause.then, env)).nest(INDENT).group())
}

fn clauses(cs: Vec<TypedClause>, env: &mut Env) -> Document {
    cs.into_iter()
        .map(|c| clause(c, env))
        .intersperse(";".to_doc().append(lines(2)))
        .collect::<Vec<_>>()
        .to_doc()
}

fn case(subject: TypedExpr, cs: Vec<TypedClause>, env: &mut Env) -> Document {
    "case "
        .to_doc()
        .append(expr(subject, env).group())
        .append(" of")
        .append(line().append(clauses(cs, env)).nest(INDENT))
        .append(line())
        .append("end")
        .group()
}

fn pattern_atom(s: String) -> Pattern {
    Pattern::Atom {
        meta: default(),
        value: s,
    }
}

fn call(fun: TypedExpr, args: Vec<TypedExpr>, env: &mut Env) -> Document {
    let fun = match fun {
        Expr::Var {
            scope: Scope::Module { .. },
            name,
            ..
        } => name.to_doc(),

        call @ Expr::Call { .. } => expr(call, env).surround("(", ")"),

        other => expr(other, env),
    };
    fun.append(call_args(args, env))
}

fn expr(expression: TypedExpr, env: &mut Env) -> Document {
    match expression {
        Expr::Nil { .. } => "[]".to_doc(),
        Expr::RecordNil { .. } => "#{}".to_doc(),
        Expr::Int { value, .. } => value.to_doc(),
        Expr::Float { value, .. } => value.to_doc(),
        Expr::Constructor { name, .. } => atom(name.to_snake_case()),
        Expr::Atom { value, .. } => atom(value),
        Expr::String { value, .. } => string(value),
        Expr::Tuple { elems, .. } => tuple(expr, elems, env),
        Expr::Seq { first, then, .. } => seq(*first, *then, env),
        Expr::Var { name, scope, .. } => var(name, scope, env),
        Expr::Fun { args, body, .. } => fun(args, *body, env),
        Expr::Cons { head, tail, .. } => cons(*head, *tail, env),
        Expr::Call { fun, args, .. } => call(*fun, args, env),
        Expr::RecordSelect { .. } => unimplemented!(),
        Expr::ModuleSelect { .. } => unimplemented!(),
        Expr::Let {
            value,
            pattern,
            then,
            ..
        } => let_(*value, pattern, *then, env),
        Expr::RecordCons {
            label, value, tail, ..
        } => expr_record_cons(label, *value, *tail, env),
        Expr::Case {
            subject, clauses, ..
        } => case(*subject, clauses, env),
        Expr::BinOp {
            name, left, right, ..
        } => bin_op(name, *left, *right, env),
    }
}

fn fun(args: Vec<Arg>, body: TypedExpr, env: &mut Env) -> Document {
    "fun"
        .to_doc()
        .append(fun_args(args).append(" ->"))
        .append(break_("", " ").append(expr(body, env)).nest(INDENT))
        .append(break_("", " "))
        .append("end")
        .group()
}

fn external_fun(public: bool, name: String, module: String, fun: String, arity: usize) -> Document {
    let chars: String = (65..(65 + arity))
        .map(|x| x as u8 as char)
        .map(|c| c.to_string())
        .intersperse(", ".to_string())
        .collect();

    let header = format!("{}({}) ->", name, chars).to_doc();
    let body = format!("{}:{}({}).", module, fun, chars).to_doc();

    export(public, &name, arity)
        .append(header)
        .append(line().append(body).nest(INDENT))
}

#[test]
fn module_test() {
    let m = Module {
        typ: crate::typ::int(),
        name: "magic".to_string(),
        statements: vec![
            Statement::ExternalType {
                meta: default(),
                public: true,
                name: "Any".to_string(),
                args: vec![],
            },
            Statement::Enum {
                meta: default(),
                public: true,
                name: "Any".to_string(),
                args: vec![],
                constructors: vec![Type::Constructor {
                    meta: default(),
                    args: vec![],
                    name: "Ok".to_string(),
                }],
            },
            Statement::Import {
                meta: default(),
                module: "result".to_string(),
            },
            Statement::ExternalFun {
                meta: default(),
                args: vec![
                    Type::Constructor {
                        meta: default(),
                        args: vec![],
                        name: "Int".to_string(),
                    },
                    Type::Constructor {
                        meta: default(),
                        args: vec![],
                        name: "Int".to_string(),
                    },
                ],
                name: "add_ints".to_string(),
                fun: "add".to_string(),
                module: "int".to_string(),
                public: false,
                retrn: Type::Constructor {
                    meta: default(),
                    args: vec![],
                    name: "Int".to_string(),
                },
            },
            Statement::ExternalFun {
                meta: default(),
                args: vec![],
                name: "map".to_string(),
                fun: "new".to_string(),
                module: "maps".to_string(),
                public: true,
                retrn: Type::Constructor {
                    meta: default(),
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

    let m = Module {
        typ: crate::typ::int(),
        name: "term".to_string(),
        statements: vec![
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "atom".to_string(),
                body: Expr::Atom {
                    typ: typ::atom(),
                    meta: default(),
                    value: "ok".to_string(),
                },
            },
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "int".to_string(),
                body: Expr::Int {
                    typ: typ::int(),
                    meta: default(),
                    value: 176,
                },
            },
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "float".to_string(),
                body: Expr::Float {
                    meta: default(),
                    typ: typ::float(),
                    value: 11177.324401,
                },
            },
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "nil".to_string(),
                body: Expr::Nil {
                    meta: default(),
                    typ: typ::int(),
                },
            },
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "record_nil".to_string(),
                body: Expr::RecordNil {
                    meta: default(),
                    typ: typ::record_nil(),
                },
            },
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "tup".to_string(),
                body: Expr::Tuple {
                    meta: default(),
                    typ: typ::int(),
                    elems: vec![
                        Expr::Int {
                            typ: typ::int(),
                            meta: default(),
                            value: 1,
                        },
                        Expr::Float {
                            meta: default(),
                            typ: typ::float(),
                            value: 2.0,
                        },
                    ],
                },
            },
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "string".to_string(),
                body: Expr::String {
                    meta: default(),
                    typ: typ::string(),
                    value: "Hello there!".to_string(),
                },
            },
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "seq".to_string(),
                body: Expr::Seq {
                    meta: default(),
                    typ: typ::int(),
                    first: Box::new(Expr::Int {
                        typ: typ::int(),
                        meta: default(),
                        value: 1,
                    }),
                    then: Box::new(Expr::Int {
                        typ: typ::int(),
                        meta: default(),
                        value: 2,
                    }),
                },
            },
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "bin_op".to_string(),
                body: Expr::BinOp {
                    meta: default(),
                    typ: typ::int(),
                    name: BinOp::AddInt,
                    left: Box::new(Expr::Int {
                        typ: typ::int(),
                        meta: default(),
                        value: 1,
                    }),
                    right: Box::new(Expr::Int {
                        typ: typ::int(),
                        meta: default(),
                        value: 2,
                    }),
                },
            },
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "enum1".to_string(),
                body: Expr::Constructor {
                    meta: default(),
                    typ: typ::int(),
                    name: "Nil".to_string(),
                },
            },
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "let".to_string(),
                body: Expr::Let {
                    meta: default(),
                    typ: typ::int(),
                    value: Box::new(Expr::Int {
                        typ: typ::int(),
                        meta: default(),
                        value: 1,
                    }),
                    pattern: Pattern::Var {
                        meta: default(),
                        name: "OneTwo".to_string(),
                    },
                    then: Box::new(Expr::Var {
                        meta: default(),
                        typ: typ::int(),
                        scope: Scope::Local,
                        name: "one_two".to_string(),
                    }),
                },
            },
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "conny".to_string(),
                body: Expr::Cons {
                    meta: default(),
                    typ: typ::int(),
                    head: Box::new(Expr::Int {
                        typ: typ::int(),
                        meta: default(),
                        value: 1234,
                    }),
                    tail: Box::new(Expr::Nil {
                        meta: default(),
                        typ: typ::int(),
                    }),
                },
            },
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "retcon".to_string(),
                body: Expr::RecordCons {
                    meta: default(),
                    typ: typ::int(),
                    label: "size".to_string(),
                    value: Box::new(Expr::Int {
                        typ: typ::int(),
                        meta: default(),
                        value: 1,
                    }),
                    tail: Box::new(Expr::RecordNil {
                        meta: default(),
                        typ: typ::record_nil(),
                    }),
                },
            },
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "funny".to_string(),
                body: Expr::Fun {
                    meta: default(),
                    typ: typ::int(),
                    args: vec![
                        Arg {
                            name: "one_really_long_arg_to_cause_wrapping".to_string(),
                        },
                        Arg {
                            name: "also_really_quite_long".to_string(),
                        },
                    ],
                    body: Box::new(Expr::Int {
                        typ: typ::int(),
                        meta: default(),
                        value: 100000000000,
                    }),
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

record_nil() ->
    #{}.

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

let() ->
    OneTwo = 1,
    OneTwo.

conny() ->
    [1234 | []].

retcon() ->
    #{}#{'size' => 1}.

funny() ->
    fun(OneReallyLongArgToCauseWrapping, AlsoReallyQuiteLong) ->
        100000000000
    end.
"
    .to_string();
    assert_eq!(expected, module(m));

    // TODO
    // enum2() ->
    //     {'ok', 1, 2.0}.

    let m = Module {
        typ: crate::typ::int(),
        name: "term".to_string(),
        statements: vec![Statement::Fun {
            meta: default(),
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
                typ: typ::atom(),
                meta: default(),
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

    let m = Module {
        typ: crate::typ::int(),
        name: "term".to_string(),
        statements: vec![Statement::Test {
            meta: default(),
            name: "bang".to_string(),
            body: Expr::Atom {
                typ: typ::atom(),
                meta: default(),
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

    let m = Module {
        typ: crate::typ::int(),
        name: "vars".to_string(),
        statements: vec![
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "arg".to_string(),
                body: Expr::Var {
                    typ: typ::int(),
                    meta: default(),
                    name: "some_arg".to_string(),
                    scope: Scope::Local,
                },
            },
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "some_arg".to_string(),
                body: Expr::Var {
                    typ: typ::int(),
                    meta: default(),
                    name: "some_arg".to_string(),
                    scope: Scope::Constant {
                        value: Box::new(Expr::Atom {
                            typ: typ::atom(),
                            meta: default(),
                            value: "hello".to_string(),
                        }),
                    },
                },
            },
            Statement::Fun {
                meta: default(),
                public: false,
                args: vec![],
                name: "another".to_string(),
                body: Expr::Var {
                    typ: typ::int(),
                    meta: default(),
                    name: "run_task".to_string(),
                    scope: Scope::Module { arity: 6 },
                },
            },
        ],
    };
    let expected = "-module(vars).

arg() ->
    SomeArg.

some_arg() ->
    'hello'.

another() ->
    fun run_task/6.
"
    .to_string();
    assert_eq!(expected, module(m));

    let m = Module {
        typ: crate::typ::int(),
        name: "my_mod".to_string(),
        statements: vec![Statement::Fun {
            meta: default(),
            public: false,
            args: vec![],
            name: "go".to_string(),
            body: Expr::Case {
                meta: default(),
                typ: typ::int(),
                subject: Box::new(Expr::Int {
                    typ: typ::int(),
                    meta: default(),
                    value: 1,
                }),
                clauses: vec![
                    Clause {
                        meta: default(),
                        pattern: Pattern::Int {
                            meta: default(),
                            value: 1,
                        },
                        then: Box::new(Expr::Int {
                            typ: typ::int(),
                            meta: default(),
                            value: 1,
                        }),
                    },
                    Clause {
                        meta: default(),
                        pattern: Pattern::Float {
                            meta: default(),
                            value: 1.0,
                        },
                        then: Box::new(Expr::Int {
                            typ: typ::int(),
                            meta: default(),
                            value: 1,
                        }),
                    },
                    Clause {
                        meta: default(),
                        pattern: Pattern::Atom {
                            meta: default(),
                            value: "ok".to_string(),
                        },
                        then: Box::new(Expr::Int {
                            typ: typ::int(),
                            meta: default(),
                            value: 1,
                        }),
                    },
                    Clause {
                        meta: default(),
                        pattern: Pattern::String {
                            meta: default(),
                            value: "hello".to_string(),
                        },
                        then: Box::new(Expr::Int {
                            typ: typ::int(),
                            meta: default(),
                            value: 1,
                        }),
                    },
                    Clause {
                        meta: default(),
                        pattern: Pattern::Tuple {
                            meta: default(),
                            elems: vec![
                                Pattern::Int {
                                    meta: default(),
                                    value: 1,
                                },
                                Pattern::Int {
                                    meta: default(),
                                    value: 2,
                                },
                            ],
                        },
                        then: Box::new(Expr::Int {
                            typ: typ::int(),
                            meta: default(),
                            value: 1,
                        }),
                    },
                    Clause {
                        meta: default(),
                        pattern: Pattern::Nil { meta: default() },
                        then: Box::new(Expr::Int {
                            typ: typ::int(),
                            meta: default(),
                            value: 1,
                        }),
                    },
                    Clause {
                        meta: default(),
                        pattern: Pattern::Enum {
                            meta: default(),
                            name: "Error".to_string(),
                            args: vec![Pattern::Int {
                                meta: default(),
                                value: 2,
                            }],
                        },
                        then: Box::new(Expr::Int {
                            typ: typ::int(),
                            meta: default(),
                            value: 1,
                        }),
                    },
                ],
            },
        }],
    };
    let expected = "-module(my_mod).

go() ->
    case 1 of
        1 ->
            1;

        1.0 ->
            1;

        'ok' ->
            1;

        <<\"hello\">> ->
            1;

        {1, 2} ->
            1;

        [] ->
            1;

        {'error', 2} ->
            1
    end.
"
    .to_string();
    assert_eq!(expected, module(m));
    let m = Module {
        typ: crate::typ::int(),
        name: "funny".to_string(),
        statements: vec![
            Statement::Fun {
                meta: default(),
                args: vec![],
                name: "one".to_string(),
                public: false,
                body: Expr::Call {
                    meta: default(),
                    typ: typ::int(),
                    args: vec![Expr::Int {
                        typ: typ::int(),
                        meta: default(),
                        value: 1,
                    }],
                    fun: Box::new(Expr::Var {
                        meta: default(),
                        scope: Scope::Module { arity: 1 },
                        typ: typ::int(),
                        name: "one_two".to_string(),
                    }),
                },
            },
            Statement::Fun {
                meta: default(),
                args: vec![],
                name: "two".to_string(),
                public: false,
                body: Expr::Call {
                    meta: default(),
                    typ: typ::int(),
                    args: vec![Expr::Int {
                        typ: typ::int(),
                        meta: default(),
                        value: 1,
                    }],
                    fun: Box::new(Expr::Var {
                        meta: default(),
                        scope: Scope::Local,
                        typ: typ::int(),
                        name: "one_two".to_string(),
                    }),
                },
            },
            Statement::Fun {
                meta: default(),
                args: vec![],
                name: "three".to_string(),
                public: false,
                body: Expr::Call {
                    meta: default(),
                    typ: typ::int(),
                    args: vec![Expr::Int {
                        typ: typ::int(),
                        meta: default(),
                        value: 2,
                    }],
                    fun: Box::new(Expr::Call {
                        meta: default(),
                        typ: typ::int(),
                        args: vec![Expr::Int {
                            typ: typ::int(),
                            meta: default(),
                            value: 1,
                        }],
                        fun: Box::new(Expr::Var {
                            meta: default(),
                            scope: Scope::Module { arity: 2 },
                            typ: typ::int(),
                            name: "one_two".to_string(),
                        }),
                    }),
                },
            },
        ],
    };
    let expected = "-module(funny).

one() ->
    one_two(1).

two() ->
    OneTwo(1).

three() ->
    (one_two(1))(2).
"
    .to_string();
    assert_eq!(expected, module(m));
}
