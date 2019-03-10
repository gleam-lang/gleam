use crate::ast::*;
use crate::pretty::*;
use heck::{CamelCase, SnakeCase};
use im::hashmap::HashMap;
use itertools::Itertools;
use std::char;
use std::default::Default;

const INDENT: isize = 4;

#[derive(Debug, Clone, Default)]
struct Env {
    vars: HashMap<String, usize>,
}

impl Env {
    pub fn local_var_name(&mut self, name: String) -> Document {
        match self.vars.get(&name) {
            None => {
                self.vars.insert(name.clone(), 0);
                name.to_camel_case().to_doc()
            }
            Some(0) => name.to_camel_case().to_doc(),
            Some(n) => name.to_camel_case().to_doc().append(*n),
        }
    }

    pub fn next_local_var_name(&mut self, name: String) -> Document {
        self.vars
            .insert(name.clone(), self.vars.get(&name).map_or(0, |i| i + 1));
        self.local_var_name(name)
    }
}

pub fn module(module: TypedModule) -> String {
    let has_tests = module.statements.iter().any(|x| match x {
        Statement::Test { .. } => true,
        _ => false,
    });
    let test_header = if has_tests {
        line().append("-include_lib(\"eunit/include/eunit.hrl\").")
    } else {
        nil()
    };

    let exports: Vec<_> = module
        .statements
        .iter()
        .flat_map(|s| match s {
            Statement::Fn {
                public: true,
                name,
                args,
                ..
            } => Some((name.clone(), args.len())),

            Statement::ExternalFn {
                public: true,
                name,
                args,
                ..
            } => Some((name.clone(), args.len())),

            _ => None,
        })
        .map(|(n, a)| n.to_doc().append("/").append(a))
        .intersperse(", ".to_doc())
        .collect();

    format!("-module({}).", module.name)
        .to_doc()
        .append(test_header)
        .append(lines(2))
        .append("-export([")
        .append(exports)
        .append("]).")
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
        Statement::Fn {
            args, name, body, ..
        } => Some(mod_fun(name, args, body)),
        Statement::ExternalFn {
            fun,
            module,
            args,
            name,
            ..
        } => Some(external_fun(name, module, fun, args.len())),
    }
}

fn mod_fun(name: String, args: Vec<Arg>, body: TypedExpr) -> Document {
    let body_doc = expr(body, &mut Env::default());

    name.to_doc()
        .append(fun_args(args))
        .append(" ->")
        .append(line().append(body_doc).nest(INDENT).group())
        .append(".")
}

fn fun_args(args: Vec<Arg>) -> Document {
    wrap_args(args.into_iter().map(|a| match a.name {
        None => "_".to_doc(),
        Some(name) => name.to_camel_case().to_doc(),
    }))
}

fn call_args(args: Vec<TypedExpr>, env: &mut Env) -> Document {
    wrap_args(args.into_iter().map(|e| wrap_expr(e, env)))
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

fn atom(value: String) -> Document {
    use regex::Regex;
    // TODO: Lazy static to avoid regex recompilation
    let re = Regex::new(r"^[a-z_\$]+$").unwrap();
    if re.is_match(&value) {
        value.to_doc()
    } else {
        value.to_doc().surround("'", "'")
    }
}

fn string(value: String) -> Document {
    value.to_doc().surround("<<\"", "\">>")
}

fn tuple(elems: Vec<Document>) -> Document {
    elems
        .into_iter()
        .intersperse(delim(","))
        .collect::<Vec<_>>()
        .to_doc()
        .nest_current()
        .surround("{", "}")
        .group()
}

fn seq(first: TypedExpr, then: TypedExpr, env: &mut Env) -> Document {
    force_break()
        .append(expr(first, env))
        .append(",")
        .append(line())
        .append(expr(then, env))
}

// TODO: Surround left or right in parens if required
// TODO: Group nested bin_ops i.e. a |> b |> c
fn bin_op(name: BinOp, left: TypedExpr, right: TypedExpr, env: &mut Env) -> Document {
    let op = match name {
        BinOp::Pipe => return pipe(left, right, env),
        BinOp::Lt => "<",
        BinOp::And => "andalso",
        BinOp::Or => "orelse",
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
        BinOp::ModuloInt => "rem",
        BinOp::ModuloFloat => "rem", // TODO: remove
    };

    expr(left, env)
        .append(break_("", " "))
        .append(op)
        .append(" ")
        .append(expr(right, env))
}

fn pipe(value: TypedExpr, fun: TypedExpr, env: &mut Env) -> Document {
    call(fun, vec![value], env)
}

fn let_(value: TypedExpr, pat: Pattern, then: TypedExpr, env: &mut Env) -> Document {
    pattern(pat, env)
        .append(" = ")
        .append(expr(value, env))
        .append(",")
        .append(line())
        .append(expr(then, env))
}

fn pattern(p: Pattern, env: &mut Env) -> Document {
    match p {
        Pattern::Nil { .. } => "[]".to_doc(),
        Pattern::Cons { head, tail, .. } => pattern_cons(*head, *tail, env),
        Pattern::Discard { .. } => "_".to_doc(),
        // Pattern::Record { .. } => unimplemented!(),
        Pattern::Var { name, .. } => env.next_local_var_name(name),
        Pattern::Int { value, .. } => value.to_doc(),
        Pattern::Float { value, .. } => value.to_doc(),
        Pattern::Tuple { elems, .. } => tuple(elems.into_iter().map(|p| pattern(p, env)).collect()),
        Pattern::String { value, .. } => string(value),
        Pattern::Constructor { name, args, .. } => enum_pattern(name, args, env),
    }
}

fn pattern_cons(head: Pattern, tail: Pattern, env: &mut Env) -> Document {
    cons(head, tail, env, pattern, |expr| match expr {
        Pattern::Nil { .. } => ListType::Nil,

        Pattern::Cons { head, tail, .. } => ListType::Cons {
            head: *head,
            tail: *tail,
        },

        other => ListType::NotList(other),
    })
}

fn expr_cons(head: TypedExpr, tail: TypedExpr, env: &mut Env) -> Document {
    cons(head, tail, env, wrap_expr, |expr| match expr {
        Expr::Nil { .. } => ListType::Nil,

        Expr::Cons { head, tail, .. } => ListType::Cons {
            head: *head,
            tail: *tail,
        },

        other => ListType::NotList(other),
    })
}

fn cons<ToDoc, Categorise, Elem>(
    head: Elem,
    tail: Elem,
    env: &mut Env,
    to_doc: ToDoc,
    categorise_element: Categorise,
) -> Document
where
    ToDoc: Fn(Elem, &mut Env) -> Document,
    Categorise: Fn(Elem) -> ListType<Elem>,
{
    let mut elems = vec![head];
    let final_tail = collect_list(tail, &mut elems, categorise_element);

    let mut elems = elems
        .into_iter()
        .map(|e| to_doc(e, env))
        .intersperse(delim(","))
        .collect::<Vec<_>>();

    match final_tail {
        Some(final_tail) => {
            elems.push(delim(" |"));
            elems.push(to_doc(final_tail, env))
        }

        None => (),
    };

    elems.to_doc().nest_current().surround("[", "]").group()
}

fn collect_list<F, E>(e: E, elems: &mut Vec<E>, f: F) -> Option<E>
where
    F: Fn(E) -> ListType<E>,
{
    match f(e) {
        ListType::Nil => None,

        ListType::Cons { head, tail } => {
            elems.push(head);
            collect_list(tail, elems, f)
        }

        ListType::NotList(other) => Some(other),
    }
}

enum ListType<E> {
    Nil,
    Cons { head: E, tail: E },
    NotList(E),
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
        Scope::Local => env.local_var_name(name),
        Scope::Import { module } => module.to_doc(),
        Scope::Module { arity, .. } => "fun ".to_doc().append(name).append("/").append(arity),
        Scope::Constant { value } => expr(*value, env),
    }
}

fn enum_pattern(name: String, args: Vec<Pattern>, env: &mut Env) -> Document {
    if args.len() == 0 {
        atom(name.to_snake_case())
    } else {
        let mut args: Vec<_> = args.into_iter().map(|p| pattern(p, env)).collect();
        // FIXME: O(n), insert at start shuffles the elemes forward by one place
        args.insert(0, atom(name.to_snake_case()));
        tuple(args)
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
        .append(wrap_expr(subject, env).group())
        .append(" of")
        .append(line().append(clauses(cs, env)).nest(INDENT))
        .append(line())
        .append("end")
        .group()
}

fn call(fun: TypedExpr, args: Vec<TypedExpr>, env: &mut Env) -> Document {
    match fun {
        Expr::Var {
            scope: Scope::Module { .. },
            name,
            ..
        } => name.to_doc().append(call_args(args, env)),

        Expr::ModuleSelect { module, label, .. } => expr(*module, env)
            .append(":")
            .append(label)
            .append(call_args(args, env)),

        Expr::Constructor { name, .. } => {
            let mut args: Vec<_> = args.into_iter().map(|e| wrap_expr(e, env)).collect();
            // FIXME: O(n), insert at start shuffles the elemes forward by one place
            args.insert(0, atom(name.to_snake_case()));
            tuple(args)
        }

        call @ Expr::Call { .. } => expr(call, env)
            .surround("(", ")")
            .append(call_args(args, env)),

        fun @ Expr::Fn { .. } => expr(fun, env)
            .surround("(", ")")
            .append(call_args(args, env)),

        other => expr(other, env).append(call_args(args, env)),
    }
}

/// Wrap a document in begin end
///
fn begin_end(document: Document) -> Document {
    force_break()
        .append("begin")
        .append(line().append(document).nest(INDENT))
        .append(line())
        .append("end")
}

/// Same as expr, expect it wraps seq, let, etc in begin end
///
fn wrap_expr(expression: TypedExpr, env: &mut Env) -> Document {
    match &expression {
        Expr::Seq { .. } => begin_end(expr(expression, env)),
        Expr::Let { .. } => begin_end(expr(expression, env)),
        _ => expr(expression, env),
    }
}

fn expr(expression: TypedExpr, env: &mut Env) -> Document {
    match expression {
        Expr::Nil { .. } => "[]".to_doc(),
        Expr::RecordNil { .. } => "#{}".to_doc(),
        Expr::Int { value, .. } => value.to_doc(),
        Expr::Float { value, .. } => value.to_doc(),
        Expr::Constructor { name, .. } => atom(name.to_snake_case()),
        Expr::String { value, .. } => string(value),
        Expr::Seq { first, then, .. } => seq(*first, *then, env),
        Expr::Var { name, scope, .. } => var(name, scope, env),
        Expr::Fn { args, body, .. } => fun(args, *body, env),
        Expr::Cons { head, tail, .. } => expr_cons(*head, *tail, env),
        Expr::Call { fun, args, .. } => call(*fun, args, env),
        Expr::RecordSelect { label, record, .. } => record_select(*record, label, env),
        Expr::ModuleSelect {
            typ, label, module, ..
        } => module_select(typ, *module, label, env),
        Expr::Tuple { elems, .. } => tuple(elems.into_iter().map(|e| wrap_expr(e, env)).collect()),
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

fn module_select(
    typ: crate::typ::Type,
    module: TypedExpr,
    label: String,
    env: &mut Env,
) -> Document {
    match typ.collapse_links() {
        crate::typ::Type::Fn { args, .. } => "fun "
            .to_doc()
            .append(expr(module, env))
            .append(":")
            .append(label)
            .append("/")
            .append(args.len()),

        _ => expr(module, env).append(":").append(label).append("()"),
    }
}

// TODO: Nest, break, etc
fn record_select(record: TypedExpr, label: String, env: &mut Env) -> Document {
    "maps:get("
        .to_doc()
        .append(atom(label))
        .append(", ")
        .append(wrap_expr(record, env))
        .append(")")
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

fn external_fun(name: String, module: String, fun: String, arity: usize) -> Document {
    let chars: String = (65..(65 + arity))
        .map(|x| x as u8 as char)
        .map(|c| c.to_string())
        .intersperse(", ".to_string())
        .collect();

    format!("{}({}) ->", name, chars)
        .to_doc()
        .append(line())
        .append(atom(module))
        .append(format!(":{}({}).", fun, chars))
        .nest(INDENT)
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
                constructors: vec![EnumConstructor {
                    meta: default(),
                    name: "Ok".to_string(),
                    args: vec![],
                }],
            },
            Statement::Import {
                meta: default(),
                module: "result".to_string(),
            },
            Statement::ExternalFn {
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
            Statement::ExternalFn {
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

-export([map/0]).

add_ints(A, B) ->
    int:add(A, B).

map() ->
    maps:new().
"
    .to_string();
    assert_eq!(expected, module(m));

    let m = Module {
        typ: crate::typ::int(),
        name: "term".to_string(),
        statements: vec![
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "int".to_string(),
                body: Expr::Int {
                    typ: crate::typ::int(),
                    meta: default(),
                    value: 176,
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "float".to_string(),
                body: Expr::Float {
                    meta: default(),
                    typ: crate::typ::float(),
                    value: 11177.324401,
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "nil".to_string(),
                body: Expr::Nil {
                    meta: default(),
                    typ: crate::typ::int(),
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "record_nil".to_string(),
                body: Expr::RecordNil {
                    meta: default(),
                    typ: crate::typ::record_nil(),
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "tup".to_string(),
                body: Expr::Tuple {
                    meta: default(),
                    typ: crate::typ::int(),
                    elems: vec![
                        Expr::Int {
                            typ: crate::typ::int(),
                            meta: default(),
                            value: 1,
                        },
                        Expr::Float {
                            meta: default(),
                            typ: crate::typ::float(),
                            value: 2.0,
                        },
                    ],
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "string".to_string(),
                body: Expr::String {
                    meta: default(),
                    typ: crate::typ::string(),
                    value: "Hello there!".to_string(),
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "seq".to_string(),
                body: Expr::Seq {
                    meta: default(),
                    typ: crate::typ::int(),
                    first: Box::new(Expr::Int {
                        typ: crate::typ::int(),
                        meta: default(),
                        value: 1,
                    }),
                    then: Box::new(Expr::Int {
                        typ: crate::typ::int(),
                        meta: default(),
                        value: 2,
                    }),
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "bin_op".to_string(),
                body: Expr::BinOp {
                    meta: default(),
                    typ: crate::typ::int(),
                    name: BinOp::AddInt,
                    left: Box::new(Expr::Int {
                        typ: crate::typ::int(),
                        meta: default(),
                        value: 1,
                    }),
                    right: Box::new(Expr::Int {
                        typ: crate::typ::int(),
                        meta: default(),
                        value: 2,
                    }),
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "enum1".to_string(),
                body: Expr::Constructor {
                    meta: default(),
                    typ: crate::typ::int(),
                    name: "Nil".to_string(),
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "let".to_string(),
                body: Expr::Let {
                    meta: default(),
                    typ: crate::typ::int(),
                    value: Box::new(Expr::Int {
                        typ: crate::typ::int(),
                        meta: default(),
                        value: 1,
                    }),
                    pattern: Pattern::Var {
                        meta: default(),
                        name: "OneTwo".to_string(),
                    },
                    then: Box::new(Expr::Var {
                        meta: default(),
                        typ: crate::typ::int(),
                        scope: Scope::Local,
                        name: "one_two".to_string(),
                    }),
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "conny".to_string(),
                body: Expr::Cons {
                    meta: default(),
                    typ: crate::typ::int(),
                    head: Box::new(Expr::Int {
                        typ: crate::typ::int(),
                        meta: default(),
                        value: 12,
                    }),
                    tail: Box::new(Expr::Cons {
                        meta: default(),
                        typ: crate::typ::int(),
                        head: Box::new(Expr::Int {
                            typ: crate::typ::int(),
                            meta: default(),
                            value: 34,
                        }),
                        tail: Box::new(Expr::Nil {
                            meta: default(),
                            typ: crate::typ::int(),
                        }),
                    }),
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "retcon".to_string(),
                body: Expr::RecordCons {
                    meta: default(),
                    typ: crate::typ::int(),
                    label: "size".to_string(),
                    value: Box::new(Expr::Int {
                        typ: crate::typ::int(),
                        meta: default(),
                        value: 1,
                    }),
                    tail: Box::new(Expr::RecordNil {
                        meta: default(),
                        typ: crate::typ::record_nil(),
                    }),
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "funny".to_string(),
                body: Expr::Fn {
                    meta: default(),
                    is_capture: false,
                    typ: crate::typ::int(),
                    args: vec![
                        Arg {
                            name: Some("one_really_long_arg_to_cause_wrapping".to_string()),
                        },
                        Arg {
                            name: Some("also_really_quite_long".to_string()),
                        },
                    ],
                    body: Box::new(Expr::Int {
                        typ: crate::typ::int(),
                        meta: default(),
                        value: 100000000000,
                    }),
                },
            },
        ],
    };
    let expected = "-module(term).

-export([]).

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
    nil.

let() ->
    OneTwo = 1,
    OneTwo.

conny() ->
    [12, 34].

retcon() ->
    #{}#{size => 1}.

funny() ->
    fun(OneReallyLongArgToCauseWrapping, AlsoReallyQuiteLong) ->
        100000000000
    end.
"
    .to_string();
    assert_eq!(expected, module(m));

    let m = Module {
        typ: crate::typ::int(),
        name: "term".to_string(),
        statements: vec![Statement::Fn {
            meta: default(),
            public: false,
            name: "some_function".to_string(),
            args: vec![
                Arg {
                    name: Some("arg_one".to_string()),
                },
                Arg {
                    name: Some("arg_two".to_string()),
                },
                Arg {
                    name: Some("arg_3".to_string()),
                },
                Arg {
                    name: Some("arg4".to_string()),
                },
                Arg {
                    name: Some("arg_four".to_string()),
                },
                Arg {
                    name: Some("arg__five".to_string()),
                },
                Arg {
                    name: Some("arg_six".to_string()),
                },
                Arg {
                    name: Some("arg_that_is_long".to_string()),
                },
            ],
            body: Expr::Int {
                typ: crate::typ::int(),
                meta: default(),
                value: 1,
            },
        }],
    };
    let expected = "-module(term).

-export([]).

some_function(ArgOne,
              ArgTwo,
              Arg3,
              Arg4,
              ArgFour,
              ArgFive,
              ArgSix,
              ArgThatIsLong) ->
    1.
"
    .to_string();
    assert_eq!(expected, module(m));

    let m = Module {
        typ: crate::typ::int(),
        name: "term".to_string(),
        statements: vec![Statement::Test {
            meta: default(),
            name: "bang".to_string(),
            body: Expr::Int {
                typ: crate::typ::int(),
                meta: default(),
                value: 1,
            },
        }],
    };
    let expected = r#"-module(term).
-include_lib("eunit/include/eunit.hrl").

-export([]).

-ifdef(TEST).
bang_test() ->
    1.
-endif.
"#
    .to_string();
    assert_eq!(expected, module(m));

    let m = Module {
        typ: crate::typ::int(),
        name: "vars".to_string(),
        statements: vec![
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "arg".to_string(),
                body: Expr::Var {
                    typ: crate::typ::int(),
                    meta: default(),
                    name: "some_arg".to_string(),
                    scope: Scope::Local,
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "some_arg".to_string(),
                body: Expr::Var {
                    typ: crate::typ::int(),
                    meta: default(),
                    name: "some_arg".to_string(),
                    scope: Scope::Constant {
                        value: Box::new(Expr::Int {
                            typ: crate::typ::int(),
                            meta: default(),
                            value: 1,
                        }),
                    },
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "another".to_string(),
                body: Expr::Var {
                    typ: crate::typ::int(),
                    meta: default(),
                    name: "run_task".to_string(),
                    scope: Scope::Module { arity: 6 },
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "moddy".to_string(),
                body: Expr::ModuleSelect {
                    typ: crate::typ::Type::Fn {
                        args: vec![],
                        retrn: Box::new(crate::typ::int()),
                    },
                    meta: default(),
                    module: Box::new(Expr::Var {
                        meta: default(),
                        name: "zero".to_string(),
                        scope: Scope::Import {
                            module: "one".to_string(),
                        },
                        typ: crate::typ::int(),
                    }),
                    label: "two".to_string(),
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "moddy2".to_string(),
                body: Expr::ModuleSelect {
                    typ: crate::typ::Type::Fn {
                        args: vec![crate::typ::int(), crate::typ::int()],
                        retrn: Box::new(crate::typ::int()),
                    },
                    meta: default(),
                    module: Box::new(Expr::Var {
                        meta: default(),
                        name: "zero".to_string(),
                        scope: Scope::Import {
                            module: "one".to_string(),
                        },
                        typ: crate::typ::int(),
                    }),
                    label: "two".to_string(),
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "moddy3".to_string(),
                body: Expr::ModuleSelect {
                    typ: crate::typ::int(),
                    meta: default(),
                    module: Box::new(Expr::Var {
                        meta: default(),
                        name: "zero".to_string(),
                        scope: Scope::Import {
                            module: "one".to_string(),
                        },
                        typ: crate::typ::int(),
                    }),
                    label: "two".to_string(),
                },
            },
            Statement::Fn {
                meta: default(),
                public: false,
                args: vec![],
                name: "moddy4".to_string(),
                body: Expr::Call {
                    meta: default(),
                    typ: crate::typ::int(),
                    args: vec![Expr::Int {
                        meta: default(),
                        typ: crate::typ::int(),
                        value: 1,
                    }],
                    fun: Box::new(Expr::ModuleSelect {
                        typ: crate::typ::int(),
                        meta: default(),
                        module: Box::new(Expr::Var {
                            meta: default(),
                            name: "zero".to_string(),
                            scope: Scope::Import {
                                module: "one".to_string(),
                            },
                            typ: crate::typ::int(),
                        }),
                        label: "two".to_string(),
                    }),
                },
            },
        ],
    };
    let expected = "-module(vars).

-export([]).

arg() ->
    SomeArg.

some_arg() ->
    1.

another() ->
    fun run_task/6.

moddy() ->
    fun one:two/0.

moddy2() ->
    fun one:two/2.

moddy3() ->
    one:two().

moddy4() ->
    one:two(1).
"
    .to_string();
    assert_eq!(expected, module(m));

    let m = Module {
        typ: crate::typ::int(),
        name: "my_mod".to_string(),
        statements: vec![Statement::Fn {
            meta: default(),
            public: false,
            args: vec![],
            name: "go".to_string(),
            body: Expr::Case {
                meta: default(),
                typ: crate::typ::int(),
                subject: Box::new(Expr::Int {
                    typ: crate::typ::int(),
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
                            typ: crate::typ::int(),
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
                            typ: crate::typ::int(),
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
                            typ: crate::typ::int(),
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
                            typ: crate::typ::int(),
                            meta: default(),
                            value: 1,
                        }),
                    },
                    Clause {
                        meta: default(),
                        pattern: Pattern::Nil { meta: default() },
                        then: Box::new(Expr::Int {
                            typ: crate::typ::int(),
                            meta: default(),
                            value: 1,
                        }),
                    },
                    Clause {
                        meta: default(),
                        pattern: Pattern::Constructor {
                            meta: default(),
                            module: None,
                            name: "Error".to_string(),
                            args: vec![Pattern::Int {
                                meta: default(),
                                value: 2,
                            }],
                        },
                        then: Box::new(Expr::Int {
                            typ: crate::typ::int(),
                            meta: default(),
                            value: 1,
                        }),
                    },
                ],
            },
        }],
    };
    let expected = "-module(my_mod).

-export([]).

go() ->
    case 1 of
        1 ->
            1;

        1.0 ->
            1;

        <<\"hello\">> ->
            1;

        {1, 2} ->
            1;

        [] ->
            1;

        {error, 2} ->
            1
    end.
"
    .to_string();
    assert_eq!(expected, module(m));
    let m = Module {
        typ: crate::typ::int(),
        name: "funny".to_string(),
        statements: vec![
            Statement::Fn {
                meta: default(),
                args: vec![],
                name: "one".to_string(),
                public: false,
                body: Expr::Call {
                    meta: default(),
                    typ: crate::typ::int(),
                    args: vec![Expr::Int {
                        typ: crate::typ::int(),
                        meta: default(),
                        value: 1,
                    }],
                    fun: Box::new(Expr::Var {
                        meta: default(),
                        scope: Scope::Module { arity: 1 },
                        typ: crate::typ::int(),
                        name: "one_two".to_string(),
                    }),
                },
            },
            Statement::Fn {
                meta: default(),
                args: vec![],
                name: "two".to_string(),
                public: false,
                body: Expr::Call {
                    meta: default(),
                    typ: crate::typ::int(),
                    args: vec![Expr::Int {
                        typ: crate::typ::int(),
                        meta: default(),
                        value: 1,
                    }],
                    fun: Box::new(Expr::Var {
                        meta: default(),
                        scope: Scope::Local,
                        typ: crate::typ::int(),
                        name: "one_two".to_string(),
                    }),
                },
            },
            Statement::Fn {
                meta: default(),
                args: vec![],
                name: "three".to_string(),
                public: false,
                body: Expr::Call {
                    meta: default(),
                    typ: crate::typ::int(),
                    args: vec![Expr::Int {
                        typ: crate::typ::int(),
                        meta: default(),
                        value: 2,
                    }],
                    fun: Box::new(Expr::Call {
                        meta: default(),
                        typ: crate::typ::int(),
                        args: vec![Expr::Int {
                            typ: crate::typ::int(),
                            meta: default(),
                            value: 1,
                        }],
                        fun: Box::new(Expr::Var {
                            meta: default(),
                            scope: Scope::Module { arity: 2 },
                            typ: crate::typ::int(),
                            name: "one_two".to_string(),
                        }),
                    }),
                },
            },
        ],
    };
    let expected = "-module(funny).

-export([]).

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

#[test]
fn integration_test() {
    struct Case {
        src: &'static str,
        erl: &'static str,
    }

    let cases = [
        Case {
            src: r#"fn go() {
  let x = {100000000000000000, {2000000000, 3000000000000, 40000000000}, 50000, 6000000000}
  x
}"#,
            erl: r#"-module().

-export([]).

go() ->
    X = {100000000000000000,
         {2000000000, 3000000000000, 40000000000},
         50000,
         6000000000},
    X.
"#,
        },
        Case {
            src: r#"fn go() {
  let y = 1
  let y = 2
  y
}"#,
            erl: r#"-module().

-export([]).

go() ->
    Y = 1,
    Y1 = 2,
    Y1.
"#,
        },
        Case {
            src: r#"fn go(x) {
    case begin 1 x end {
    | y ->
        1
    }
}

fn x() {
    go(begin 1 2 end)
}"#,
            erl: r#"-module().

-export([]).

go(X) ->
    case begin
        1,
        X
    end of
        Y ->
            1
    end.

x() ->
    go(begin
           1,
           2
       end).
"#,
        },
        Case {
            src: r#"pub enum Money = | Pound(Int)
                    fn pound(x) {
                      Pound(x)
                    }"#,
            erl: r#"-module().

-export([]).

pound(X) ->
    {pound, X}.
"#,
        },
        Case {
            src: r#"fn loop() { loop() }"#,
            erl: r#"-module().

-export([]).

loop() ->
    loop().
"#,
        },
        Case {
            src: r#"external fn run() -> Int = "Elixir.MyApp" "run""#,
            erl: r#"-module().

-export([]).

run() ->
    'Elixir.MyApp':run().
"#,
        },
        Case {
            src: r#"fn inc(x) { x + 1 }
                    pub fn go() { 1 |> inc |> inc |> inc }"#,
            erl: r#"-module().

-export([go/0]).

inc(X) ->
    X + 1.

go() ->
    inc(inc(inc(1))).
"#,
        },
        Case {
            src: r#"fn add(x, y) { x + y }
                    pub fn go() { 1 |> add(_, 1) |> add(2, _) |> add(_, 3) }"#,
            erl: r#"-module().

-export([go/0]).

add(X, Y) ->
    X + Y.

go() ->
    (fun(Capture1) ->
        add(Capture1, 3)
    end)((fun(Capture1) ->
             add(2, Capture1)
         end)((fun(Capture1) -> add(Capture1, 1) end)(1))).
"#,
        },
        Case {
            src: r#"fn and(x, y) { x && y }
                    fn or(x, y) { x || y }
                    fn modulo(x, y) { x % y }
            "#,
            erl: r#"-module().

-export([]).

and(X, Y) ->
    X andalso Y.

or(X, Y) ->
    X orelse Y.

modulo(X, Y) ->
    X rem Y.
"#,
        },
        Case {
            src: r#"fn second(list) { case list { | [x, y] -> y | z -> 1 } }
                    fn tail(list) { case list { | [x | xs] -> xs | z -> list } }
            "#,
            erl: r#"-module().

-export([]).

second(List) ->
    case List of
        [X, Y] ->
            Y;

        Z ->
            1
    end.

tail(List) ->
    case List of
        [X | Xs] ->
            Xs;

        Z ->
            List
    end.
"#,
        },
        Case {
            src: r#"fn age(x) { x.age }"#,
            erl: r#"-module().

-export([]).

age(X) ->
    maps:get(age, X).
"#,
        },
        // TODO: Check that var num is incremented for args
        // TODO: Check that var num is incremented for nested patterns
        // TODO: Check that var num is reset after a closure that increments
    ];

    for Case { src, erl } in cases.into_iter() {
        let ast = crate::grammar::ModuleParser::new()
            .parse(src)
            .expect("syntax error");
        let ast = crate::typ::infer_module(ast, &std::collections::HashMap::new())
            .expect("should successfully infer");
        let output = module(ast);
        assert_eq!((src, output), (src, erl.to_string()));
    }
}

#[cfg(test)]
fn default<T>() -> T
where
    T: Default,
{
    Default::default()
}
