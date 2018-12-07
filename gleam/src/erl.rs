use ast::{Meta, Module, Statement, Type};
use pretty;
use pretty::Document::{self, *};

const INDENT: isize = 2;

pub fn module<T>(module: Module<T>) -> String {
    let mut docs = vec![Text(format!("-module({}).", module.name)), Line];

    docs.append(&mut module.statements.into_iter().map(statement).collect());

    pretty::format(80, pretty::concat(docs))
}

fn statement<T>(statement: Statement<T>) -> Document {
    match statement {
        Statement::Fun { .. } => unimplemented!(),
        Statement::Test { .. } => unimplemented!(),
        Statement::Enum { .. } => Nil,
        Statement::Import { .. } => Nil,
        Statement::ExternalType { .. } => Nil,
        Statement::ExternalFun {
            fun,
            module,
            args,
            public,
            name,
            ..
        } => external_fn(public, name, module, fun, args.len() as u8),
    }
}

fn external_fn(public: bool, name: String, module: String, fun: String, arity: u8) -> Document {
    use itertools::Itertools;
    use std::char;

    let chars: String = (65..(65 + arity))
        .map(|x| x as char)
        .map(|c| c.to_string())
        .intersperse(", ".to_string())
        .collect();

    let header = Text(format!("{}({}) ->", name, chars));
    let body = Text(format!("{}:{}({}).", module, fun, chars));

    let export = if public {
        pretty::cons(Text(format!("-export([{}/{}]).", name, arity)), Line)
    } else {
        Nil
    };

    pretty::concat(vec![
        Line,
        export,
        header,
        indent(pretty::cons(Line, body)),
        Line,
    ])
}

fn indent(content: Document) -> Document {
    Nest(INDENT, Box::new(content))
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
".to_string();
    assert_eq!(expected, module(m));
}
