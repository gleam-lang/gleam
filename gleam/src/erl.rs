use ast::{Meta, Module, Statement, Type};
use pretty;
use pretty::Document::{self, *};

pub fn module<T>(module: Module<T>) -> String {
    let header = Cons(
        Box::new(Text(format!("-module({}).", module.name))),
        Box::new(Line),
    );

    let doc = Cons(
        Box::new(header),
        Box::new(pretty::concat(
            module.statements.into_iter().map(statement).collect(),
        )),
    );

    pretty::format(80, doc)
}

fn statement<T>(statement: Statement<T>) -> Document {
    match statement {
        Statement::Fun { .. } => unimplemented!(),
        Statement::Test { .. } => unimplemented!(),
        Statement::Enum { .. } => Nil,
        Statement::Import { .. } => unimplemented!(),
        Statement::ExternalFun { .. } => unimplemented!(),
        Statement::ExternalType { .. } => Nil,
    }
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
        ],
    };
    let expected = "-module(magic).\n".to_string();
    assert_eq!(expected, module(m));
}
