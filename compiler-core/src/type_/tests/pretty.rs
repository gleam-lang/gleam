use std::sync::Arc;

use crate::type_::{
    prelude::{bool, int, tuple},
    pretty::Printer,
    Type,
};

fn print(type_: Arc<Type>) -> String {
    Printer::new().pretty_print(&type_, 0)
}

fn custom_bool() -> Arc<Type> {
    Arc::new(Type::Named {
        public: true,
        package: "foo".into(),
        module: "one/two".into(),
        name: "Bool".into(),
        args: vec![],
    })
}

#[test]
fn repeated_prelude_type() {
    insta::assert_snapshot!(print(tuple(vec![int(), int(), int()])));
}

#[test]
fn prelude_type_clash_prelude_first() {
    insta::assert_snapshot!(print(tuple(vec![bool(), custom_bool()])));
}

#[test]
fn prelude_type_clash_custom_first() {
    insta::assert_snapshot!(print(tuple(vec![custom_bool(), bool()])));
}
