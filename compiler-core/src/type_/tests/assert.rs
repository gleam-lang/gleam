use crate::{assert_error, assert_infer, assert_module_infer};

#[test]
fn bool_literal_true() {
    assert_infer!("assert True", "Bool");
}

#[test]
fn bool_literal_false() {
    assert_infer!("assert False", "Bool");
}

#[test]
fn equality_check() {
    assert_infer!("assert 1 == 2", "Bool");
}

#[test]
fn comparison() {
    assert_infer!("assert 1 < 2", "Bool");
}

#[test]
fn function_call() {
    assert_module_infer!(
        "
fn bool() {
  True
}

pub fn main() {
  assert bool()
}
",
        vec![("main", "fn() -> Bool")]
    );
}

#[test]
fn with_message() {
    assert_infer!(r#"assert True as "This should never panic""#, "Bool");
}

#[test]
fn compound_message() {
    assert_infer!(
        r#"assert 1 == 2 as { "one" <> " is never equal to " <> "two" }"#,
        "Bool"
    );
}

#[test]
fn mismatched_types() {
    assert_error!("assert 10");
}

#[test]
fn wrong_message_type() {
    assert_error!("assert True as 10");
}
