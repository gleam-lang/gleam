use crate::{assert_error, assert_infer, assert_module_infer, assert_warning};

#[test]
fn bool_value() {
    assert_infer!(
        "
let value = True
assert value
",
        "Bool"
    );
}

#[test]
fn equality_check() {
    assert_infer!(
        "
let value = 10
assert value == 10
",
        "Bool"
    );
}

#[test]
fn comparison() {
    assert_infer!(
        "
let value = 4
assert value < 5
",
        "Bool"
    );
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
fn bool_literal() {
    assert_warning!(
        "
pub fn main() {
  assert True
}
"
    );
}

#[test]
fn negation_of_bool_literal() {
    assert_warning!(
        "
pub fn main() {
  assert !False
}
"
    );
}

#[test]
fn equality_check_on_literals() {
    assert_warning!(
        "
pub fn main() {
  assert 1 == 2
}
"
    );
}

#[test]
fn comparison_on_literals() {
    assert_warning!(
        "
pub fn main() {
  assert 1 < 2
}
"
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
