use crate::assert_erl;

#[test]
fn function_with_documentation() {
    assert_erl!(
        r#"
/// Function doc!
pub fn documented() { 1 }"#
    );
}

#[test]
fn function_with_multiline_documentation() {
    assert_erl!(
        r#"
/// Function doc!
/// Hello!!
///
pub fn documented() { 1 }"#
    );
}

#[test]
fn quotes_in_documentation_are_escaped() {
    assert_erl!(
        r#"
/// "hello"
pub fn documented() { 1 }"#
    );
}

#[test]
fn backslashes_in_documentation_are_escaped() {
    assert_erl!(
        r#"
/// \hello\
pub fn documented() { 1 }"#
    );
}
