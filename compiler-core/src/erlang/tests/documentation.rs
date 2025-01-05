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

#[test]
fn single_line_module_comment() {
    assert_erl!(
        r#"
//// Hello! This is a single line module comment.

pub fn main() { 1 }"#
    );
}

#[test]
fn multi_line_module_comment() {
    assert_erl!(
        r#"
//// Hello! This is a multi-
//// line module comment.
////

pub fn main() { 1 }"#
    );
}

#[test]
fn double_quotes_are_escaped_in_module_comment() {
    assert_erl!(
        r#"
//// "quotes!"

pub fn main() { 1 }"#
    );
}

#[test]
fn backslashes_are_escaped_in_module_comment() {
    assert_erl!(
        r#"
//// \backslashes!\

pub fn main() { 1 }"#
    );
}
