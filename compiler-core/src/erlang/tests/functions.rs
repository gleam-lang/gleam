use crate::assert_erl;

#[test]
fn function_as_value() {
    assert_erl!(
        r#"
pub fn main() {
  main
}
"#
    );
}

#[test]
fn nested_imported_function_as_value() {
    assert_erl!(
        ("package", "some/other", "pub fn wibble() { Nil }"),
        r#"
import some/other

pub fn main() {
  other.wibble
}
"#
    );
}

#[test]
fn nested_unqualified_imported_function_as_value() {
    assert_erl!(
        ("package", "some/other", "pub fn wibble() { Nil }"),
        r#"
import some/other.{wibble}

pub fn main() {
  wibble
}
"#
    );
}

#[test]
fn nested_aliased_imported_function_as_value() {
    assert_erl!(
        ("package", "some/other", "pub fn wibble() { Nil }"),
        r#"
import some/other.{wibble as wobble}

pub fn main() {
  wobble
}
"#
    );
}

#[test]
fn function_called() {
    assert_erl!(
        r#"
pub fn main() {
  main()
}
"#
    );
}

#[test]
fn nested_imported_function_called() {
    assert_erl!(
        ("package", "some/other", "pub fn wibble() { Nil }"),
        r#"
import some/other

pub fn main() {
  other.wibble()
}
"#
    );
}

#[test]
fn nested_unqualified_imported_function_called() {
    assert_erl!(
        ("package", "some/other", "pub fn wibble() { Nil }"),
        r#"
import some/other.{wibble}

pub fn main() {
  wibble()
}
"#
    );
}

#[test]
fn nested_aliased_imported_function_called() {
    assert_erl!(
        ("package", "some/other", "pub fn wibble() { Nil }"),
        r#"
import some/other.{wibble as wobble}

pub fn main() {
  wobble()
}
"#
    );
}
