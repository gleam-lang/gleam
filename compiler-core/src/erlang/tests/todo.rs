use crate::assert_erl;

#[test]
fn plain() {
    assert_erl!(
        r#"
pub fn main() {
  todo
}
"#
    );
}

#[test]
fn todo_as() {
    assert_erl!(
        r#"
pub fn main() {
  todo as "wibble"
}
"#
    );
}

#[test]
fn named() {
    assert_erl!(
        r#"
pub fn main() {
  todo as "testing"
}
"#
    );
}

#[test]
fn todo_as_function() {
    assert_erl!(
        r#"
pub fn retstring() {
  "wibble"
}
pub fn main() {
  todo as { retstring() <> "wobble" }
}
"#
    );
}

#[test]
fn piped() {
    assert_erl!(
        r#"
     pub fn main() {
      "lets"
      |> todo as "pipe"
      |> todo as "other todo"
    }
    "#
    );
}
