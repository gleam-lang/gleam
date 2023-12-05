use crate::assert_erl;

#[test]
fn panic_as() {
    assert_erl!(
        r#"
pub fn main() {
  panic as "wibble"
}
"#
    );
}

#[test]
fn plain() {
    assert_erl!(
        r#"
pub fn main() {
  panic
}
"#
    );
}

#[test]
fn panic_as_function() {
    assert_erl!(
        r#"
pub fn retstring() {
  "wibble"
}
pub fn main() {
  panic as { retstring() <> "wobble" }
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
      |> panic as "pipe"
      |> panic as "other panic"
    }
    "#
    );
}