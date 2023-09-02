use crate::assert_format;

#[test]
fn field_access() {
    assert_format!(
        r#"pub fn main() {
  case x {
    _ if a.b -> 1
    _ -> 0
  }
}
"#
    );
}

#[test]
fn nested_field_access() {
    assert_format!(
        r#"pub fn main() {
  case x {
    _ if a.b.c.d -> 1
    _ -> 0
  }
}
"#
    );
}
