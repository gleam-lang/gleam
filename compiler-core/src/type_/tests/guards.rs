use crate::{assert_module_error, assert_module_infer};

#[test]
fn nested_record_access() {
    assert_module_infer!(
        r#"
pub type A {
  A(b: B)
}

pub type B {
  B(c: C)
}

pub type C {
  C(d: Bool)
}

pub fn a(a: A) {
  case a {
    _ if a.b.c.d -> 1
    _ -> 0
  }
}
"#,
        vec![
            ("A", "fn(B) -> A"),
            ("B", "fn(C) -> B"),
            ("C", "fn(Bool) -> C"),
            ("a", "fn(A) -> Int"),
        ],
    );
}

#[test]
fn string_variable_access() {
    assert_module_error!(
        r#"
pub fn a(a: String) {
  case a {
    _ if a.b -> 1
    _ -> 0
  }
}
"#
    );
}
