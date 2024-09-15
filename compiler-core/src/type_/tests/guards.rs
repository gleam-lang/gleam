use crate::{assert_module_error, assert_module_infer, assert_no_warnings};

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

#[test]
fn constant_record_with_function_in_guard_compiles() {
    assert_no_warnings!(
        r#"
pub fn test_function(x: Int) -> Int {
  x + 1
}

pub type Test {
  Test(input_string: String, func: fn(Int) -> Int)
}

pub const case_test = Test("test", test_function)

pub fn main() {
  let case_var = ["test", "test2"]
  case case_var {
    [head, ..] if head == case_test.input_string -> {
      io.println("test")
    }
    _ -> {
      io.println("not test")
    }
  }
}
"#
    );
}
