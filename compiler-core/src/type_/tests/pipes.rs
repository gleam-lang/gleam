use crate::{assert_infer, assert_module_infer};

// https://github.com/gleam-lang/gleam/issues/2392
#[test]
fn empty_list() {
    assert_module_infer!(
        "
pub fn a() {
  fn(_) { Nil }
}

pub fn b(_) {
  fn(_) { Nil }
}

pub fn c() {
  Nil
  |> b(
    Nil
    |> a(),
  )
}",
        vec![
            ("a", "fn() -> fn(a) -> Nil"),
            ("b", "fn(a) -> fn(b) -> Nil"),
            ("c", "fn() -> Nil"),
        ]
    );
}

// https://github.com/gleam-lang/gleam/issues/2504
#[test]
fn provide_argument_type_to_anonymous_function() {
    assert_infer!(
        "
   let a = 1
     |> fn (x) { #(x, x + 1) }
     |> fn (x) { x.0 }
     |> fn (x) { x }
",
        "Int"
    );
}
