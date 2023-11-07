use crate::assert_module_infer;

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
