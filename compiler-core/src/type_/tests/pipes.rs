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

// https://github.com/gleam-lang/gleam/pull/3406#discussion_r1683068647
#[test]
fn pipe_rewrite_with_missing_argument() {
    assert_module_infer!(
        r#"
pub fn main() {
  let f = fn(a, b) { fn(c) { a + b + c } }
  1 |> f(2)
}
"#,
        vec![("main", "fn() -> fn(Int) -> Int")]
    );
}
