use crate::assert_module_infer;

#[test]
fn arity_1() {
    assert_module_infer!(
        r#"
pub fn main() {
  use <- pair()
  123
}

fn pair(f) {
  let x = f()
  #(x, x)
}
"#,
        vec![("main", "fn() -> #(Int, Int)")],
    )
}

#[test]
fn arity_2() {
    assert_module_infer!(
        r#"
pub fn main() {
  use <- pair(1.0)
  123
}

fn pair(x, f) {
  let y = f()
  #(x, y)
}
"#,
        vec![("main", "fn() -> #(Float, Int)")],
    )
}

#[test]
fn arity_3() {
    assert_module_infer!(
        r#"
pub fn main() {
  use <- trip(1.0, "")
  123
}

fn trip(x, y, f) {
  let z = f()
  #(x, y, z)
}
"#,
        vec![("main", "fn() -> #(Float, String, Int)")],
    )
}
