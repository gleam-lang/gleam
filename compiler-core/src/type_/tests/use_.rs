use crate::assert_module_infer;

#[test]
fn zero_arity() {
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
