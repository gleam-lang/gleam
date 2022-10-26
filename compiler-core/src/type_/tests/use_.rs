use crate::assert_module_infer;

#[test]
fn zero_arity_no_parens() {
    assert_module_infer!(
        r#"
pub fn main() {
  use <- pair
  123
}

fn pair(f) {
  let x = f()
  #(x, x)
}
"#,
        vec![("main", "#(Int, Int)")],
    )
}
