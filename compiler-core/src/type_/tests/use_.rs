use crate::assert_module_infer;

#[test]
fn zero_arity_no_parens() {
    assert_module_infer!(
        r#"
pub fn main() {
  use <- call
  123
}

fn call(f) {
  f()
}
"#,
        vec![("main", "Int")],
    )
}
