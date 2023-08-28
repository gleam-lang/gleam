use crate::assert_module_infer;

// https://github.com/gleam-lang/gleam/issues/2215
#[test]
fn generic_phantom() {
    assert_module_infer!(
        r#"
pub type Test(a) {
  MakeTest(field: Test(Int))
}
"#,
        vec![("MakeTest", "fn(Test(Int)) -> Test(a)")]
    );
}
