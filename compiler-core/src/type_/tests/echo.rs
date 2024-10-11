use crate::assert_module_infer;

#[test]
pub fn echo_has_same_type_as_printed_expression() {
    assert_module_infer!(
        r#"
pub fn main() {
  echo 1
}
"#,
        vec![("main", "fn() -> Int")]
    );
}

#[test]
pub fn echo_has_same_type_as_printed_expression_2() {
    assert_module_infer!(
        r#"
pub fn main() {
  let wibble = todo
  echo wibble
}
"#,
        vec![("main", "fn() -> a")]
    );
}

#[test]
pub fn echo_in_pipeline_acts_as_the_identity_function() {
    assert_module_infer!(
        r#"
pub fn main() {
  [1, 2, 3]
  |> echo
}
"#,
        vec![("main", "fn() -> List(Int)")]
    );
}

#[test]
pub fn echo_in_pipeline_acts_as_the_identity_function_2() {
    assert_module_infer!(
        r#"
pub fn main() {
  1
  |> echo
  |> fn(_: Int) { True }
}
"#,
        vec![("main", "fn() -> Bool")]
    );
}

#[test]
pub fn echo_in_pipeline_acts_as_the_identity_function_3() {
    assert_module_infer!(
        r#"
pub fn main() {
  [1, 2, 3]
  |> echo
  |> echo
  |> wibble
}

fn wibble(_: List(Int)) -> List(String) { todo }
"#,
        vec![("main", "fn() -> List(String)")]
    );
}
