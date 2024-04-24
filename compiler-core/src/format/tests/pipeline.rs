use crate::{assert_format, assert_format_rewrite};

#[test]
pub fn single_line_pipeline_longer_than_line_limit_gets_split() {
    assert_format_rewrite!(
        r#"pub fn main() {
  wibble |> wobble |> loooooooooooooooooooooooooooooooooooooooooooong_function_name
}
"#,
        r#"pub fn main() {
  wibble
  |> wobble
  |> loooooooooooooooooooooooooooooooooooooooooooong_function_name
}
"#,
    );
}

#[test]
pub fn single_line_pipeline_shorter_than_line_limit_is_kept_on_a_single_line() {
    assert_format!(
        r#"pub fn main() {
  wibble(1) |> wobble
}
"#
    );
}

#[test]
pub fn multi_line_pipeline_is_split_no_matter_the_length() {
    assert_format!(
        r#"pub fn main() {
  wibble(1)
  |> wobble
}
"#
    );
}
