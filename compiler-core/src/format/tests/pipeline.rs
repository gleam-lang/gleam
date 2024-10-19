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

#[test]
pub fn adding_a_newline_to_a_pipeline_splits_all() {
    assert_format_rewrite!(
        r#"pub fn main() {
  wibble |> wobble
  |> wabble
}
"#,
        r#"pub fn main() {
  wibble
  |> wobble
  |> wabble
}
"#,
    );
}

#[test]
pub fn multiline_function_inside_pipeline_function_argument_is_indented_properly() {
    assert_format!(
        r#"pub fn main() {
  function(
    arg0,
    thing
      |> string.replace(
        "{something something}",
        date.month_to_string(month, config.l10n.context),
      ),
  )
}
"#,
    );
}

#[test]
pub fn multiline_function_inside_pipeline_in_list_is_indented_properly() {
    assert_format!(
        r#"pub fn main() {
  [
    item1,
    thing
      |> string.replace(
        "{something something}",
        date.month_to_string(month, config.l10n.context),
      ),
  ]
}
"#,
    );
}

#[test]
pub fn multiline_function_inside_pipeline_in_tuple_is_indented_properly() {
    assert_format!(
        r#"pub fn main() {
  #(
    item1,
    thing
      |> string.replace(
        "{something something}",
        date.month_to_string(month, config.l10n.context),
      ),
  )
}
"#,
    );
}

#[test]
fn pipe_with_labelled_first_argument_capture() {
    assert_format!(
        "fn wibble(label1 a, label2 b, lots c, of d, labels e) {
  a + b * c - d / e
}

fn main() {
  1 |> wibble(label1: _, label2: 2, lots: 3, of: 4, labels: 5)
}
"
    );
}

#[test]
fn pipe_with_labelled_only_argument_capture() {
    assert_format!(
        "fn wibble(descriptive_label value) {
  value
}

fn main() {
  42 |> wibble(descriptive_label: _)
}
"
    );
}
