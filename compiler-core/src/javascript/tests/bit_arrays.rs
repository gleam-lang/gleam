use crate::{assert_js, assert_ts_def};

#[test]
fn empty() {
    assert_js!(
        r#"
fn go() {
  <<>>
}
"#,
    );
}

#[test]
fn one() {
    assert_js!(
        r#"
fn go() {
  <<256>>
}
"#,
    );
}

#[test]
fn two() {
    assert_js!(
        r#"
fn go() {
  <<256, 4>>
}
"#,
    );
}

#[test]
fn integer() {
    assert_js!(
        r#"
fn go() {
  <<256:int>>
}
"#,
    );
}

#[test]
fn float() {
    assert_js!(
        r#"
fn go() {
  <<1.1:float>>
}
"#,
    );
}

#[test]
fn sized() {
    assert_js!(
        r#"
fn go() {
  <<256:4>>
}
"#,
    );
}

#[test]
fn explicit_sized() {
    assert_js!(
        r#"
fn go() {
  <<256:size(4)>>
}
"#,
    );
}

#[test]
fn variable_sized() {
    assert_js!(
        r#"
fn go(x, y) {
  <<x:size(y)>>
}
"#,
    );
}

#[test]
fn variable() {
    assert_js!(
        r#"
fn go(x) {
  <<256, 4, x>>
}
"#,
    );
}

#[test]
fn utf8() {
    assert_js!(
        r#"
fn go(x) {
  <<256, 4, x, "Gleam":utf8>>
}
"#,
    );
}

#[test]
fn utf8_codepoint() {
    assert_js!(
        r#"
fn go(x) {
  <<x:utf8_codepoint, "Gleam":utf8>>
}
"#,
    );
}

#[test]
fn utf8_codepoint_typescript() {
    assert_ts_def!(
        r#"
pub fn go(x) {
  <<x:utf8_codepoint, "Gleam":utf8>>
}
"#,
    );
}

#[test]
fn bit_string() {
    assert_js!(
        r#"
fn go(x) {
  <<x:bits>>
}
"#,
    );
}

#[test]
fn bits() {
    assert_js!(
        r#"
fn go(x) {
  <<x:bits>>
}
"#,
    );
}

#[test]
fn bit_string_typescript() {
    assert_ts_def!(
        r#"
pub fn go(x) {
  <<x:bits>>
}
"#,
    );
}

#[test]
fn bits_typescript() {
    assert_ts_def!(
        r#"
pub fn go(x) {
  <<x:bits>>
}
"#,
    );
}

#[test]
fn empty_match() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<>> = x
}
"#,
    );
}

#[test]
fn match_bytes() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<1, y>> = x
}
"#,
    );
}

#[test]
fn match_sized() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:16, b:8>> = x
}
"#,
    );
}

#[test]
fn discard_sized() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<_:16, _:8>> = x
}
"#,
    );
}

#[test]
fn match_sized_value() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<258:16>> = x
}
"#,
    );
}

#[test]
fn match_float() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<a:float, b:int>> = x
}
"#,
    );
}

#[test]
fn match_rest() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<_, b:bytes>> = <<1,2,3>>
}
"#,
    );
}

#[test]
fn match_rest_deprecated() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<_, b:bytes>> = <<1,2,3>>
}
"#,
    );
}

#[test]
fn match_binary_size() {
    assert_js!(
        r#"
fn go(x) {
  let assert <<_, a:2-bytes>> = x
  let assert <<_, b:bytes-size(2)>> = x
}
"#,
    );
}

#[test]
fn as_module_const() {
    assert_js!(
        r#"
          pub const data = <<
            0x1,
            2,
            2:size(16),
            0x4:size(32),
            "Gleam":utf8,
            4.2:float,
            <<
              <<1, 2, 3>>:bits,
              "Gleam":utf8,
              1024
            >>:bits
          >>
        "#
    )
}
