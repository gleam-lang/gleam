use crate::{
    assert_js_module_error, assert_js_module_infer, assert_module_error, assert_module_infer,
    assert_with_module_error,
};

// https://github.com/gleam-lang/gleam/issues/2324
#[test]
fn javascript_only_function_used_by_erlang_module() {
    let module = r#"@external(javascript, "one", "two")
fn js_only() -> Int

pub fn main() {
  js_only()
}
"#;
    assert_module_error!(module);
    assert_js_module_infer!(module, vec![("main", "fn() -> Int")]);
}

#[test]
fn erlang_only_function_used_by_javascript_module() {
    let module = r#"@external(erlang, "one", "two")
fn erlang_only() -> Int

pub fn main() {
  erlang_only()
}
"#;
    assert_js_module_error!(module);
    assert_module_infer!(module, vec![("main", "fn() -> Int")]);
}

#[test]
fn unused_javascript_only_function_is_not_rejected_on_erlang_target() {
    assert_module_infer!(
        r#"@external(javascript, "one", "two")
fn js_only() -> Int

pub fn main() {
  10
}
"#,
        vec![("main", "fn() -> Int")]
    );
}

#[test]
fn unused_erlang_only_function_is_not_rejected_on_javascript_target() {
    assert_js_module_infer!(
        r#"@external(erlang, "one", "two")
fn erlang_only() -> Int

pub fn main() {
  10
}
"#,
        vec![("main", "fn() -> Int")]
    );
}

#[test]
fn erlang_only_function_with_javascript_external() {
    let module = r#"
@external(erlang, "one", "two")
fn erlang_only() -> Int

@external(javascript, "one", "two")
fn all_targets() -> Int {
  erlang_only()
}

pub fn main() {
  all_targets()
}
    "#;

    let expected = vec![("main", "fn() -> Int")];

    assert_module_infer!(module, expected.clone());
    assert_js_module_infer!(module, expected);
}

#[test]
fn javascript_only_function_with_erlang_external() {
    let module = r#"
@external(javascript, "one", "two")
fn javascript_only() -> Int

@external(erlang, "one", "two")
fn all_targets() -> Int {
  javascript_only()
}

pub fn main() {
  all_targets()
}
    "#;

    let expected = vec![("main", "fn() -> Int")];

    assert_module_infer!(module, expected.clone());
    assert_js_module_infer!(module, expected);
}

#[test]
fn javascript_only_function_with_javascript_external() {
    let module = r#"@external(javascript, "one", "two")
fn javascript_only() -> Int

@external(javascript, "one", "two")
pub fn uh_oh() -> Int {
  javascript_only()
}
"#;
    assert_js_module_infer!(module, vec![("uh_oh", "fn() -> Int")]);
    assert_module_error!(module);
}

#[test]
fn erlang_only_function_with_erlang_external() {
    let module = r#"@external(erlang, "one", "two")
fn erlang_only() -> Int

@external(erlang, "one", "two")
pub fn uh_oh() -> Int {
  erlang_only()
}
"#;
    assert_js_module_error!(module);
    assert_module_infer!(module, vec![("uh_oh", "fn() -> Int")]);
}

#[test]
fn erlang_targeted_function_cant_contain_javascript_only_function() {
    let module = r#"@target(erlang)
pub fn erlang_only() -> Int {
  javascript_only()
}

@external(javascript, "one", "two")
fn javascript_only() -> Int
    "#;
    assert_js_module_infer!(module, vec![]);
    assert_module_error!(module);
}

#[test]
fn javascript_targeted_function_cant_contain_erlang_only_function() {
    let module = r#"@target(javascript)
pub fn javascript_only() -> Int {
  erlang_only()
}

@external(erlang, "one", "two")
fn erlang_only() -> Int
    "#;
    assert_module_infer!(module, vec![]);
    assert_js_module_error!(module);
}

#[test]
fn imported_javascript_only_function() {
    assert_with_module_error!(
        (
            "module",
            r#"@external(javascript, "one", "two")
pub fn javascript_only() -> Int"#
        ),
        "import module
pub fn main() {
  module.javascript_only()
}",
    );
}

#[test]
fn javascript_only_constant() {
    assert_with_module_error!(
        (
            "module",
            r#"@external(javascript, "one", "two")
fn javascript_only() -> Int
const constant = javascript_only
pub const javascript_only_constant = constant 
"#
        ),
        "import module
pub fn main() {
  module.javascript_only_constant()
}",
    );
}

#[test]
fn public_javascript_external() {
    let module = r#"@external(javascript, "one", "two")
pub fn main() -> Int
"#;
    assert_module_error!(module);
    assert_js_module_infer!(module, vec![("main", "fn() -> Int")]);
}

#[test]
fn public_erlang_external() {
    let module = r#"@external(erlang, "one", "two")
pub fn main() -> Int
"#;
    assert_module_infer!(module, vec![("main", "fn() -> Int")]);
    assert_js_module_error!(module);
}
