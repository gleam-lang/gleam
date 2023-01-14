use crate::{assert_js, assert_ts_def};

#[test]
fn type_() {
    assert_js!(r#"pub external type Thing"#,);
}

#[test]
fn module_fn() {
    assert_js!(r#"external fn show(anything) -> Nil = "utils" "inspect""#,);
}

#[test]
fn pub_module_fn() {
    assert_js!(r#"pub external fn show(anything) -> Nil = "utils" "inspect""#,);
}

#[test]
fn pub_module_fn_typescript() {
    assert_ts_def!(r#"pub external fn show(anything) -> Nil = "utils" "inspect""#,);
}

#[test]
fn global_fn() {
    assert_js!(r#"external fn down(Float) -> Float = "" "Math.floor""#,);
}

#[test]
fn pub_global_fn() {
    assert_js!(r#"pub external fn down(Float) -> Float = "" "Math.floor""#,);
}

#[test]
fn pub_global_fn_typescript() {
    assert_ts_def!(r#"pub external fn down(Float) -> Float = "" "Math.floor""#,);
}

#[test]
fn same_name_global_external() {
    assert_js!(r#"pub external fn fetch(Nil) -> Nil = "" "fetch""#,);
}

#[test]
fn same_module_multiple_imports() {
    assert_js!(
        r#"pub external fn one() -> Nil = "./the/module.mjs" "one"
pub external fn two() -> Nil = "./the/module.mjs" "two"
"#,
    );
}

#[test]
fn duplicate_import() {
    assert_js!(
        r#"pub external fn one() -> Nil = "./the/module.mjs" "dup"
pub external fn two() -> Nil = "./the/module.mjs" "dup"
"#,
    );
}

#[test]
fn name_to_escape() {
    assert_js!(
        r#"pub external fn class() -> Nil = "./the/module.mjs" "one"
"#,
    );
}

#[test]
fn external_type_typescript() {
    assert_ts_def!(
        r#"pub external type Queue(a)
pub external fn new() -> Queue(a) = "queue" "new"
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/1636
#[test]
fn external_fn_escaping() {
    assert_js!(r#"pub external fn then(a) -> b = "./ffi.js" "then""#,);
}

// https://github.com/gleam-lang/gleam/issues/1954
#[test]
fn pipe_variable_shadow() {
    assert_js!(
        r#"
external fn name() -> String = "module" "string"

pub fn main() {
  let name = name()
  name
}
"#
    );
}
