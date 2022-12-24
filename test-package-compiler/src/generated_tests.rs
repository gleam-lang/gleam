//! This file is generated by build.rs
//! Do not edit it directly, instead add new test cases to ./cases

#[test]
fn erlang_import() {
    let output =
        crate::prepare("/Users/louis/src/gleam/gleam/test-package-compiler/cases/erlang_import");
    insta::assert_snapshot!(
        "erlang_import",
        output,
        "/Users/louis/src/gleam/gleam/test-package-compiler/cases/erlang_import"
    );
}

#[test]
fn erlang_nested() {
    let output =
        crate::prepare("/Users/louis/src/gleam/gleam/test-package-compiler/cases/erlang_nested");
    insta::assert_snapshot!(
        "erlang_nested",
        output,
        "/Users/louis/src/gleam/gleam/test-package-compiler/cases/erlang_nested"
    );
}

#[test]
fn erlang_nested_qualified_constant() {
    let output =
        crate::prepare("/Users/louis/src/gleam/gleam/test-package-compiler/cases/erlang_nested_qualified_constant");
    insta::assert_snapshot!(
        "erlang_nested_qualified_constant",
        output,
        "/Users/louis/src/gleam/gleam/test-package-compiler/cases/erlang_nested_qualified_constant"
    );
}

#[test]
fn erlang_escape_names() {
    let output =
        crate::prepare("/Users/louis/src/gleam/gleam/test-package-compiler/cases/erlang_escape_names");
    insta::assert_snapshot!(
        "erlang_escape_names",
        output,
        "/Users/louis/src/gleam/gleam/test-package-compiler/cases/erlang_escape_names"
    );
}

#[test]
fn erlang_app_generation() {
    let output =
        crate::prepare("/Users/louis/src/gleam/gleam/test-package-compiler/cases/erlang_app_generation");
    insta::assert_snapshot!(
        "erlang_app_generation",
        output,
        "/Users/louis/src/gleam/gleam/test-package-compiler/cases/erlang_app_generation"
    );
}

#[test]
fn import_shadowed_name_warning() {
    let output =
        crate::prepare("/Users/louis/src/gleam/gleam/test-package-compiler/cases/import_shadowed_name_warning");
    insta::assert_snapshot!(
        "import_shadowed_name_warning",
        output,
        "/Users/louis/src/gleam/gleam/test-package-compiler/cases/import_shadowed_name_warning"
    );
}

#[test]
fn record_constructors() {
    let output =
        crate::prepare("/Users/louis/src/gleam/gleam/test-package-compiler/cases/record_constructors");
    insta::assert_snapshot!(
        "record_constructors",
        output,
        "/Users/louis/src/gleam/gleam/test-package-compiler/cases/record_constructors"
    );
}

#[test]
fn javascript_d_ts() {
    let output =
        crate::prepare("/Users/louis/src/gleam/gleam/test-package-compiler/cases/javascript_d_ts");
    insta::assert_snapshot!(
        "javascript_d_ts",
        output,
        "/Users/louis/src/gleam/gleam/test-package-compiler/cases/javascript_d_ts"
    );
}

#[test]
fn erlang_bug_752() {
    let output =
        crate::prepare("/Users/louis/src/gleam/gleam/test-package-compiler/cases/erlang_bug_752");
    insta::assert_snapshot!(
        "erlang_bug_752",
        output,
        "/Users/louis/src/gleam/gleam/test-package-compiler/cases/erlang_bug_752"
    );
}

#[test]
fn erlang_empty() {
    let output =
        crate::prepare("/Users/louis/src/gleam/gleam/test-package-compiler/cases/erlang_empty");
    insta::assert_snapshot!(
        "erlang_empty",
        output,
        "/Users/louis/src/gleam/gleam/test-package-compiler/cases/erlang_empty"
    );
}

#[test]
fn alias_unqualified_import() {
    let output =
        crate::prepare("/Users/louis/src/gleam/gleam/test-package-compiler/cases/alias_unqualified_import");
    insta::assert_snapshot!(
        "alias_unqualified_import",
        output,
        "/Users/louis/src/gleam/gleam/test-package-compiler/cases/alias_unqualified_import"
    );
}

#[test]
fn hello_joe() {
    let output =
        crate::prepare("/Users/louis/src/gleam/gleam/test-package-compiler/cases/hello_joe");
    insta::assert_snapshot!(
        "hello_joe",
        output,
        "/Users/louis/src/gleam/gleam/test-package-compiler/cases/hello_joe"
    );
}

#[test]
fn javascript_import() {
    let output =
        crate::prepare("/Users/louis/src/gleam/gleam/test-package-compiler/cases/javascript_import");
    insta::assert_snapshot!(
        "javascript_import",
        output,
        "/Users/louis/src/gleam/gleam/test-package-compiler/cases/javascript_import"
    );
}

#[test]
fn variable_or_module() {
    let output =
        crate::prepare("/Users/louis/src/gleam/gleam/test-package-compiler/cases/variable_or_module");
    insta::assert_snapshot!(
        "variable_or_module",
        output,
        "/Users/louis/src/gleam/gleam/test-package-compiler/cases/variable_or_module"
    );
}

#[test]
fn erlang_import_shadowing_prelude() {
    let output =
        crate::prepare("/Users/louis/src/gleam/gleam/test-package-compiler/cases/erlang_import_shadowing_prelude");
    insta::assert_snapshot!(
        "erlang_import_shadowing_prelude",
        output,
        "/Users/louis/src/gleam/gleam/test-package-compiler/cases/erlang_import_shadowing_prelude"
    );
}

#[test]
fn javascript_empty() {
    let output =
        crate::prepare("/Users/louis/src/gleam/gleam/test-package-compiler/cases/javascript_empty");
    insta::assert_snapshot!(
        "javascript_empty",
        output,
        "/Users/louis/src/gleam/gleam/test-package-compiler/cases/javascript_empty"
    );
}

