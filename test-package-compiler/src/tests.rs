// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2024 The Gleam contributors

use crate::{Compilation, TestHarness};

#[test]
fn alias_unqualified_import() {
    let output = crate::run_package_compiler("alias_unqualified_import");
    insta::assert_snapshot!(
        "alias_unqualified_import",
        output,
        "./cases/alias_unqualified_import",
    );
}

#[test]
fn dev_importing_test() {
    let output = crate::run_package_compiler("dev_importing_test");
    insta::assert_snapshot!("dev_importing_test", output, "./cases/dev_importing_test",);
}

#[test]
fn duplicate_module() {
    let output = crate::run_package_compiler("duplicate_module");
    insta::assert_snapshot!("duplicate_module", output, "./cases/duplicate_module",);
}

#[test]
fn duplicate_module_dev() {
    let output = crate::run_package_compiler("duplicate_module_dev");
    insta::assert_snapshot!(
        "duplicate_module_dev",
        output,
        "./cases/duplicate_module_dev",
    );
}

#[test]
fn duplicate_module_test_dev() {
    let output = crate::run_package_compiler("duplicate_module_test_dev");
    insta::assert_snapshot!(
        "duplicate_module_test_dev",
        output,
        "./cases/duplicate_module_test_dev",
    );
}

#[test]
fn empty_module_warning() {
    let output = crate::run_package_compiler("empty_module_warning");
    insta::assert_snapshot!(
        "empty_module_warning",
        output,
        "./cases/empty_module_warning",
    );
}

#[test]
fn erlang_app_generation() {
    let output = crate::run_package_compiler("erlang_app_generation");
    insta::assert_snapshot!(
        "erlang_app_generation",
        output,
        "./cases/erlang_app_generation",
    );
}

#[test]
fn erlang_app_generation_with_argument() {
    let output = crate::run_package_compiler("erlang_app_generation_with_argument");
    insta::assert_snapshot!(
        "erlang_app_generation_with_argument",
        output,
        "./cases/erlang_app_generation_with_argument",
    );
}

#[test]
fn erlang_bug_752() {
    let output = crate::run_package_compiler("erlang_bug_752");
    insta::assert_snapshot!("erlang_bug_752", output, "./cases/erlang_bug_752",);
}

#[test]
fn erlang_empty() {
    let output = crate::run_package_compiler("erlang_empty");
    insta::assert_snapshot!("erlang_empty", output, "./cases/erlang_empty",);
}

#[test]
fn erlang_escape_names() {
    let output = crate::run_package_compiler("erlang_escape_names");
    insta::assert_snapshot!("erlang_escape_names", output, "./cases/erlang_escape_names",);
}

#[test]
fn erlang_import() {
    let output = crate::run_package_compiler("erlang_import");
    insta::assert_snapshot!("erlang_import", output, "./cases/erlang_import",);
}

#[test]
fn erlang_import_shadowing_prelude() {
    let output = crate::run_package_compiler("erlang_import_shadowing_prelude");
    insta::assert_snapshot!(
        "erlang_import_shadowing_prelude",
        output,
        "./cases/erlang_import_shadowing_prelude",
    );
}

#[test]
fn erlang_nested() {
    let output = crate::run_package_compiler("erlang_nested");
    insta::assert_snapshot!("erlang_nested", output, "./cases/erlang_nested",);
}

#[test]
fn erlang_nested_qualified_constant() {
    let output = crate::run_package_compiler("erlang_nested_qualified_constant");
    insta::assert_snapshot!(
        "erlang_nested_qualified_constant",
        output,
        "./cases/erlang_nested_qualified_constant",
    );
}

#[test]
fn errors_from_related_modules() {
    let output = crate::run_package_compiler("errors_from_related_modules");
    insta::assert_snapshot!(
        "errors_from_related_modules",
        output,
        "./cases/errors_from_related_modules",
    );
}

#[test]
fn errors_from_related_skipped_modules() {
    let output = crate::run_package_compiler("errors_from_related_skipped_modules");
    insta::assert_snapshot!(
        "errors_from_related_skipped_modules",
        output,
        "./cases/errors_from_related_skipped_modules",
    );
}

#[test]
fn errors_from_unrelated_modules() {
    let output = crate::run_package_compiler("errors_from_unrelated_modules");
    insta::assert_snapshot!(
        "errors_from_unrelated_modules",
        output,
        "./cases/errors_from_unrelated_modules",
    );
}

#[test]
fn hello_joe() {
    let output = crate::run_package_compiler("hello_joe");
    insta::assert_snapshot!("hello_joe", output, "./cases/hello_joe",);
}

#[test]
fn import_cycle() {
    let output = crate::run_package_compiler("import_cycle");
    insta::assert_snapshot!("import_cycle", output, "./cases/import_cycle",);
}

#[test]
fn import_cycle_multi() {
    let output = crate::run_package_compiler("import_cycle_multi");
    insta::assert_snapshot!("import_cycle_multi", output, "./cases/import_cycle_multi",);
}

#[test]
fn import_shadowed_name_warning() {
    let output = crate::run_package_compiler("import_shadowed_name_warning");
    insta::assert_snapshot!(
        "import_shadowed_name_warning",
        output,
        "./cases/import_shadowed_name_warning",
    );
}

#[test]
fn imported_constants() {
    let output = crate::run_package_compiler("imported_constants");
    insta::assert_snapshot!("imported_constants", output, "./cases/imported_constants",);
}

#[test]
fn imported_external_fns() {
    let output = crate::run_package_compiler("imported_external_fns");
    insta::assert_snapshot!(
        "imported_external_fns",
        output,
        "./cases/imported_external_fns",
    );
}

#[test]
fn imported_record_constructors() {
    let output = crate::run_package_compiler("imported_record_constructors");
    insta::assert_snapshot!(
        "imported_record_constructors",
        output,
        "./cases/imported_record_constructors",
    );
}

#[test]
fn javascript_d_ts() {
    let output = crate::run_package_compiler("javascript_d_ts");
    insta::assert_snapshot!("javascript_d_ts", output, "./cases/javascript_d_ts",);
}

#[test]
fn javascript_empty() {
    let output = crate::run_package_compiler("javascript_empty");
    insta::assert_snapshot!("javascript_empty", output, "./cases/javascript_empty",);
}

#[test]
fn javascript_import() {
    let output = crate::run_package_compiler("javascript_import");
    insta::assert_snapshot!("javascript_import", output, "./cases/javascript_import",);
}

#[test]
fn javascript_sourcemaps() {
    let output = crate::run_package_compiler("javascript_sourcemaps");
    insta::assert_snapshot!(
        "javascript_sourcemaps",
        output,
        "./cases/javascript_sourcemaps",
    );
}

#[test]
fn not_overwriting_erlang_module() {
    let output = crate::run_package_compiler("not_overwriting_erlang_module");
    insta::assert_snapshot!(
        "not_overwriting_erlang_module",
        output,
        "./cases/not_overwriting_erlang_module",
    );
}

#[test]
fn opaque_type_accessor() {
    let output = crate::run_package_compiler("opaque_type_accessor");
    insta::assert_snapshot!(
        "opaque_type_accessor",
        output,
        "./cases/opaque_type_accessor",
    );
}

#[test]
fn opaque_type_destructure() {
    let output = crate::run_package_compiler("opaque_type_destructure");
    insta::assert_snapshot!(
        "opaque_type_destructure",
        output,
        "./cases/opaque_type_destructure",
    );
}

#[test]
fn overwriting_erlang_module() {
    let output = crate::run_package_compiler("overwriting_erlang_module");
    insta::assert_snapshot!(
        "overwriting_erlang_module",
        output,
        "./cases/overwriting_erlang_module",
    );
}

#[test]
fn src_importing_dev() {
    let output = crate::run_package_compiler("src_importing_dev");
    insta::assert_snapshot!("src_importing_dev", output, "./cases/src_importing_dev",);
}

#[test]
fn src_importing_test() {
    let output = crate::run_package_compiler("src_importing_test");
    insta::assert_snapshot!("src_importing_test", output, "./cases/src_importing_test",);
}

#[test]
fn unknown_module_field_in_constant() {
    let output = crate::run_package_compiler("unknown_module_field_in_constant");
    insta::assert_snapshot!(
        "unknown_module_field_in_constant",
        output,
        "./cases/unknown_module_field_in_constant",
    );
}

#[test]
fn unknown_module_field_in_expression() {
    let output = crate::run_package_compiler("unknown_module_field_in_expression");
    insta::assert_snapshot!(
        "unknown_module_field_in_expression",
        output,
        "./cases/unknown_module_field_in_expression",
    );
}

#[test]
fn unknown_module_field_in_import() {
    let output = crate::run_package_compiler("unknown_module_field_in_import");
    insta::assert_snapshot!(
        "unknown_module_field_in_import",
        output,
        "./cases/unknown_module_field_in_import",
    );
}

#[test]
fn variable_or_module() {
    let output = crate::run_package_compiler("variable_or_module");
    insta::assert_snapshot!("variable_or_module", output, "./cases/variable_or_module",);
}

#[test]
fn src_only() {
    let mut compiler = TestHarness::new();
    // This one should fail due to the type errors in test/ and dev/.
    compiler
        .compile(Compilation::for_package("src_only"))
        .unwrap_err();
    // This one should succeed
    compiler
        .compile(Compilation::for_package("src_only").src_only())
        .unwrap();
    let output = compiler.into_snapshot();
    insta::assert_snapshot!("src_only", output, "./cases/src_only");
}

#[test]
fn otp_app_override() {
    let mut compiler = TestHarness::new();
    compiler
        .compile(
            Compilation::for_package("otp_app_override")
                .otp_app_override("hpack_erl", "hpack")
                .otp_app_override("uuid_erl", "uuid"),
        )
        .unwrap();
    let output = compiler.into_snapshot();
    insta::assert_snapshot!("otp_app_override", output, "./cases/otp_app_override");
}
