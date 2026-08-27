// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2022 The Gleam contributors

use crate::{
    Error,
    analyse::TargetSupport,
    build::{ApiFingerprint, Target},
    manifest::ManifestPackage,
    type_::tests::compile_module_with_opts,
};

use super::project_compiler::{BuildTool, usable_build_tools};

#[test]
fn usable_build_tool_unknown() {
    assert_eq!(
        usable_build_tools(&ManifestPackage::default().with_build_tools(&["unknown"])),
        Err(Error::UnsupportedBuildTool {
            package: "".into(),
            build_tools: vec!["unknown".into()],
        })
    )
}

#[test]
fn usable_build_tool_none() {
    assert_eq!(
        usable_build_tools(&ManifestPackage::default()),
        Err(Error::UnsupportedBuildTool {
            package: "".into(),
            build_tools: vec![],
        })
    )
}

#[test]
fn usable_build_tool_only_mix() {
    assert_eq!(
        usable_build_tools(&ManifestPackage::default().with_build_tools(&["mix"])),
        Ok(vec![BuildTool::Mix])
    )
}

#[test]
fn usable_build_tool_only_rebar3() {
    assert_eq!(
        usable_build_tools(&ManifestPackage::default().with_build_tools(&["rebar3"])),
        Ok(vec![BuildTool::Rebar3])
    )
}

#[test]
fn usable_build_tool_only_gleam() {
    assert_eq!(
        usable_build_tools(&ManifestPackage::default().with_build_tools(&["gleam"])),
        Ok(vec![BuildTool::Gleam])
    )
}

#[test]
fn usable_build_tool_mix_then_rebar3() {
    assert_eq!(
        usable_build_tools(&ManifestPackage::default().with_build_tools(&["mix", "rebar3"])),
        Ok(vec![BuildTool::Mix, BuildTool::Rebar3])
    )
}

fn fingerprint(src: &str) -> ApiFingerprint {
    let ast = compile_module_with_opts(
        "module",
        src,
        None,
        vec![],
        Target::Erlang,
        TargetSupport::NotEnforced,
        None,
    )
    .expect("should successfully infer");

    ApiFingerprint::new(&ast)
}

#[test]
fn deprecating_function_changes_api_fingerprint() {
    assert_ne!(
        fingerprint("pub fn wibble() {}"),
        fingerprint(
            r#"
            @deprecated("some message")
            pub fn wibble() {}
            "#
        )
    )
}

#[test]
fn changing_deprecation_message_invalidates_api_fingerprint() {
    assert_ne!(
        fingerprint(
            r#"
            @deprecated("some message")
            pub fn wibble() {}
            "#
        ),
        fingerprint(
            r#"
            @deprecated("new message")
            pub fn wibble() {}
            "#
        )
    )
}

#[test]
fn shuffling_definitions_does_not_change_api_fingerprint() {
    assert_eq!(
        fingerprint(
            r#"
            pub const wobble = Nil
            pub fn wibble() { Nil }
            "#
        ),
        fingerprint(
            r#"
            pub fn wibble() { Nil }
            pub const wobble = Nil
            "#
        )
    )
}

#[test]
fn switching_names_changes_api_fingerprint() {
    assert_ne!(
        fingerprint(
            r#"
            pub const wobble = Nil
            pub fn wibble() { Nil }
            "#
        ),
        fingerprint(
            r#"
            pub const wibble = Nil
            pub fn wobble() { Nil }
            "#
        )
    )
}

#[test]
fn changing_a_function_body_does_not_change_api_fingerprint() {
    assert_eq!(
        fingerprint("pub fn wibble() { 1 }"),
        fingerprint("pub fn wibble() { 1 + 100 }")
    )
}

#[test]
fn changin_a_constant_value_does_not_change_api_fingerprint() {
    assert_eq!(
        fingerprint(r#"pub const wibble = "hello" "#),
        fingerprint(r#"pub const wibble = "hello" <> "joe" "#)
    )
}

#[test]
fn adding_private_constant_does_not_change_api_fingerprint() {
    assert_eq!(fingerprint(""), fingerprint("const wibble = Nil"))
}

#[test]
fn adding_private_function_does_not_change_api_fingerprint() {
    assert_eq!(fingerprint(""), fingerprint("fn wibble() { Nil }"))
}

#[test]
fn adding_private_alias_does_not_change_api_fingerprint() {
    assert_eq!(fingerprint(""), fingerprint("type Wibble = Int"))
}

#[test]
fn adding_private_type_does_not_change_api_fingerprint() {
    assert_eq!(fingerprint(""), fingerprint("type Wibble { Wobble }"))
}

#[test]
fn adding_variants_to_opaque_type_does_not_change_api_fingerprint() {
    assert_eq!(
        fingerprint("pub opaque type Wibble"),
        fingerprint("pub opaque type Wibble { Wobble }")
    )
}

#[test]
fn removing_variants_from_opaque_type_does_not_change_api_fingerprint() {
    assert_eq!(
        fingerprint("pub opaque type Wibble { Wibble Wobble }"),
        fingerprint("pub opaque type Wibble { Wobble }")
    )
}

#[test]
fn updating_variants_of_opaque_type_does_not_change_api_fingerprint() {
    assert_eq!(
        fingerprint("pub opaque type Wibble { Wibble }"),
        fingerprint("pub opaque type Wibble { Wibble(Int) }")
    )
}

#[test]
fn adding_variants_to_type_changes_api_fingerprint() {
    assert_ne!(
        fingerprint("pub type Wibble"),
        fingerprint("pub type Wibble { Wobble }")
    )
}

#[test]
fn removing_variants_from_type_changes_api_fingerprint() {
    assert_ne!(
        fingerprint("pub type Wibble { Wibble Wobble }"),
        fingerprint("pub type Wibble { Wobble }")
    )
}

#[test]
fn updating_variants_of_type_changes_api_fingerprint() {
    assert_ne!(
        fingerprint("pub type Wibble { Wibble }"),
        fingerprint("pub type Wibble { Wibble(Int) }")
    )
}

#[test]
fn adding_label_to_variant_changes_api_fingerprint() {
    assert_ne!(
        fingerprint("pub type Wibble { Wibble(Int) }"),
        fingerprint("pub type Wibble { Wibble(label: Int) }")
    )
}

#[test]
fn adding_field_to_variant_changes_api_fingerprint() {
    assert_ne!(
        fingerprint("pub type Wibble { Wibble(Int) }"),
        fingerprint("pub type Wibble { Wibble(Int, Int) }")
    )
}

#[test]
fn changing_aliased_type_changes_api_fingerprint() {
    assert_ne!(
        fingerprint("pub type Wibble = Int"),
        fingerprint("pub type Wibble = String")
    )
}

#[test]
fn making_type_opaque_changes_api_fingerprint() {
    assert_ne!(
        fingerprint("pub type Wibble"),
        fingerprint("pub opaque type Wibble")
    )
}

#[test]
fn adding_type_parameter_changes_api_fingerprint() {
    assert_ne!(
        fingerprint("pub type Wibble(a)"),
        fingerprint("pub type Wibble(a, b)")
    )
}

#[test]
fn renaming_type_parameter_does_not_change_api_fingerprint() {
    assert_eq!(
        fingerprint("pub type Wibble(a) { Wibble(a, a) }"),
        fingerprint("pub type Wibble(b) { Wibble(b, b) }")
    )
}

#[test]
fn adding_function_parameter_changes_api_fingerprint() {
    assert_ne!(
        fingerprint("pub fn wibble() {}"),
        fingerprint("pub fn wibble(x) {}")
    )
}

#[test]
fn adding_function_parameter_label_changes_api_fingerprint() {
    assert_ne!(
        fingerprint("pub fn wibble(x) {}"),
        fingerprint("pub fn wibble(label x) {}")
    )
}

#[test]
fn adding_compatible_type_annotations_does_not_change_api_fingeprint() {
    assert_eq!(
        fingerprint("pub fn wibble(x) { x + 1 }"),
        fingerprint("pub fn wibble(x: Int) -> Int { x + 1 }")
    )
}

#[test]
fn changing_argument_type_changes_api_fingerprint() {
    assert_ne!(
        fingerprint("pub fn wibble(x: Int) { todo }"),
        fingerprint("pub fn wibble(x: String) { todo }")
    )
}

#[test]
fn changing_return_type_changes_api_fingerprint() {
    assert_ne!(
        fingerprint("pub fn wibble() { True }"),
        fingerprint("pub fn wibble() { 1 }")
    )
}

#[test]
fn discarding_parameter_does_not_change_api_fingerprint() {
    assert_eq!(
        fingerprint("pub fn wibble(x) { todo }"),
        fingerprint("pub fn wibble(_x) { todo }")
    )
}

#[test]
fn changing_supported_targets_changes_api_fingerprint() {
    assert_ne!(
        fingerprint(
            r#"
            pub fn wibble() -> Int { 1 }
            "#
        ),
        fingerprint(
            r#"
            pub fn wibble() -> Int { erlang_only() }

            @external(erlang, "wibble", "wobble")
            fn erlang_only() -> Int
            "#
        )
    )
}

#[test]
fn adding_externals_does_not_change_api_fingerprint_if_same_targets_are_supported() {
    assert_eq!(
        fingerprint(
            r#"
            pub fn wibble() -> Int { 1 }
            "#
        ),
        fingerprint(
            r#"
            pub fn wibble() -> Int { both_targets() }

            @external(erlang, "wibble", "wobble")
            @external(javascript, "wibble", "wobble")
            fn both_targets() -> Int
            "#
        )
    )
}
