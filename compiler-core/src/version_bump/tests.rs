// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use super::{VersionChanges, detect_changes};
use crate::{
    analyse::TargetSupport,
    build::{Origin, Target},
    config::PackageConfig,
    package_interface::{ModuleInterface, PackageInterface},
    type_::{PRELUDE_MODULE_NAME, build_prelude},
    uid::UniqueIdGenerator,
    version_bump::show_diffs,
    warning::{TypeWarningEmitter, VectorWarningEmitterIO, WarningEmitter},
};
use camino::Utf8PathBuf;
use ecow::{EcoString, eco_format};
use src_span::LineNumbers;
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

fn get_bump(old: &str, new: &str) -> EcoString {
    get_bumps(vec![("the_module", old)], vec![("the_module", new)])
}

fn get_bumps(old_modules: Vec<(&str, &str)>, new_modules: Vec<(&str, &str)>) -> EcoString {
    let old_modules = compile_modules(old_modules)
        .into_iter()
        .filter(|(_, module)| !module.is_internal)
        .map(|(name, module)| (name, ModuleInterface::from_interface(&module)))
        .collect();

    let new_modules = compile_modules(new_modules);

    let package_interface = PackageInterface {
        name: "thepackage".into(),
        version: "1.0.0".into(),
        gleam_version_constraint: None,
        modules: old_modules,
    };

    match detect_changes(&package_interface, &new_modules) {
        VersionChanges::Major(changes) => eco_format!(
            "-- MAJOR\n\n{}",
            show_diffs(changes, &package_interface, &new_modules),
        ),
        VersionChanges::Minor(changes) => eco_format!(
            "-- MINOR\n\n{}",
            show_diffs(changes, &package_interface, &new_modules),
        ),
        VersionChanges::Patch => "-- PATCH".into(),
    }
}

fn compile_modules(
    modules: Vec<(&str, &str)>,
) -> im::HashMap<EcoString, crate::type_::ModuleInterface> {
    let ids = UniqueIdGenerator::new();
    let mut module_interfaces = im::HashMap::new();
    let emitter = WarningEmitter::new(Rc::new(VectorWarningEmitterIO::default()));
    let _ = module_interfaces.insert(PRELUDE_MODULE_NAME.into(), build_prelude(&ids));

    for (name, src) in modules {
        let parsed = crate::parse::parse_module(Utf8PathBuf::from("test/path"), src, &emitter)
            .expect("syntax error");
        let mut ast = parsed.module;
        ast.name = name.into();

        let line_numbers = LineNumbers::new(src);
        let mut config = PackageConfig::default();
        config.name = "thepackage".into();

        let module = crate::analyse::ModuleAnalyzerConstructor::<()> {
            target: Target::Erlang,
            ids: &ids,
            origin: Origin::Src,
            importable_modules: &module_interfaces,
            warnings: &TypeWarningEmitter::null(),
            direct_dependencies: &HashMap::new(),
            dev_dependencies: &HashSet::new(),
            target_support: TargetSupport::Enforced,
            package_config: &config,
        }
        .infer_module(ast, line_numbers, "".into())
        .expect("should successfully infer");

        _ = module_interfaces.insert(name.into(), module.type_info);
    }

    _ = module_interfaces.remove(PRELUDE_MODULE_NAME);
    module_interfaces
}

macro_rules! assert_bump {
    ($old:literal, $new:literal) => {
        let bump = get_bump($old, $new);
        insta::assert_snapshot!(insta::internals::AutoName, bump);
    };

    ($(($old_name:literal, $old_src:literal)),+; $(($new_name:literal, $new_src:literal)),+) => {
        let bump = get_bumps(vec![$(($old_name, $old_src)),*], vec![$(($new_name, $new_src)),*]);
        insta::assert_snapshot!(insta::internals::AutoName, bump);
    };
}

#[test]
fn changing_type_alias_is_major() {
    assert_bump!("pub type Number = Int", "pub type Number = Float");
}

#[test]
fn changing_type_alias_generics_is_major() {
    assert_bump!(
        "pub type MyResult = Result(Int, String)",
        "pub type MyResult = Result(String, Int)"
    );
}

#[test]
fn adding_type_alias_parameter_is_major() {
    assert_bump!(
        "pub type Maybe(a) = Result(a, Nil)",
        "pub type Maybe(a, b) = Result(a, b)"
    );
}

#[test]
fn changing_type_alias_parameter_order_is_major() {
    assert_bump!(
        "pub type OkOrError(a, b) = Result(a, b)",
        "pub type OkOrError(a, b) = Result(b, a)"
    );
}

#[test]
fn changing_type_alias_parameter_names_is_patch() {
    assert_bump!(
        "pub type OkOrError(a, b) = Result(a, b)",
        "pub type OkOrError(ok, error) = Result(ok, error)"
    );
}

#[test]
fn removing_public_type_alias_is_major() {
    assert_bump!("pub type MyAlias = Int", "");
}

#[test]
fn making_public_type_alias_private_is_major() {
    assert_bump!("pub type MyAlias = Int", "type MyAlias = Int");
}

#[test]
fn making_public_type_alias_internal_is_major() {
    assert_bump!("pub type MyAlias = Int", "@internal pub type MyAlias = Int");
}

#[test]
fn adding_public_type_alias_is_minor() {
    assert_bump!("", "pub type MyAlias = Int");
}

#[test]
fn making_private_type_alias_public_is_minor() {
    assert_bump!("type MyAlias = Int", "pub type MyAlias = Int");
}

#[test]
fn making_internal_type_alias_public_is_minor() {
    assert_bump!("@internal pub type MyAlias = Int", "pub type MyAlias = Int");
}

#[test]
fn removing_private_type_alias_is_patch() {
    assert_bump!("type MyAlias = Int", "");
}

#[test]
fn removing_internal_type_alias_is_patch() {
    assert_bump!("@internal pub type MyAlias = Int", "");
}

#[test]
fn changing_private_type_alias_is_patch() {
    assert_bump!("type MyAlias = Int", "type MyAlias = Float");
}

#[test]
fn changing_internal_type_alias_is_patch() {
    assert_bump!(
        "@internal pub type MyAlias = Int",
        "@internal pub type MyAlias = Float"
    );
}

#[test]
fn adding_custom_type_parameter_is_major() {
    assert_bump!(
        "pub type Wibble(a) { Wibble(a) }",
        "pub type Wibble(a, b) { Wibble(a) }"
    );
}

#[test]
fn changing_custom_type_parameter_order_is_major() {
    assert_bump!(
        "pub type Wibble(a, b) { Wibble(a, b) }",
        "pub type Wibble(a, b) { Wibble(b, a) }"
    );
}

#[test]
fn changing_custom_type_parameter_names_is_patch() {
    assert_bump!(
        "pub type Box(a) { Box(a) }",
        "pub type Box(inner) { Box(inner) }"
    );
}

#[test]
fn removing_public_custom_type_is_major() {
    assert_bump!("pub type Wibble { Wibble }", "");
}

#[test]
fn making_public_custom_type_private_is_major() {
    assert_bump!("pub type Wibble { Wibble }", "type Wibble { Wibble }");
}

#[test]
fn making_public_custom_type_internal_is_major() {
    assert_bump!(
        "pub type Wibble { Wibble }",
        "@internal pub type Wibble { Wibble }"
    );
}

#[test]
fn adding_public_custom_type_is_minor() {
    assert_bump!("", "pub type Wibble { Wibble }");
}

#[test]
fn making_private_custom_type_public_is_minor() {
    assert_bump!("type Wibble { Wibble }", "pub type Wibble { Wibble }");
}

#[test]
fn making_internal_custom_type_public_is_minor() {
    assert_bump!(
        "@internal pub type Wibble { Wibble }",
        "pub type Wibble { Wibble }"
    );
}

#[test]
fn removing_private_custom_type_is_patch() {
    assert_bump!("type Wibble { Wibble }", "");
}

#[test]
fn removing_internal_custom_type_is_patch() {
    assert_bump!("@internal pub type Wibble { Wibble }", "");
}

#[test]
fn changing_private_custom_type_is_patch() {
    assert_bump!("type Wibble { Wibble }", "type Wibble(a) { Wibble }");
}

#[test]
fn changing_internal_custom_type_is_patch() {
    assert_bump!(
        "@internal pub type Wibble { Wibble }",
        "@internal pub type Wibble(a) { Wibble }"
    );
}

#[test]
fn adding_constructor_to_public_custom_type_is_major() {
    assert_bump!(
        "
pub type Wibble {
  Wibble
}",
        "
pub type Wibble {
  Wibble
  Wobble
}"
    );
}

#[test]
fn removing_constructor_from_public_custom_type_is_major() {
    assert_bump!(
        "
pub type Wibble {
  Wibble
  Wobble
}",
        "
pub type Wibble {
  Wibble
}"
    );
}

#[test]
fn adding_constructor_to_internal_custom_type_is_patch() {
    assert_bump!(
        "@internal pub type Wibble {
  Wibble
}",
        "@internal pub type Wibble {
  Wibble
  Wobble
}"
    );
}

#[test]
fn removing_constructor_from_internal_custom_type_is_patch() {
    assert_bump!(
        "@internal pub type Wibble {
  Wibble
  Wobble
}",
        "@internal pub type Wibble {
  Wibble
}"
    );
}

#[test]
fn adding_constructor_to_private_custom_type_is_patch() {
    assert_bump!(
        "
type Wibble {
  Wibble
}",
        "
type Wibble {
  Wibble
  Wobble
}"
    );
}

#[test]
fn removing_constructor_from_private_custom_type_is_patch() {
    assert_bump!(
        "
type Wibble {
  Wibble
  Wobble
}",
        "
type Wibble {
  Wibble
}"
    );
}

#[test]
fn adding_constructor_to_opaque_custom_type_is_patch() {
    assert_bump!(
        "
pub opaque type Wibble {
  Wibble
}",
        "
pub opaque type Wibble {
  Wibble
  Wobble
}"
    );
}

#[test]
fn removing_constructor_from_opaque_custom_type_is_patch() {
    assert_bump!(
        "
pub opaque type Wibble {
  Wibble
  Wobble
}",
        "
pub opaque type Wibble {
  Wibble
}"
    );
}

#[test]
fn making_opaque_custom_type_public_is_minor() {
    assert_bump!(
        "
pub opaque type Wibble {
  Wibble
}",
        "
pub type Wibble {
  Wibble
}"
    );
}

#[test]
fn making_public_custom_type_opaque_is_major() {
    assert_bump!(
        "
pub type Wibble {
  Wibble
}",
        "
pub opaque type Wibble {
  Wibble
}"
    );
}

#[test]
fn changing_external_custom_type_to_opaque_is_patch() {
    assert_bump!(
        "pub type Wibble",
        "
pub opaque type Wibble {
  Wibble
}"
    );
}

#[test]
fn adding_constructors_to_external_custom_type_is_minor() {
    assert_bump!(
        "pub type Wibble",
        "
pub type Wibble {
  Wibble
}"
    );
}

#[test]
fn making_custom_type_with_constructors_external_is_major() {
    assert_bump!(
        "
pub type Wibble {
  Wibble
}",
        "pub type Wibble"
    );
}

#[test]
fn adding_label_to_constructor_field_is_minor() {
    assert_bump!(
        "
pub type Wibble {
  Wibble(Int, String)
}",
        "
pub type Wibble {
  Wibble(int: Int, string: String)
}"
    );
}

#[test]
fn changing_label_of_constructor_field_is_major() {
    assert_bump!(
        "
pub type Wibble {
  Wibble(int: Int, string: String)
}",
        "
pub type Wibble {
  Wibble(number: Int, text: String)
}"
    );
}

#[test]
fn removing_label_from_constructor_field_is_major() {
    assert_bump!(
        "
pub type Wibble {
  Wibble(int: Int, string: String)
}",
        "
pub type Wibble {
  Wibble(Int, String)
}"
    );
}

#[test]
fn removing_label_from_opaque_constructor_field_is_patch() {
    assert_bump!(
        "
pub opaque type Wibble {
  Wibble(int: Int, string: String)
}",
        "
pub opaque type Wibble {
  Wibble(Int, String)
}"
    );
}

#[test]
fn removing_label_from_opaque_constructor_field_when_turning_to_non_opaque_is_minor() {
    assert_bump!(
        "
pub opaque type Wibble {
  Wibble(int: Int, string: String)
}",
        "
pub type Wibble {
  Wibble(Int, String)
}"
    );
}

#[test]
fn adding_constructor_field_is_major() {
    assert_bump!(
        "
pub type Wibble {
  Wibble(Int, String)
}",
        "
pub type Wibble {
  Wibble(Int, String, Float)
}"
    );
}

#[test]
fn removing_constructor_field_is_major() {
    assert_bump!(
        "
pub type Wibble {
  Wibble(Int, String)
}",
        "
pub type Wibble {
  Wibble(Int)
}"
    );
}

#[test]
fn changing_type_of_constructor_field_is_major() {
    assert_bump!(
        "
pub type Wibble {
  Wibble(Int, String)
}",
        "
pub type Wibble {
  Wibble(Float, String)
}"
    );
}

#[test]
fn moving_custom_type_while_keeping_alias_is_minor() {
    assert_bump!(
        "
pub type OldType {
  Wibble
  Wobble
}",
        "
pub type NewType {
  Wibble
  Wobble
}

pub type OldType = NewType"
    );
}

#[test]
fn moving_and_changing_custom_type_while_keeping_alias_is_major() {
    assert_bump!(
        "
pub type OldType {
  Wibble
  Wobble
}",
        "
pub type NewType {
  Wibble
  Wobble
  Wubble
}

pub type OldType = NewType"
    );
}

#[test]
fn moving_custom_type_while_keeping_alias_with_different_parameters_is_major() {
    assert_bump!(
        "
pub type OldType(a, b) {
  Wibble(a)
  Wobble(b)
}",
        "
pub type NewType(a, b) {
  Wibble(a)
  Wobble(b)
}

pub type OldType(a, b) = NewType(b, a)"
    );
}

#[test]
fn moving_and_changing_custom_type_while_keeping_alias_with_same_parameters_is_minor() {
    assert_bump!(
        "
pub type OldType(a, b) {
  Wibble(a)
  Wobble(b)
}",
        "
pub type NewType(a, b, c) {
  Wibble(a)
  Wobble(b)
}

pub type OldType(a, b) = NewType(a, b, Nil)"
    );
}

#[test]
fn moving_and_changing_custom_type_while_keeping_alias_with_same_parameters_is_minor2() {
    assert_bump!(
        "
pub type OldType(a, b) {
  Wibble(a)
  Wobble(b)
  Wubble(Int)
}",
        "
pub type NewType(a, b, c) {
  Wibble(a)
  Wobble(b)
  Wubble(c)
}

pub type OldType(a, b) = NewType(a, b, Int)"
    );
}

#[test]
fn moving_and_changing_custom_type_while_keeping_alias_with_same_parameters_is_minor3() {
    assert_bump!(
        "
pub type OldType(wibble, wobble) {
  Wibble(wibble)
  Wobble(wobble)
}",
        "
pub type NewType(wobble, wibble) {
  Wibble(wibble)
  Wobble(wobble)
}

pub type OldType(wibble, wobble) = NewType(wobble, wibble)"
    );
}

#[test]
fn moving_custom_type_while_keeping_alias_with_function_using_type_is_minor() {
    assert_bump!(
        "
pub type OldType(a, b) {
  Wibble(a)
  Wobble(b)
}

pub fn make_wibble(value: a) -> OldType(a, b) { Wibble(value) }",
        "
pub type NewType(a, b) {
  Wibble(a)
  Wobble(b)
}

pub type OldType(a, b) = NewType(a, b)

pub fn make_wibble(value: a) -> OldType(a, b) { Wibble(value) }"
    );
}

#[test]
fn swapping_custom_type_with_alias_is_minor() {
    assert_bump!(
        "
pub type Custom {
  Wibble
  Wobble
}

pub type Alias = Custom",
        "
pub type Alias {
  Wibble
  Wobble
}

pub type Custom = Alias"
    );
}

#[test]
fn making_custom_type_internal_while_keeping_alias_is_major() {
    assert_bump!(
        "
pub type OldType {
  Wibble
  Wobble
}",
        "
@internal
pub type NewType {
  Wibble
  Wobble
}

pub type OldType = NewType"
    );
}

#[test]
fn changing_custom_type_to_type_alias_is_major() {
    assert_bump!(
        "
pub type MyBool {
  True
  False
}",
        "pub type MyBool = Bool"
    );
}

#[test]
fn changing_type_alias_to_custom_type_is_major() {
    assert_bump!(
        "pub type MyBool = Bool",
        "
pub type MyBool {
  True
  False
}"
    );
}

#[test]
fn changing_constructor_field_type_from_concrete_to_generic_is_major() {
    assert_bump!(
        "
pub type Wibble(a) {
  Wibble(Int)
}",
        "
pub type Wibble(a) {
  Wibble(a)
}"
    );
}

#[test]
fn changing_value_of_public_constant_with_same_type_is_patch() {
    assert_bump!("pub const max_size = 512", "pub const max_size = 1024");
}

#[test]
fn changing_type_of_public_constant_is_major() {
    assert_bump!("pub const max_size = 512", "pub const max_size = 512.2");
}

#[test]
fn changing_type_of_internal_constant_is_patch() {
    assert_bump!(
        "@internal pub const max_size = 512",
        "@internal pub const max_size = 512.2"
    );
}

#[test]
fn changing_type_of_private_constant_is_patch() {
    assert_bump!("const max_size = 512", "const max_size = 512.2");
}

#[test]
fn adding_public_constant_is_minor() {
    assert_bump!("", "pub const max_size = 512");
}

#[test]
fn removing_public_constant_is_major() {
    assert_bump!("pub const max_size = 512", "");
}

#[test]
fn adding_internal_constant_is_patch() {
    assert_bump!("", "@internal pub const max_size = 512");
}

#[test]
fn removing_internal_constant_is_patch() {
    assert_bump!("@internal pub const max_size = 512", "");
}

#[test]
fn adding_private_constant_is_patch() {
    assert_bump!("", "const max_size = 512");
}

#[test]
fn removing_private_constant_is_patch() {
    assert_bump!("const max_size = 512", "");
}

#[test]
fn making_private_constant_public_is_minor() {
    assert_bump!("const max_size = 512", "pub const max_size = 512");
}

#[test]
fn making_public_constant_private_is_major() {
    assert_bump!("pub const max_size = 512", "const max_size = 512");
}

#[test]
fn making_internal_constant_public_is_minor() {
    assert_bump!(
        "@internal pub const max_size = 512",
        "pub const max_size = 512"
    );
}

#[test]
fn making_public_constant_internal_is_major() {
    assert_bump!(
        "pub const max_size = 512",
        "@internal pub const max_size = 512"
    );
}

#[test]
fn adding_annotation_to_constant_without_changing_type_is_patch() {
    assert_bump!("pub const list = [1]", "pub const list: List(Int) = [1]");
}

#[test]
fn adding_annotation_to_constant_while_changing_type_is_major() {
    assert_bump!("pub const list = []", "pub const list: List(Int) = []");
}

#[test]
fn changing_constant_annotation_and_type_is_major() {
    assert_bump!(
        "pub const list: List(Int) = []",
        "pub const list: List(Float) = []"
    );
}

#[test]
fn removing_constant_annotation_without_changing_type_is_patch() {
    assert_bump!("pub const list: List(Int) = [1]", "pub const list = [1]");
}

#[test]
fn changing_concrete_constant_type_to_generic_is_minor() {
    assert_bump!("pub const list = [1]", "pub const list = []");
}

#[test]
fn removing_constant_annotation_to_change_concrete_type_to_generic_is_minor() {
    assert_bump!("pub const list: List(Int) = []", "pub const list = []");
}

#[test]
fn changing_different_concrete_constant_types_to_the_same_generic_is_major() {
    assert_bump!(
        "pub const lists: #(List(Int), List(Float)) = #([], [])",
        "pub const lists: #(List(a), List(a)) = #([], [])"
    );
}

#[test]
fn changing_multiple_concrete_constant_types_to_separate_generics_is_minor() {
    assert_bump!(
        "pub const lists: #(List(Int), List(Float)) = #([], [])",
        "pub const lists: #(List(a), List(b)) = #([], [])"
    );
}

#[test]
fn changing_same_concrete_constant_type_to_the_same_generic_is_minor() {
    assert_bump!(
        "pub const lists: #(List(Int), List(Int)) = #([], [])",
        "pub const lists: #(List(a), List(a)) = #([], [])"
    );
}

#[test]
fn changing_generic_constant_type_to_concrete_is_major() {
    assert_bump!(
        "pub const list: List(a) = []",
        "pub const list: List(Int) = []"
    );
}

#[test]
fn renaming_constant_while_keeping_old_alias_is_minor() {
    assert_bump!(
        "pub const old_name = 10",
        "
pub const new_name = 10
pub const old_name = new_name"
    );
}

#[test]
fn changing_constant_to_function_of_same_type_is_patch() {
    assert_bump!(
        "
fn add_private(a, b) { a + b }

pub const add = add_private",
        "pub fn add(a, b) { a + b }"
    );
}

#[test]
fn changing_constant_to_function_of_same_type_while_adding_labels_is_minor() {
    assert_bump!(
        "
fn add_private(a, b) { a + b }

pub const add = add_private",
        "pub fn add(first a, second b) { a + b }"
    );
}

#[test]
fn adding_new_external_to_constant_is_minor() {
    assert_bump!(
        r#"
pub const wobble = wibble

@external(erlang, "wibble_ffi", "something")
fn wibble() -> Nil"#,
        r#"
pub const wobble = wibble

@external(erlang, "wibble_ffi", "something")
@external(javascript, "./wibble_ffi.mjs", "something")
pub fn wibble() -> Nil"#
    );
}

#[test]
fn adding_pure_gleam_implementation_to_external_constant_is_minor() {
    assert_bump!(
        r#"
pub const wobble = wibble

@external(erlang, "wibble_ffi", "something")
fn wibble() -> Nil"#,
        r#"
pub const wobble = wibble

@external(erlang, "wibble_ffi", "something")
fn wibble() -> Nil {
  Nil
}"#
    );
}

#[test]
fn removing_target_support_from_constant_is_major() {
    assert_bump!(
        r#"
pub const wobble = wibble

@external(erlang, "wibble_ffi", "something")
@external(javascript, "./wibble_ffi.mjs", "something")
fn wibble() -> Nil"#,
        r#"
pub const wobble = wibble

@external(erlang, "wibble_ffi", "something")
fn wibble() -> Nil"#
    );
}

#[test]
fn removing_pure_gleam_implementation_from_external_constant_is_major() {
    assert_bump!(
        r#"
pub const wobble = wibble

@external(erlang, "wibble_ffi", "something")
fn wibble() -> Nil {
  Nil
}"#,
        r#"
pub const wobble = wibble

@external(erlang, "wibble_ffi", "something")
fn wibble() -> Nil"#
    );
}

#[test]
fn adding_pure_gleam_implementation_to_external_constant_which_supports_both_targets_is_patch() {
    assert_bump!(
        r#"
pub const wobble = wibble

@external(erlang, "wibble_ffi", "something")
@external(javascript, "./wibble_ffi.mjs", "something")
fn wibble() -> Nil"#,
        r#"
pub const wobble = wibble

@external(erlang, "wibble_ffi", "something")
@external(javascript, "./wibble_ffi.mjs", "something")
fn wibble() -> Nil {
  Nil
}"#
    );
}

#[test]
fn removing_pure_gleam_implementation_from_external_constant_which_supports_both_targets_is_patch()
{
    assert_bump!(
        r#"
pub const wobble = wibble

@external(erlang, "wibble_ffi", "something")
@external(javascript, "./wibble_ffi.mjs", "something")
fn wibble() -> Nil {
  Nil
}"#,
        r#"
pub const wobble = wibble

@external(erlang, "wibble_ffi", "something")
@external(javascript, "./wibble_ffi.mjs", "something")
fn wibble() -> Nil"#
    );
}

#[test]
fn changing_implementation_of_public_function_with_same_type_is_patch() {
    assert_bump!(
        "pub fn combine(a, b) { a + b }",
        "pub fn combine(a, b) { a * b }"
    );
}

#[test]
fn changing_type_of_public_function_is_major() {
    assert_bump!("pub fn add(a, b) { a + b }", "pub fn add(a, b) { a +. b }");
}

#[test]
fn changing_type_of_internal_function_is_patch() {
    assert_bump!(
        "@internal pub fn add(a, b) { a + b }",
        "@internal pub fn add(a, b) { a +. b }"
    );
}

#[test]
fn changing_type_of_private_function_is_patch() {
    assert_bump!("fn add(a, b) { a + b }", "fn add(a, b) { a +. b }");
}

#[test]
fn adding_public_function_is_minor() {
    assert_bump!("", "pub fn add(a, b) { a + b }");
}

#[test]
fn removing_public_function_is_major() {
    assert_bump!("pub fn add(a, b) { a + b }", "");
}

#[test]
fn adding_internal_function_is_patch() {
    assert_bump!("", "@internal pub fn add(a, b) { a + b }");
}

#[test]
fn removing_internal_function_is_patch() {
    assert_bump!("@internal pub fn add(a, b) { a + b }", "");
}

#[test]
fn adding_private_function_is_patch() {
    assert_bump!("", "fn add(a, b) { a + b }");
}

#[test]
fn removing_private_function_is_patch() {
    assert_bump!("fn add(a, b) { a + b }", "");
}

#[test]
fn making_private_function_public_is_minor() {
    assert_bump!("fn add(a, b) { a + b }", "pub fn add(a, b) { a + b }");
}

#[test]
fn making_public_function_private_is_major() {
    assert_bump!("pub fn add(a, b) { a + b }", "fn add(a, b) { a + b }");
}

#[test]
fn making_internal_function_public_is_minor() {
    assert_bump!(
        "@internal pub fn add(a, b) { a + b }",
        "pub fn add(a, b) { a + b }"
    );
}

#[test]
fn making_public_function_internal_is_major() {
    assert_bump!(
        "pub fn add(a, b) { a + b }",
        "@internal pub fn add(a, b) { a + b }"
    );
}

#[test]
fn adding_label_to_function_parameter_is_minor() {
    assert_bump!(
        "pub fn add(a, b) { a + b }",
        "pub fn add(first a, second b) { a + b }"
    );
}

#[test]
fn removing_label_from_function_parameter_is_major() {
    assert_bump!(
        "pub fn add(first a, second b) { a + b }",
        "pub fn add(a, b) { a + b }"
    );
}

#[test]
fn changing_label_of_function_parameter_is_major() {
    assert_bump!(
        "pub fn add(first a, second b) { a + b }",
        "pub fn add(this a, to b) { a + b }"
    );
}

#[test]
fn adding_annotation_to_function_without_changing_type_is_patch() {
    assert_bump!(
        "pub fn add(a, b) { a + b }",
        "pub fn add(a: Int, b: Int) -> Int { a + b }"
    );
}

#[test]
fn adding_annotation_to_function_while_changing_type_is_major() {
    assert_bump!(
        "pub fn pair(a, b) { #(a, b) }",
        "pub fn pair(a: Int, b: Int) -> #(Int, Int) { #(a, b) }"
    );
}

#[test]
fn changing_function_annotation_and_type_is_major() {
    assert_bump!(
        "pub fn pair(a: Int, b: Int) -> #(Int, Int) { #(a, b) }",
        "pub fn pair(a: Float, b: Float) -> #(Float, Float) { #(a, b) }"
    );
}

#[test]
fn removing_function_annotation_without_changing_type_is_patch() {
    assert_bump!(
        "pub fn add(a: Int, b: Int) -> Int { a + b }",
        "pub fn add(a, b) { a + b }"
    );
}

#[test]
fn changing_concrete_function_type_to_generic_is_minor() {
    assert_bump!(
        "pub fn identity(x: Int) { x }",
        "pub fn identity(x: a) { x }"
    );
}

#[test]
fn removing_function_annotation_to_change_concrete_type_to_generic_is_minor() {
    assert_bump!("pub fn identity(x: Int) { x }", "pub fn identity(x) { x }");
}

#[test]
fn changing_different_concrete_function_types_to_the_same_generic_is_major() {
    assert_bump!(
        "pub fn pair(a: Int, b: Float) { #(a, b) }",
        "pub fn pair(a: a, b: a) { #(a, b) }"
    );
}

#[test]
fn changing_multiple_concrete_function_types_to_separate_generics_is_minor() {
    assert_bump!(
        "pub fn pair(a: Int, b: Float) { #(a, b) }",
        "pub fn pair(a: a, b: b) { #(a, b) }"
    );
}

#[test]
fn changing_same_concrete_function_type_to_the_same_generic_is_minor() {
    assert_bump!(
        "pub fn pair(a: Int, b: Int) { #(a, b) }",
        "pub fn pair(a: a, b: a) { #(a, b) }"
    );
}

#[test]
fn changing_generic_function_type_to_concrete_is_major() {
    assert_bump!(
        "pub fn identity(x: a) { x }",
        "pub fn identity(x: Int) { x }"
    );
}

#[test]
fn changing_function_without_labels_to_constant_is_patch() {
    assert_bump!(
        "pub fn add(a, b) { a + b }",
        "
pub const add = do_add
fn do_add(a, b) { a + b }"
    );
}

#[test]
fn changing_function_with_labels_to_constant_is_major() {
    assert_bump!(
        "pub fn add(a, addend b) { a + b }",
        "
pub const add = do_add

fn do_add(a, addend b) { a + b }"
    );
}

#[test]
fn adding_new_external_to_function_is_minor() {
    assert_bump!(
        r#"
@external(erlang, "wibble_ffi", "something")
pub fn wibble() -> Nil"#,
        r#"
@external(erlang, "wibble_ffi", "something")
@external(javascript, "./wibble_ffi.mjs", "something")
pub fn wibble() -> Nil"#
    );
}

#[test]
fn adding_pure_gleam_implementation_to_external_function_is_minor() {
    assert_bump!(
        r#"
@external(erlang, "wibble_ffi", "something")
pub fn wibble() -> Nil"#,
        r#"
@external(erlang, "wibble_ffi", "something")
pub fn wibble() -> Nil {
  Nil
}"#
    );
}

#[test]
fn removing_target_support_from_function_is_major() {
    assert_bump!(
        r#"
@external(erlang, "wibble_ffi", "something")
@external(javascript, "./wibble_ffi.mjs", "something")
pub fn wibble() -> Nil"#,
        r#"
@external(erlang, "wibble_ffi", "something")
pub fn wibble() -> Nil"#
    );
}

#[test]
fn removing_pure_gleam_implementation_from_external_function_is_major() {
    assert_bump!(
        r#"
@external(erlang, "wibble_ffi", "something")
pub fn wibble() -> Nil {
  Nil
}"#,
        r#"
@external(erlang, "wibble_ffi", "something")
pub fn wibble() -> Nil"#
    );
}

#[test]
fn adding_pure_gleam_implementation_to_external_function_which_supports_both_targets_is_patch() {
    assert_bump!(
        r#"
@external(erlang, "wibble_ffi", "something")
@external(javascript, "./wibble_ffi.mjs", "something")
pub fn wibble() -> Nil"#,
        r#"
@external(erlang, "wibble_ffi", "something")
@external(javascript, "./wibble_ffi.mjs", "something")
pub fn wibble() -> Nil {
  Nil
}"#
    );
}

#[test]
fn removing_pure_gleam_implementation_from_external_function_which_supports_both_targets_is_patch()
{
    assert_bump!(
        r#"
@external(erlang, "wibble_ffi", "something")
@external(javascript, "./wibble_ffi.mjs", "something")
pub fn wibble() -> Nil {
  Nil
}"#,
        r#"
@external(erlang, "wibble_ffi", "something")
@external(javascript, "./wibble_ffi.mjs", "something")
pub fn wibble() -> Nil"#
    );
}

#[test]
fn changing_concrete_to_generic_type_with_other_unchanging_generic_is_minor() {
    assert_bump!(
        "
pub fn wibble(a: Int, b: a) -> #(Int, a) {
  #(a, b)
}",
        "
pub fn wibble(a: a, b: b) -> #(a, b) {
  #(a, b)
}"
    );
}

#[test]
fn changing_concrete_to_existing_generic_type_is_major() {
    assert_bump!(
        "
pub fn wibble(a: Int, b: a) -> #(Int, a) {
  #(a, b)
}",
        "
pub fn wibble(a: a, b: a) -> #(a, a) {
  #(a, b)
}"
    );
}

#[test]
fn generic_function_staying_the_same_is_patch() {
    assert_bump!(
        "pub fn wibble(a, b) -> c { todo }",
        "pub fn wibble(a, b) -> c { todo }"
    );
}

#[test]
fn tuple_length_change_is_major() {
    assert_bump!(
        "pub const tuple = #(1, 2.0)",
        "pub const tuple = #(1, 2.0, True)"
    );
}

#[test]
fn tuple_length_change_is_major2() {
    assert_bump!(
        "pub const tuple = #(1, 2.0, True)",
        "pub const tuple = #(1, 2.0)"
    );
}

#[test]
fn function_parameter_length_change_is_major() {
    assert_bump!(
        "pub fn x() { fn(a, b) { a + b } }",
        "pub fn x() { fn(a, b, c) { a + b + c } }"
    );
}

#[test]
fn function_parameter_length_change_is_major2() {
    assert_bump!(
        "pub fn x() { fn(a, b, c) { a + b + c } }",
        "pub fn x() { fn(a, b) { a + b } }"
    );
}

#[test]
fn adding_public_module_is_minor() {
    assert_bump!(
        ("wibble", "pub type Wibble");
        ("wibble", "pub type Wibble"), ("wobble", "pub type Wobble")
    );
}

#[test]
fn removing_public_module_is_major() {
    assert_bump!(
        ("wibble", "pub type Wibble"), ("wobble", "pub type Wobble");
        ("wibble", "pub type Wibble")
    );
}

#[test]
fn adding_internal_module_is_patch() {
    assert_bump!(
        ("wibble", "pub type Wibble");
        ("wibble", "pub type Wibble"), ("thepackage/internal", "pub type Wobble")
    );
}

#[test]
fn removing_internal_module_is_patch() {
    assert_bump!(
        ("wibble", "pub type Wibble"), ("thepackage/internal", "pub type Wobble");
        ("wibble", "pub type Wibble")
    );
}

#[test]
fn moving_type_to_internal_is_major() {
    assert_bump!(
        "pub type Wibble { Wibble Wobble }",
        "
@internal
pub type Wobble { Wibble Wobble }

pub type Wibble = Wobble
"
    );
}

#[test]
fn moving_opaque_type_to_internal_is_minor() {
    assert_bump!(
        "pub opaque type Wibble { Wibble Wobble }",
        "
@internal
pub type Wobble { Wibble Wobble }

pub type Wibble = Wobble
"
    );
}

#[test]
fn moving_type_with_constructors_across_modules_is_major() {
    assert_bump!(
        ("wibble", "pub type Wibble { Wibble Wobble }");
        ("wobble", "pub type Wibble { Wibble Wobble }"),
        (
            "wibble",
            "
import wobble
pub type Wibble = wobble.Wibble"
        )
    );
}

#[test]
fn moving_type_without_constructors_across_modules_is_minor() {
    assert_bump!(
        ("wibble", "pub type Wibble");
        ("wobble", "pub type Wibble"),
        (
            "wibble",
            "
import wobble
pub type Wibble = wobble.Wibble"
        )
    );
}

#[test]
fn moving_opaque_type_across_modules_is_minor() {
    assert_bump!(
        ("wibble", "pub opaque type Wibble { Wibble Wobble }");
        ("wobble", "pub type Wibble { Wibble Wobble }"),
        (
            "wibble",
            "
import wobble
pub type Wibble = wobble.Wibble"
        )
    );
}

#[test]
fn moving_type_without_constructors_to_internal_module_is_minor() {
    assert_bump!(
        ("wibble", "pub type Wibble");
        ("thepackage/internal", "pub type Wibble"),
        (
            "wibble",
            "
import thepackage/internal
pub type Wibble = internal.Wibble"
        )
    );
}

#[test]
fn moving_opaque_type_to_internal_module_is_minor() {
    assert_bump!(
        ("wibble", "pub opaque type Wibble { Wibble Wobble }");
        ("thepackage/internal", "pub type Wibble { Wibble Wobble }"),
        (
            "wibble",
            "
import thepackage/internal
pub type Wibble = internal.Wibble"
        )
    );
}

#[test]
fn deprecating_public_function_is_minor() {
    assert_bump!(
        "pub fn add(a, b) { a + b }",
        r#"
@deprecated("Use `int.add`")
pub fn add(a, b) { a + b }"#
    );
}

#[test]
fn removing_deprecation_from_public_function_is_minor() {
    assert_bump!(
        r#"
@deprecated("Use `int.add`")
pub fn add(a, b) { a + b }"#,
        "pub fn add(a, b) { a + b }"
    );
}
