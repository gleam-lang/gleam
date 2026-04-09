use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use camino::Utf8PathBuf;
use ecow::EcoString;
use itertools::Itertools;

use crate::{
    analyse::TargetSupport,
    assert_module_error,
    build::{Origin, Target},
    config::PackageConfig,
    line_numbers::LineNumbers,
    type_::{build_prelude, expression::Implementations, prelude::PRELUDE_MODULE_NAME},
    uid::UniqueIdGenerator,
    warning::{TypeWarningEmitter, VectorWarningEmitterIO, WarningEmitter, WarningEmitterIO},
};

use super::compile_module_with_opts;

macro_rules! assert_targets {
    ($src:expr, $implementations:expr $(,)?) => {
        let result = $crate::type_::tests::target_implementations::implementations($src);
        let expected = $implementations
            .iter()
            .map(|(name, expected_impl)| ((*name).into(), *expected_impl))
            .collect_vec();
        assert_eq!(expected, result);
    };
}

pub fn implementations(src: &str) -> Vec<(EcoString, Implementations)> {
    compile_module_with_opts(
        "test_module",
        src,
        None,
        vec![],
        Target::Erlang,
        TargetSupport::NotEnforced,
        None,
    )
    .expect("compile src")
    .type_info
    .values
    .into_iter()
    .map(|(name, value)| (name, value.variant.implementations()))
    .sorted()
    .collect_vec()
}

fn dependency_implementations(
    module_name: &str,
    src: &str,
    dep: Vec<(&str, &str, &str)>,
) -> Vec<(EcoString, Implementations)> {
    let ids = UniqueIdGenerator::new();
    let warnings: Rc<dyn WarningEmitterIO> = Rc::new(VectorWarningEmitterIO::default());
    let emitter = WarningEmitter::new(warnings);
    let mut modules = im::HashMap::new();

    let _ = modules.insert(PRELUDE_MODULE_NAME.into(), build_prelude(&ids));
    let mut direct_dependencies = HashMap::new();

    for (package, name, module_src) in dep {
        let parsed =
            crate::parse::parse_module(Utf8PathBuf::from("test/path"), module_src, &emitter)
                .expect("syntax error");
        let mut ast = parsed.module;
        ast.name = name.into();
        let line_numbers = LineNumbers::new(module_src);
        let mut config = PackageConfig::default();
        config.name = package.into();

        let module = crate::analyse::ModuleAnalyzerConstructor::<()> {
            target: Target::Erlang,
            ids: &ids,
            origin: Origin::Src,
            importable_modules: &modules,
            warnings: &TypeWarningEmitter::null(),
            direct_dependencies: &HashMap::new(),
            dev_dependencies: &HashSet::new(),
            target_support: TargetSupport::NotEnforced,
            package_config: &config,
        }
        .infer_module(ast, line_numbers, "".into())
        .expect("should successfully infer");

        let _ = modules.insert(name.into(), module.type_info);

        if package != "non-dependency-package" {
            let _ = direct_dependencies.insert(package.into(), ());
        }
    }

    let parsed = crate::parse::parse_module(Utf8PathBuf::from("test/path"), src, &emitter)
        .expect("syntax error");
    let mut ast = parsed.module;
    ast.name = module_name.into();
    let mut config = PackageConfig::default();
    config.name = "thepackage".into();

    crate::analyse::ModuleAnalyzerConstructor::<()> {
        target: Target::Erlang,
        ids: &ids,
        origin: Origin::Src,
        importable_modules: &modules,
        warnings: &TypeWarningEmitter::null(),
        direct_dependencies: &direct_dependencies,
        dev_dependencies: &HashSet::new(),
        target_support: TargetSupport::NotEnforced,
        package_config: &config,
    }
    .infer_module(ast, LineNumbers::new(src), "".into())
    .expect("compile src")
    .type_info
    .values
    .into_iter()
    .map(|(name, value)| (name, value.variant.implementations()))
    .sorted()
    .collect_vec()
}

#[test]
pub fn pure_gleam_function() {
    assert_targets!(
        r#"
pub fn pure_gleam_1() { 1 + 1 }
pub fn pure_gleam_2() { pure_gleam_1() * 2 }
"#,
        [
            (
                "pure_gleam_1",
                Implementations {
                    gleam: true,
                    uses_erlang_externals: false,
                    uses_javascript_externals: false,
                    can_run_on_erlang: true,
                    can_run_on_javascript: true,
                }
            ),
            (
                "pure_gleam_2",
                Implementations {
                    gleam: true,
                    uses_erlang_externals: false,
                    uses_javascript_externals: false,
                    can_run_on_erlang: true,
                    can_run_on_javascript: true,
                }
            )
        ],
    );
}

#[test]
pub fn erlang_only_function() {
    assert_targets!(
        r#"
@external(erlang, "wibble", "wobble")
pub fn erlang_only_1() -> Int

pub fn erlang_only_2() { erlang_only_1() * 2 }
"#,
        [
            (
                "erlang_only_1",
                Implementations {
                    gleam: false,
                    uses_erlang_externals: true,
                    uses_javascript_externals: false,
                    can_run_on_erlang: true,
                    can_run_on_javascript: false,
                }
            ),
            (
                "erlang_only_2",
                Implementations {
                    gleam: false,
                    uses_erlang_externals: true,
                    uses_javascript_externals: false,
                    can_run_on_erlang: true,
                    can_run_on_javascript: false,
                }
            )
        ],
    );
}

#[test]
pub fn externals_only_function() {
    assert_targets!(
        r#"
@external(erlang, "wibble", "wobble")
@external(javascript, "wibble", "wobble")
pub fn all_externals_1() -> Int

pub fn all_externals_2() { all_externals_1() * 2 }
"#,
        [
            (
                "all_externals_1",
                Implementations {
                    gleam: false,
                    uses_erlang_externals: true,
                    uses_javascript_externals: true,
                    can_run_on_erlang: true,
                    can_run_on_javascript: true,
                }
            ),
            (
                "all_externals_2",
                Implementations {
                    gleam: false,
                    uses_erlang_externals: true,
                    uses_javascript_externals: true,
                    can_run_on_erlang: true,
                    can_run_on_javascript: true,
                }
            )
        ],
    );
}

#[test]
pub fn imported_module_select_does_not_create_false_cycle() {
    let result = dependency_implementations(
        "test_module",
        r#"
import option

@external(javascript, "../gleam_stdlib/gleam/function.mjs", "identity")
pub fn id(a: a) -> a

fn wibble() -> option.Option(Int) {
  option.Some(id(42))
}

pub fn option() -> option.Option(Int) {
  wibble()
}
"#,
        vec![(
            "thepackage",
            "option",
            "pub type Option(a) { Some(a) None }",
        )],
    );

    let expected = vec![
        (
            "id".into(),
            Implementations {
                gleam: false,
                uses_erlang_externals: false,
                uses_javascript_externals: true,
                can_run_on_erlang: false,
                can_run_on_javascript: true,
            },
        ),
        (
            "option".into(),
            Implementations {
                gleam: false,
                uses_erlang_externals: false,
                uses_javascript_externals: true,
                can_run_on_erlang: false,
                can_run_on_javascript: true,
            },
        ),
        (
            "wibble".into(),
            Implementations {
                gleam: false,
                uses_erlang_externals: false,
                uses_javascript_externals: true,
                can_run_on_erlang: false,
                can_run_on_javascript: true,
            },
        ),
    ];

    assert_eq!(expected, result);
}

#[test]
pub fn externals_with_pure_gleam_body() {
    assert_targets!(
        r#"
@external(javascript, "wibble", "wobble")
pub fn javascript_external_and_pure_body() -> Int { 1 + 1 }

@external(erlang, "wibble", "wobble")
pub fn erlang_external_and_pure_body() -> Int { 1 + 1 }

pub fn pure_gleam() {
  javascript_external_and_pure_body() + erlang_external_and_pure_body()
}
"#,
        [
            (
                "erlang_external_and_pure_body",
                Implementations {
                    gleam: true,
                    uses_erlang_externals: true,
                    uses_javascript_externals: false,
                    can_run_on_erlang: true,
                    can_run_on_javascript: true,
                }
            ),
            (
                "javascript_external_and_pure_body",
                Implementations {
                    gleam: true,
                    uses_erlang_externals: false,
                    uses_javascript_externals: true,
                    can_run_on_erlang: true,
                    can_run_on_javascript: true,
                }
            ),
            (
                "pure_gleam",
                Implementations {
                    gleam: true,
                    uses_erlang_externals: true,
                    uses_javascript_externals: true,
                    can_run_on_erlang: true,
                    can_run_on_javascript: true,
                }
            )
        ],
    );
}

#[test]
pub fn erlang_external_with_javascript_body() {
    assert_targets!(
        r#"
@external(javascript, "wibble", "wobble")
fn javascript_only() -> Int

@external(erlang, "wibble", "wobble")
pub fn erlang_external_and_javascript_body() -> Int { javascript_only() }

pub fn all_externals() -> Int { erlang_external_and_javascript_body() }
"#,
        [
            (
                "all_externals",
                Implementations {
                    gleam: false,
                    uses_erlang_externals: true,
                    uses_javascript_externals: true,
                    can_run_on_erlang: true,
                    can_run_on_javascript: true,
                }
            ),
            (
                "erlang_external_and_javascript_body",
                Implementations {
                    gleam: false,
                    uses_erlang_externals: true,
                    uses_javascript_externals: true,
                    can_run_on_erlang: true,
                    can_run_on_javascript: true,
                }
            ),
            (
                "javascript_only",
                Implementations {
                    gleam: false,
                    uses_erlang_externals: false,
                    uses_javascript_externals: true,
                    can_run_on_erlang: false,
                    can_run_on_javascript: true,
                }
            )
        ],
    );
}

#[test]
pub fn javascript_external_with_erlang_body() {
    assert_targets!(
        r#"
@external(erlang, "wibble", "wobble")
pub fn erlang_only() -> Int

@external(javascript, "wibble", "wobble")
pub fn javascript_external_and_erlang_body() -> Int { erlang_only() }

pub fn all_externals() -> Int { javascript_external_and_erlang_body() }
"#,
        [
            (
                "all_externals",
                Implementations {
                    gleam: false,
                    uses_erlang_externals: true,
                    uses_javascript_externals: true,
                    can_run_on_erlang: true,
                    can_run_on_javascript: true,
                }
            ),
            (
                "erlang_only",
                Implementations {
                    gleam: false,
                    uses_erlang_externals: true,
                    uses_javascript_externals: false,
                    can_run_on_erlang: true,
                    can_run_on_javascript: false,
                }
            ),
            (
                "javascript_external_and_erlang_body",
                Implementations {
                    gleam: false,
                    uses_erlang_externals: true,
                    uses_javascript_externals: true,
                    can_run_on_erlang: true,
                    can_run_on_javascript: true,
                }
            )
        ],
    );
}

#[test]
pub fn function_with_no_valid_implementations() {
    assert_module_error!(
        r#"
@external(javascript, "wibble", "wobble")
fn javascript_only() -> Int

@external(erlang, "wibble", "wobble")
fn erlang_only() -> Int

pub fn main() {
    javascript_only()
    erlang_only()
}
"#
    );
}

#[test]
pub fn invalid_both_and_one_called_from_erlang() {
    let src = r#"
@external(erlang, "wibble", "wobble")
@external(javascript, "wibble", "wobble")
fn both_external() -> Int

@external(javascript, "wibble", "wobble")
fn javascript_only() -> Int

pub fn no_valid_erlang_impl() {
  both_external()
  javascript_only()
}
"#;
    let out = compile_module_with_opts(
        "test_module",
        src,
        None,
        vec![],
        Target::Erlang,
        TargetSupport::Enforced,
        None,
    );
    assert!(out.into_result().is_err());
}

#[test]
pub fn invalid_both_and_one_called_from_javascript() {
    let src = r#"
@external(erlang, "wibble", "wobble")
@external(javascript, "wibble", "wobble")
fn both_external() -> Int

@external(erlang, "wibble", "wobble")
fn erlang_only() -> Int

pub fn no_valid_javascript_impl() {
  both_external()
  erlang_only()
}
"#;
    let out = compile_module_with_opts(
        "test_module",
        src,
        None,
        vec![],
        Target::JavaScript,
        TargetSupport::Enforced,
        None,
    );
    assert!(out.into_result().is_err());
}

#[test]
pub fn invalid_both_and_one_called_from_erlang_flipped() {
    let src = r#"
@external(erlang, "wibble", "wobble")
@external(javascript, "wibble", "wobble")
fn both_external() -> Int

@external(javascript, "wibble", "wobble")
fn javascript_only() -> Int

pub fn no_valid_erlang_impl() {
  javascript_only()
  both_external()
}
"#;
    let out = compile_module_with_opts(
        "test_module",
        src,
        None,
        vec![],
        Target::Erlang,
        TargetSupport::Enforced,
        None,
    );
    assert!(out.into_result().is_err());
}

#[test]
pub fn invalid_both_and_one_called_from_javascript_flipped() {
    let src = r#"
@external(erlang, "wibble", "wobble")
@external(javascript, "wibble", "wobble")
fn both_external() -> Int

@external(erlang, "wibble", "wobble")
fn erlang_only() -> Int

pub fn no_valid_javascript_impl() {
  erlang_only()
  both_external()
}
"#;
    let out = compile_module_with_opts(
        "test_module",
        src,
        None,
        vec![],
        Target::JavaScript,
        TargetSupport::Enforced,
        None,
    );
    assert!(out.into_result().is_err());
}

#[test]
pub fn invalid_erlang_with_external() {
    let src = r#"
@external(javascript, "wibble", "wobble")
fn javascript_only() -> Int

@external(javascript, "one", "two")
pub fn no_valid_erlang_impl() {
  javascript_only()
}
"#;
    let out = compile_module_with_opts(
        "test_module",
        src,
        None,
        vec![],
        Target::Erlang,
        TargetSupport::Enforced,
        None,
    );
    assert!(out.into_result().is_err());
}

#[test]
pub fn invalid_javascript_with_external() {
    let src = r#"
@external(erlang, "wibble", "wobble")
fn erlang_only() -> Int

@external(erlang, "one", "two")
pub fn no_valid_javascript_impl() {
  erlang_only()
}
"#;
    let out = compile_module_with_opts(
        "test_module",
        src,
        None,
        vec![],
        Target::JavaScript,
        TargetSupport::Enforced,
        None,
    );
    assert!(out.into_result().is_err());
}
