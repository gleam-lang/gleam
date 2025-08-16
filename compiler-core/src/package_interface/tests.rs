use std::time::SystemTime;

use camino::Utf8PathBuf;
use ecow::EcoString;
use globset::GlobBuilder;
use hexpm::version::Identifier;

use crate::{
    analyse::TargetSupport,
    build::{Module, Origin, Package, Target},
    config::{Docs, ErlangConfig, GleamVersion, JavaScriptConfig, PackageConfig},
    line_numbers::LineNumbers,
    type_::PRELUDE_MODULE_NAME,
    uid::UniqueIdGenerator,
    warning::{TypeWarningEmitter, WarningEmitter},
};

use super::PackageInterface;

#[macro_export]
macro_rules! assert_package_interface_with_name {
    ($module_name:expr, $src:expr) => {
        let output =
            $crate::package_interface::tests::compile_package(Some($module_name), $src, None);
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    };
}

#[macro_export]
macro_rules! assert_package_interface {
    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr $(,)?) => {{
        let output = $crate::package_interface::tests::compile_package(
            None,
            $src,
            Some(($dep_package, $dep_name, $dep_src)),
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr $(,)?) => {{
        let output = $crate::package_interface::tests::compile_package(
            None,
            $src,
            Some(($dep_package, $dep_name, $dep_src)),
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    ($src:expr) => {{
        let output = $crate::package_interface::tests::compile_package(None, $src, None);
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};
}

pub fn compile_package(
    module_name: Option<&str>,
    src: &str,
    dep: Option<(&str, &str, &str)>,
) -> String {
    let mut modules = im::HashMap::new();
    let ids = UniqueIdGenerator::new();
    // DUPE: preludeinsertion
    // TODO: Currently we do this here and also in the tests. It would be better
    // to have one place where we create all this required state for use in each
    // place.
    let _ = modules.insert(
        PRELUDE_MODULE_NAME.into(),
        crate::type_::build_prelude(&ids),
    );
    let mut direct_dependencies = std::collections::HashMap::from_iter(vec![]);
    if let Some((dep_package, dep_name, dep_src)) = dep {
        let parsed = crate::parse::parse_module(
            Utf8PathBuf::from("test/path"),
            dep_src,
            &WarningEmitter::null(),
        )
        .expect("dep syntax error");
        let mut ast = parsed.module;
        ast.name = dep_name.into();
        let line_numbers = LineNumbers::new(dep_src);
        let mut config = PackageConfig::default();
        config.name = dep_package.into();

        let dep = crate::analyse::ModuleAnalyzerConstructor::<()> {
            target: Target::Erlang,
            ids: &ids,
            origin: Origin::Src,
            importable_modules: &modules,
            warnings: &TypeWarningEmitter::null(),
            direct_dependencies: &std::collections::HashMap::new(),
            dev_dependencies: &std::collections::HashSet::new(),
            target_support: TargetSupport::Enforced,
            package_config: &config,
        }
        .infer_module(ast, line_numbers, "".into())
        .expect("should successfully infer");
        let _ = modules.insert(dep_name.into(), dep.type_info);
        let _ = direct_dependencies.insert(dep_package.into(), ());
    }
    let parsed =
        crate::parse::parse_module(Utf8PathBuf::from("test/path"), src, &WarningEmitter::null())
            .expect("syntax error");

    let mut ast = parsed.module;
    let module_name = module_name
        .map(EcoString::from)
        .unwrap_or("my/module".into());

    ast.name = module_name.clone();
    let mut config = PackageConfig::default();
    config.name = "my_package".into();
    let ast = crate::analyse::ModuleAnalyzerConstructor {
        target: Target::Erlang,
        ids: &ids,
        origin: Origin::Src,
        importable_modules: &modules,
        warnings: &TypeWarningEmitter::null(),
        direct_dependencies: &direct_dependencies,
        dev_dependencies: &std::collections::HashSet::new(),
        target_support: TargetSupport::Enforced,
        package_config: &config,
    }
    .infer_module(ast, LineNumbers::new(src), "".into())
    .expect("should successfully infer");

    // TODO: all the bits above are basically copy pasted from the javascript
    // and erlang test helpers. A refactor might be due here.
    let mut module = Module {
        name: module_name,
        code: src.into(),
        mtime: SystemTime::UNIX_EPOCH,
        input_path: "wibble".into(),
        origin: Origin::Src,
        ast,
        extra: parsed.extra,
        dependencies: vec![],
    };
    module.attach_doc_and_module_comments();
    let package: Package = package_from_module(module);
    serde_json::to_string_pretty(&PackageInterface::from_package(
        &package,
        &Default::default(),
    ))
    .expect("to json")
}

fn package_from_module(module: Module) -> Package {
    Package {
        config: PackageConfig {
            name: "my_package".into(),
            version: hexpm::version::Version {
                major: 11,
                minor: 10,
                patch: 9,
                pre: vec![
                    Identifier::Numeric(1),
                    Identifier::AlphaNumeric("wibble".into()),
                ],
                build: Some("build".into()),
            },
            gleam_version: Some(GleamVersion::new("1.0.0".to_string()).unwrap()),
            licences: vec![],
            description: "description".into(),
            documentation: Docs { pages: vec![] },
            dependencies: std::collections::HashMap::new(),
            dev_dependencies: std::collections::HashMap::new(),
            repository: None,
            links: vec![],
            erlang: ErlangConfig::default(),
            javascript: JavaScriptConfig::default(),
            target: Target::Erlang,
            internal_modules: Some(vec![
                GlobBuilder::new("internals/*")
                    .build()
                    .expect("internals glob"),
            ]),
        },
        cached_module_names: Vec::new(),
        modules: vec![module],
    }
}

#[test]
pub fn package_documentation_is_included() {
    assert_package_interface!(
        "
//// Some package
//// documentation!

pub fn main() { 1 }
"
    );
}

#[test]
pub fn private_definitions_are_not_included() {
    assert_package_interface!(
        "
const float = 1.1
fn main() {}
type Wibble
type Wob = Int
"
    );
}

#[test]
pub fn internal_definitions_are_not_included() {
    assert_package_interface!(
        "
@internal pub const float = 1.1
@internal pub fn main() {}
@internal pub type Wibble
@internal pub type Wobble = Int
"
    );
}

#[test]
pub fn opaque_constructors_are_not_exposed() {
    assert_package_interface!("pub opaque type Wibble { Wob }")
}

#[test]
pub fn type_aliases() {
    assert_package_interface!("pub type Wibble(a) = List(a)")
}

#[test]
pub fn type_definition() {
    assert_package_interface!(
        "
/// Wibble's documentation
pub type Wibble(a, b) {
  Wibble
  Wobble
}
"
    )
}

#[test]
pub fn prelude_types() {
    assert_package_interface!(
        r#"
pub const float = 1.1
pub const string = ""
pub const int = 1
pub const bool = True
"#
    );
}

#[test]
pub fn generic_function() {
    assert_package_interface!(
        r#"
pub type Wob(a) { Wob }
@deprecated("deprecation message")
pub fn main() { Wob }
"#
    );
}

#[test]
pub fn imported_type() {
    assert_package_interface!(
        ("other_package", "other_module", "pub type Element(a)"),
        r#"
import other_module.{type Element}
pub fn main() -> Element(Int) {}
"#
    );
}

#[test]
pub fn imported_aliased_type_keeps_original_name() {
    assert_package_interface!(
        ("other_package", "other_module", "pub type Element(a)"),
        r#"
import other_module.{type Element as Alias} as module_alias
pub fn main() -> Alias(module_alias.Element(a)) {}
"#
    );
}

#[test]
pub fn multiple_type_variables() {
    assert_package_interface!(
        r#"
pub type Box(a, b)
pub fn some_type_variables(a: a, b: b, c: Box(c, d)) -> Box(a, d) {}
"#
    );
}

#[test]
pub fn type_constructors() {
    assert_package_interface!(
        r#"
pub type Box(a, b) {
  Box(b, Int)
  OtherBox(message: String, a: a)
}
"#
    );
}

#[test]
pub fn internal_modules_are_not_exported() {
    assert_package_interface_with_name!("internals/internal_module", "pub fn main() { 1 }");
}

#[test]
pub fn labelled_function_parameters() {
    assert_package_interface!(
        r#"
pub fn fold(list: List(a), from acc: b, with f: fn(a, b) -> b) -> b {
  todo
}
"#
    );
}

#[test]
pub fn constructors_with_documentation() {
    assert_package_interface!(
        r#"
pub type Wibble {
  /// This is the Wibble variant. It contains some example data.
  Wibble(Int)
  /// This is the Wobble variant. It is a recursive type.
  Wobble(Wibble)
}
"#
    );
}
