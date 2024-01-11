use std::time::SystemTime;

use crate::{
    analyse::TargetSupport,
    build::{Module, Origin, Package, Target},
    config::{Docs, ErlangConfig, JavaScriptConfig, PackageConfig, Repository},
    parse::extra::ModuleExtra,
    type_::PRELUDE_MODULE_NAME,
    uid::UniqueIdGenerator,
    warning::TypeWarningEmitter,
};

use super::PackageInterface;

#[macro_export]
macro_rules! assert_package_interface {
    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr $(,)?) => {{
        let output = $crate::package_interface::tests::compile_package(
            $src,
            Some(($dep_package, $dep_name, $dep_src)),
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    ($src:expr) => {{
        let output = $crate::package_interface::tests::compile_package($src, None);
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};
}

pub fn compile_package(src: &str, dep: Option<(&str, &str, &str)>) -> String {
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
        let parsed = crate::parse::parse_module(dep_src).expect("dep syntax error");
        let mut ast = parsed.module;
        ast.name = dep_name.into();
        let dep = crate::analyse::infer_module::<()>(
            Target::Erlang,
            &ids,
            ast,
            Origin::Src,
            &dep_package.into(),
            &modules,
            &TypeWarningEmitter::null(),
            &std::collections::HashMap::new(),
            TargetSupport::Enforced,
        )
        .expect("should successfully infer");
        let _ = modules.insert(dep_name.into(), dep.type_info);
        let _ = direct_dependencies.insert(dep_package.into(), ());
    }
    let parsed = crate::parse::parse_module(src).expect("syntax error");
    let mut ast = parsed.module;
    ast.name = "my/module".into();
    let ast = crate::analyse::infer_module::<()>(
        Target::Erlang,
        &ids,
        ast,
        Origin::Src,
        &"my_package".into(),
        &modules,
        &TypeWarningEmitter::null(),
        &direct_dependencies,
        TargetSupport::Enforced,
    )
    .expect("should successfully infer");

    // TODO: all the bits above are basically copy pasted from the javascript
    // and erlang test helpers. A refactor might be due here, but I'm not sure
    // where to put such function...

    let module = Module {
        name: "my/module".into(),
        code: src.into(),
        mtime: SystemTime::UNIX_EPOCH,
        input_path: "foo".into(),
        origin: Origin::Src,
        ast,
        extra: ModuleExtra::default(),
        dependencies: vec![],
    };
    let package: Package = package_from_module(module);
    serde_json::to_string_pretty(&PackageInterface::from_package(&package)).expect("to json")
}

fn package_from_module(module: Module) -> Package {
    Package {
        config: PackageConfig {
            name: "my_package".into(),
            version: hexpm::version::Version {
                major: 11,
                minor: 11,
                patch: 11,
                pre: vec![],
                build: None,
            },
            gleam_version: Some("1.0.0".into()),
            licences: vec![],
            description: "description".into(),
            documentation: Docs { pages: vec![] },
            dependencies: std::collections::HashMap::new(),
            dev_dependencies: std::collections::HashMap::new(),
            repository: Repository::default(),
            links: vec![],
            erlang: ErlangConfig::default(),
            javascript: JavaScriptConfig::default(),
            target: Target::Erlang,
            internal_modules: None,
        },
        modules: vec![module],
    }
}

#[test]
pub fn private_definitions_are_not_included() {
    assert_package_interface!(
        "
const float = 1.1
fn main() {}
type Foo
type alias Bar = Int
"
    );
}

#[test]
pub fn opaque_constructors_are_not_exposed() {
    assert_package_interface!("pub opaque type Foo { Bar }")
}

#[test]
pub fn type_aliases() {
    assert_package_interface!("pub type Foo(a) = Int")
}

#[test]
pub fn type_definition() {
    assert_package_interface!(
        "
/// Foo's documentation
pub type Foo(a, b) {
  Bar
  Baz
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
pub type Bar(a) { Bar }
@deprecated("deprecation message")
pub fn main() { Bar }
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
pub fn main(a: a, b: b, c: Box(c, d)) -> Box(a, d) {}
    "#
    );
}
