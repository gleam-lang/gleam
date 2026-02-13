use std::collections::HashSet;

use camino::Utf8Path;
use ecow::EcoString;

use crate::{
    Error,
    build::{
        self, NullTelemetry, Outcome, PackageCompiler, StaleTracker, Target,
        TargetCodegenConfiguration, Telemetry, package_compiler::Compiled,
    },
    config::{PackageConfig, PackageKind},
    error::DefinedModuleOrigin,
    io::{FileSystemWriter, memory::InMemoryFileSystem},
    uid::UniqueIdGenerator,
    warning::WarningEmitter,
};

fn compile_modules(
    package_kind: PackageKind,
    module: &str,
    dependency_modules: Vec<(&str, &str)>,
) -> Outcome<Compiled, Error> {
    let mut fs = InMemoryFileSystem::new();
    fs.write(
        Utf8Path::new(&format!("/src/{module}.gleam")),
        "pub fn main() -> Nil { Nil }",
    )
    .expect("write module");

    let config = PackageConfig::default();
    let compiler = PackageCompiler::new(
        package_kind,
        &config,
        build::Mode::Dev,
        Utf8Path::new("/"),
        Utf8Path::new("/out"),
        Utf8Path::new("/lib"),
        &TargetCodegenConfiguration::Erlang { app_file: None },
        UniqueIdGenerator::new(),
        fs,
    );

    let mut already_defined_modules = dependency_modules
        .into_iter()
        .map(|(package_name, module_name)| {
            (
                EcoString::from(module_name),
                DefinedModuleOrigin::Dependency {
                    package_name: EcoString::from(package_name),
                },
            )
        })
        .collect();

    compiler.compile(
        &WarningEmitter::null(),
        &mut im::HashMap::new(),
        &mut already_defined_modules,
        &mut StaleTracker::default(),
        &mut HashSet::new(),
        &NullTelemetry,
    )
}

#[test]
pub fn dependency_defining_same_module_as_root_package() {
    let output = compile_modules(PackageKind::Root, "a_module", vec![("dep1", "a_module")])
        .into_result()
        .expect_err("should produce an error")
        .pretty_string();

    insta::assert_snapshot!(insta::internals::AutoName, output);
}

#[test]
pub fn dependency_defining_same_module_as_another_dependency() {
    let output = compile_modules(
        PackageKind::Dependency {
            package_name: EcoString::from("a_dependency"),
        },
        "a_module",
        vec![("another_dependency", "a_module")],
    )
    .into_result()
    .expect_err("should produce an error")
    .pretty_string();

    insta::assert_snapshot!(insta::internals::AutoName, output);
}
