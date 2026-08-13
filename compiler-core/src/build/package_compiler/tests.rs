// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use std::collections::{HashMap, HashSet};

use camino::Utf8Path;
use ecow::EcoString;

use crate::{
    Error,
    build::{
        self, ErlangAppCodegenConfiguration, NullTelemetry, Outcome, PackageCompiler, StaleTracker,
        TargetCodegenConfiguration, package_compiler::Compiled,
    },
    config::PackageConfig,
    error::DefinedModuleOrigin,
    io::{FileSystemReader, FileSystemWriter, memory::InMemoryFileSystem},
    uid::UniqueIdGenerator,
    warning::WarningEmitter,
};

fn compile_modules(
    package_name: &str,
    module: &str,
    existing_modules: Vec<(&str, &str)>,
) -> Outcome<Compiled, Error> {
    let fs = InMemoryFileSystem::new();
    fs.write(
        Utf8Path::new(&format!("/src/{module}.gleam")),
        "pub fn main() -> Nil { Nil }",
    )
    .expect("write module");

    let mut config = PackageConfig::default();
    config.name = package_name.into();

    let compiler = PackageCompiler::new(
        &config,
        build::Mode::Dev,
        Utf8Path::new("/"),
        Utf8Path::new("/out"),
        Utf8Path::new("/lib"),
        &TargetCodegenConfiguration::Erlang { app_file: None },
        UniqueIdGenerator::new(),
        fs,
    );

    let mut already_defined_modules = existing_modules
        .into_iter()
        .map(|(package_name, module_name)| {
            (
                EcoString::from(module_name),
                DefinedModuleOrigin {
                    package_name: EcoString::from(package_name),
                    path: Utf8Path::new(&format!("{package_name}/{module_name}.gleam"))
                        .to_path_buf(),
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
pub fn different_packages_defining_duplicate_module() {
    let output = compile_modules("a_package", "a_module", vec![("dep1", "a_module")])
        .into_result()
        .expect_err("should produce an error")
        .pretty_string();

    insta::assert_snapshot!(insta::internals::AutoName, output);
}

#[test]
pub fn same_package_defining_duplicate_module() {
    let output = compile_modules("a_package", "a_module", vec![("a_package", "a_module")])
        .into_result()
        .expect_err("should produce an error")
        .pretty_string();

    insta::assert_snapshot!(insta::internals::AutoName, output);
}

fn compile_package_with_colocation(compile_beam_bytecode: bool) -> InMemoryFileSystem {
    let fs = InMemoryFileSystem::new();
    fs.write(
        Utf8Path::new("/src/main.gleam"),
        "pub fn main() -> Nil { Nil }",
    )
    .expect("write gleam module");
    fs.write(Utf8Path::new("/src/native.erl"), "-module(native).\n")
        .expect("write erlang file");
    fs.write(
        Utf8Path::new("/src/native.ex"),
        "defmodule Native do\nend\n",
    )
    .expect("write elixir file");
    // Pre-populate the Elixir core library pathfinder cache so that
    // `ElixirLibraries::make_available` doesn't try to shell out to a real
    // `elixir` executable (which the in-memory command executor can't run)
    // to regenerate it.
    fs.write(Utf8Path::new("/lib/gleam_elixir_paths"), "/fake/eex")
        .expect("write elixir paths cache");

    let mut config = PackageConfig::default();
    config.name = "a_package".into();

    let target = TargetCodegenConfiguration::Erlang {
        app_file: compile_beam_bytecode.then(|| ErlangAppCodegenConfiguration {
            include_dev_deps: false,
            package_name_overrides: HashMap::new(),
        }),
    };

    let mut compiler = PackageCompiler::new(
        &config,
        build::Mode::Dev,
        Utf8Path::new("/"),
        Utf8Path::new("/out"),
        Utf8Path::new("/lib"),
        &target,
        UniqueIdGenerator::new(),
        fs.clone(),
    );
    compiler.compile_beam_bytecode = compile_beam_bytecode;

    let _ = compiler
        .compile(
            &WarningEmitter::null(),
            &mut im::HashMap::new(),
            &mut im::HashMap::new(),
            &mut StaleTracker::default(),
            &mut HashSet::new(),
            &NullTelemetry,
        )
        .into_result()
        .expect("compilation should succeed");

    fs
}

#[test]
fn erlang_app_file_is_written_when_beam_bytecode_is_compiled() {
    let fs = compile_package_with_colocation(true);

    let app_file = fs
        .read(Utf8Path::new("/out/ebin/a_package.app"))
        .expect("app file should exist");

    insta::assert_snapshot!(insta::internals::AutoName, app_file);
}

#[test]
fn erlang_app_file_is_not_written_when_beam_bytecode_compilation_is_skipped() {
    let fs = compile_package_with_colocation(false);

    assert!(!fs.is_file(Utf8Path::new("/out/ebin/a_package.app")));
}
