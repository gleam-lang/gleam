// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

use std::collections::{HashMap, HashSet};

use camino::{Utf8Path, Utf8PathBuf};
use ecow::EcoString;

use crate::{
    Error,
    build::{
        self, ErlangAppCodegenConfiguration, ErlangOutput, NullTelemetry, Outcome, PackageCompiler,
        StaleTracker, TargetCodegenConfiguration, package_compiler::Compiled,
    },
    config::PackageConfig,
    error::DefinedModuleOrigin,
    io::{FileSystemReader, FileSystemWriter, memory::InMemoryFileSystem},
    strings::to_snake_case,
    uid::UniqueIdGenerator,
    warning::WarningEmitter,
};

fn compile_modules(
    package_name: &str,
    modules: Vec<(Utf8PathBuf, String)>,
    existing_modules: Vec<(&str, &str)>,
) -> (InMemoryFileSystem, Outcome<Compiled, Error>) {
    let fs = InMemoryFileSystem::new();
    //
    // Pre-populate the Elixir core library pathfinder cache so that
    // `ElixirLibraries::make_available` doesn't try to shell out to a real
    // `elixir` executable (which the in-memory command executor can't run)
    // to regenerate it.
    fs.write(Utf8Path::new("/lib/gleam_elixir_paths"), "/fake/eex")
        .expect("write elixir paths cache");

    for (path, code) in &modules {
        fs.write(&Utf8PathBuf::from(format!("/src/{path}")), code)
            .expect("write module");
    }

    let mut config = PackageConfig::default();
    config.name = package_name.into();

    let app_file = Some(ErlangAppCodegenConfiguration {
        include_dev_deps: false,
        package_name_overrides: HashMap::new(),
    });

    let target = &TargetCodegenConfiguration::Erlang {
        app_file,
        output: ErlangOutput::Binary,
    };

    let mut compiler = PackageCompiler::new(
        &config,
        build::Mode::Dev,
        Utf8Path::new("/"),
        Utf8Path::new("/out"),
        Utf8Path::new("/lib"),
        target,
        UniqueIdGenerator::new(),
        fs.clone(),
    );

    compiler.compile_beam_bytecode = true;

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

    let outcome = compiler.compile(
        &WarningEmitter::null(),
        &mut im::HashMap::new(),
        &mut already_defined_modules,
        &mut StaleTracker::default(),
        &mut HashSet::new(),
        &NullTelemetry,
    );

    (fs, outcome)
}
fn gleam_module(name: &str) -> (Utf8PathBuf, String) {
    (
        Utf8PathBuf::from(format!("{name}.gleam")),
        "pub fn main() -> Nil { Nil }".to_string(),
    )
}

fn erlang_module(name: &str) -> (Utf8PathBuf, String) {
    (
        Utf8PathBuf::from(format!("{name}.erl")),
        format!(
            r#"
        -module({name}).
        -exports(main/0).

        main() -> nil.
        "#
        ),
    )
}
fn elixir_module(name: &str) -> (Utf8PathBuf, String) {
    let file = to_snake_case(name);
    (
        Utf8PathBuf::from(format!("{file}.ex")),
        format!("defmodule {name}, do: def main, do: nil"),
    )
}
#[test]
pub fn different_packages_defining_duplicate_module() {
    let (_, output) = compile_modules(
        "a_package",
        vec![gleam_module("a_module")],
        vec![("dep1", "a_module")],
    );
    let output = output
        .into_result()
        .expect_err("should produce an error")
        .pretty_string();

    insta::assert_snapshot!(insta::internals::AutoName, output);
}

#[test]
pub fn same_package_defining_duplicate_module() {
    let (_, output) = compile_modules(
        "a_package",
        vec![gleam_module("a_module")],
        vec![("a_package", "a_module")],
    );
    let output = output
        .into_result()
        .expect_err("should produce an error")
        .pretty_string();

    insta::assert_snapshot!(insta::internals::AutoName, output);
}

#[test]
fn app_file_is_written_when_package_is_compiled() {
    let (fs, _) = compile_modules(
        "a_package",
        vec![
            gleam_module("a_module"),
            erlang_module("some_erl"),
            elixir_module("SomeElixir"),
        ],
        vec![],
    );

    let app_file = fs
        .read(Utf8Path::new("/out/ebin/a_package.app"))
        .expect("app file should exist");

    insta::assert_snapshot!(insta::internals::AutoName, app_file);
}
