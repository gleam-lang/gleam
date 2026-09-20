// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2022 The Gleam contributors

#[cfg(test)]
mod tests;

use camino::{Utf8Path, Utf8PathBuf};
use ecow::EcoString;
use gleam_core::{
    build::{
        ErlangAppCodegenConfiguration, ErlangOutput, Mode, NullTelemetry, Outcome, StaleTracker,
        Target, TargetCodegenConfiguration,
    },
    config::PackageConfig,
    io::{FileSystemReader, FileSystemWriter, memory::InMemoryFileSystem},
    uid::UniqueIdGenerator,
    warning::{VectorWarningEmitterIO, WarningEmitter},
};
use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};
use test_helpers_rs::TestCompileOutput;

pub struct TestHarness {
    file_system: InMemoryFileSystem,
    initial_files: Vec<Utf8PathBuf>,
    warnings: VectorWarningEmitterIO,
    ids: UniqueIdGenerator,
}

impl TestHarness {
    pub fn new() -> Self {
        let root = Utf8PathBuf::from("cases/")
            .canonicalize_utf8()
            .unwrap()
            .to_path_buf();
        let file_system = test_helpers_rs::to_in_memory_filesystem(&root);
        Self {
            initial_files: file_system.files(),
            file_system,
            warnings: VectorWarningEmitterIO::default(),
            ids: UniqueIdGenerator::new(),
        }
    }

    pub fn compile(&mut self, arguments: Compilation) -> Result<(), String> {
        let root = Utf8PathBuf::default().join(arguments.package);
        let toml = self.file_system.read(&root.join("gleam.toml")).unwrap();
        let config: PackageConfig = toml::from_str(&toml).unwrap();
        let target = match arguments.target.unwrap_or(config.target) {
            Target::Erlang => TargetCodegenConfiguration::Erlang {
                app_file: Some(ErlangAppCodegenConfiguration {
                    include_dev_deps: true,
                    package_name_overrides: arguments.otp_app_overrides,
                }),
                output: ErlangOutput::Binary,
            },
            Target::JavaScript => TargetCodegenConfiguration::JavaScript {
                emit_typescript_definitions: config.javascript.typescript_declarations,
                emit_source_maps: config.javascript.source_maps,
                prelude_location: Utf8PathBuf::from("../prelude.mjs"),
            },
        };
        let mut modules = imbl::HashMap::new();
        let warning_emitter = WarningEmitter::new(Rc::new(self.warnings.clone()));
        let mode = if arguments.src_only {
            Mode::Prod
        } else {
            Mode::Dev
        };
        let mut compiler = gleam_core::build::PackageCompiler::new(
            &config,
            mode,
            &root,
            &root,
            Utf8Path::new(""),
            &target,
            self.ids.clone(),
            self.file_system.clone(),
        );
        compiler.write_entrypoint = false;
        compiler.write_metadata = true;
        compiler.copy_native_files = false;
        let result = compiler.compile(
            &warning_emitter,
            &mut modules,
            &mut imbl::HashMap::new(),
            &mut StaleTracker::default(),
            &mut HashSet::new(),
            &NullTelemetry,
        );
        match result {
            Outcome::Ok(_) => Ok(()),
            Outcome::TotalFailure(error) | Outcome::PartialFailure(_, error) => Err(
                test_helpers_rs::normalise_diagnostic(&error.pretty_string()),
            ),
        }
    }

    pub fn into_snapshot(self) -> String {
        for path in self.initial_files {
            if self.file_system.is_file(&path) {
                self.file_system.delete_file(&path).unwrap();
            }
        }
        let files = self.file_system.into_contents();
        let warnings = self.warnings.take();
        TestCompileOutput { files, warnings }.as_overview_text()
    }
}

impl Default for TestHarness {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Compilation {
    package: &'static str,
    target: Option<Target>,
    src_only: bool,
    otp_app_overrides: HashMap<EcoString, EcoString>,
}

impl Compilation {
    pub fn for_package(package: &'static str) -> Self {
        Self {
            package,
            target: None,
            src_only: false,
            otp_app_overrides: HashMap::new(),
        }
    }

    pub fn src_only(mut self) -> Self {
        self.src_only = true;
        self
    }

    pub fn otp_app_override(mut self, package: &str, otp_app: &str) -> Self {
        let _ = self
            .otp_app_overrides
            .insert(package.into(), otp_app.into());
        self
    }
}

pub fn run_package_compiler(package: &'static str) -> String {
    let mut harness = TestHarness::new();
    match harness.compile(Compilation::for_package(package)) {
        Ok(_) => harness.into_snapshot(),
        Err(error) => error,
    }
}
