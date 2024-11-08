#[cfg(test)]
mod generated_tests;

use camino::Utf8PathBuf;
use gleam_core::{
    analyse::TargetSupport,
    build::{Codegen, Compile, Mode, NullTelemetry, Options, ProjectCompiler, Telemetry},
    config::PackageConfig,
    io::{FileSystemReader, FileSystemWriter},
    paths::ProjectPaths,
    warning::VectorWarningEmitterIO,
};
use std::rc::Rc;

pub fn prepare(path: &str, mode: Mode) -> String {
    let root = Utf8PathBuf::from(path).canonicalize_utf8().unwrap();
    let filesystem = test_helpers_rs::to_in_memory_filesystem(&root);
    let initial_files = filesystem.files();

    let toml = std::fs::read_to_string(root.join("gleam.toml")).unwrap();
    let config: PackageConfig = toml::from_str(&toml).unwrap();
    let warnings = VectorWarningEmitterIO::default();
    let telemetry: &'static dyn Telemetry = &NullTelemetry;

    let options = Options {
        mode,
        target: None,
        compile: Compile::All,
        codegen: Codegen::All,
        warnings_as_errors: false,
        root_target_support: TargetSupport::Enforced,
        no_print_progress: true,
    };

    let compiler = ProjectCompiler::new(
        config,
        options,
        vec![],
        telemetry,
        Rc::new(warnings.clone()),
        ProjectPaths::new(root),
        filesystem.clone(),
    );

    compiler.compile().unwrap();

    for path in initial_files {
        if filesystem.is_file(&path) {
            filesystem.delete_file(&path).unwrap();
        }
    }
    let files = filesystem.into_contents();
    let warnings = warnings.take();
    test_helpers_rs::TestCompileOutput { files, warnings }.as_overview_text()
}
