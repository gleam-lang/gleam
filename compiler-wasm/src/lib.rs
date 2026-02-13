#[cfg(test)]
mod tests;
mod wasm_filesystem;

use camino::Utf8PathBuf;
use gleam_core::{
    Error,
    analyse::TargetSupport,
    build::{
        Mode, NullTelemetry, PackageCompiler, StaleTracker, Target, TargetCodegenConfiguration,
    },
    config::{PackageConfig, PackageKind},
    io::{FileSystemReader, FileSystemWriter},
    uid::UniqueIdGenerator,
    warning::{VectorWarningEmitterIO, WarningEmitter},
};
use hexpm::version::Version;
use im::HashMap;
use std::{cell::RefCell, collections::HashSet, rc::Rc};
use wasm_filesystem::WasmFileSystem;

use wasm_bindgen::prelude::*;

#[derive(Debug, Clone, Default)]
struct Project {
    fs: WasmFileSystem,
    warnings: VectorWarningEmitterIO,
}

thread_local! {
    static PROJECTS: RefCell<HashMap<usize, Project>> = RefCell::new(HashMap::new());
}

/// You should call this once to ensure that if the compiler crashes it gets
/// reported in JavaScript.
///
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn initialise_panic_hook(debug: bool) {
    console_error_panic_hook::set_once();

    if debug {
        let _ = tracing_wasm::try_set_as_global_default();
    }
}

/// Reset the virtual file system to an empty state.
///
#[wasm_bindgen]
pub fn reset_filesystem(project_id: usize) {
    let fs = get_filesystem(project_id);
    fs.reset();
}

/// Delete project, freeing any memory associated with it.
///
#[wasm_bindgen]
pub fn delete_project(project_id: usize) {
    PROJECTS.with(|lock| {
        _ = lock.borrow_mut().remove(&project_id);
    })
}

fn get_project(project_id: usize) -> Project {
    PROJECTS.with(|lock| lock.borrow_mut().entry(project_id).or_default().clone())
}

fn get_filesystem(project_id: usize) -> WasmFileSystem {
    get_project(project_id).fs
}

fn get_warnings(project_id: usize) -> VectorWarningEmitterIO {
    get_project(project_id).warnings
}

/// Write a Gleam module to the `/src` directory of the virtual file system.
///
#[wasm_bindgen]
pub fn write_module(project_id: usize, module_name: &str, code: &str) {
    let fs = get_filesystem(project_id);
    let path = format!("/src/{module_name}.gleam");
    fs.write(&Utf8PathBuf::from(path), code)
        .expect("writing file")
}

/// Write a file to the virtual file system.
///
#[wasm_bindgen]
pub fn write_file(project_id: usize, path: &str, content: &str) {
    let fs = get_filesystem(project_id);
    fs.write(&Utf8PathBuf::from(path), content)
        .expect("writing file")
}

/// Write a non-text file to the virtual file system.
///
#[wasm_bindgen]
pub fn write_file_bytes(project_id: usize, path: &str, content: &[u8]) {
    let fs = get_filesystem(project_id);
    fs.write_bytes(&Utf8PathBuf::from(path), content)
        .expect("writing file")
}

/// Read a file from the virtual file system.
///
#[wasm_bindgen]
pub fn read_file_bytes(project_id: usize, path: &str) -> Option<Vec<u8>> {
    let fs = get_filesystem(project_id);
    fs.read_bytes(&Utf8PathBuf::from(path)).ok()
}

/// Run the package compiler. If this succeeds you can use
///
#[wasm_bindgen]
pub fn compile_package(project_id: usize, target: &str) -> Result<(), String> {
    let target = match target.to_lowercase().as_str() {
        "erl" | "erlang" => Target::Erlang,
        "js" | "javascript" => Target::JavaScript,
        _ => {
            let msg = format!("Unknown target `{target}`, expected `erlang` or `javascript`");
            return Err(msg);
        }
    };

    do_compile_package(get_project(project_id), target).map_err(|e| e.pretty_string())
}

/// Get the compiled JavaScript output for a given module.
///
/// You need to call `compile_package` before calling this function.
///
#[wasm_bindgen]
pub fn read_compiled_javascript(project_id: usize, module_name: &str) -> Option<String> {
    let fs = get_filesystem(project_id);
    let path = format!("/build/{module_name}.mjs");
    fs.read(&Utf8PathBuf::from(path)).ok()
}

/// Get the compiled Erlang output for a given module.
///
/// You need to call `compile_package` before calling this function.
///
#[wasm_bindgen]
pub fn read_compiled_erlang(project_id: usize, module_name: &str) -> Option<String> {
    let fs = get_filesystem(project_id);
    let path = format!(
        "/build/_gleam_artefacts/{}.erl",
        module_name.replace('/', "@")
    );
    fs.read(&Utf8PathBuf::from(path)).ok()
}

/// Clear any stored warnings. This is performed automatically when before compilation.
///
#[wasm_bindgen]
pub fn reset_warnings(project_id: usize) {
    get_warnings(project_id).reset();
}

/// Pop the latest warning from the compiler.
///
#[wasm_bindgen]
pub fn pop_warning(project_id: usize) -> Option<String> {
    get_warnings(project_id).pop().map(|w| w.to_pretty_string())
}

fn do_compile_package(project: Project, target: Target) -> Result<(), Error> {
    let ids = UniqueIdGenerator::new();
    let mut type_manifests = im::HashMap::new();
    let mut defined_modules = im::HashMap::new();
    #[allow(clippy::arc_with_non_send_sync)]
    let warning_emitter = WarningEmitter::new(Rc::new(project.warnings));
    let config = PackageConfig {
        name: "library".into(),
        version: Version::new(1, 0, 0),
        target,
        ..Default::default()
    };

    let target = match target {
        Target::Erlang => TargetCodegenConfiguration::Erlang { app_file: None },
        Target::JavaScript => TargetCodegenConfiguration::JavaScript {
            emit_typescript_definitions: false,
            prelude_location: Utf8PathBuf::from("./gleam_prelude.mjs"),
        },
    };

    tracing::info!("Compiling package");

    let lib = Utf8PathBuf::from("/lib");
    let out = Utf8PathBuf::from("/build");
    let package = Utf8PathBuf::from("/");
    let mut compiler = PackageCompiler::new(
        PackageKind::Root,
        &config,
        Mode::Dev,
        &package,
        &out,
        &lib,
        &target,
        ids,
        project.fs,
    );
    compiler.write_entrypoint = false;
    compiler.write_metadata = false;
    compiler.compile_beam_bytecode = true;
    compiler.target_support = TargetSupport::Enforced;
    compiler
        .compile(
            &warning_emitter,
            &mut type_manifests,
            &mut defined_modules,
            &mut StaleTracker::default(),
            &mut HashSet::new(),
            &NullTelemetry,
        )
        .into_result()
        .map(|_| ())
}
