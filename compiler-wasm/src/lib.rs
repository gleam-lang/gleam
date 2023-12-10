#[cfg(test)]
mod tests;
mod wasm_filesystem;
#[cfg(target_arch = "wasm32")]
#[cfg(test)]
mod wasm_tests;

mod log_telemetry;

use camino::Utf8PathBuf;
use gleam_core::{
    build::{
        Mode, NullTelemetry, PackageCompiler, StaleTracker, Target, TargetCodegenConfiguration,
    },
    config::PackageConfig,
    io::{FileSystemReader, FileSystemWriter},
    uid::UniqueIdGenerator,
    warning::{VectorWarningEmitterIO, WarningEmitter},
    Error,
};
use hexpm::version::Version;
use std::{cell::OnceCell, sync::Arc};
use wasm_filesystem::WasmFileSystem;

use wasm_bindgen::prelude::*;

thread_local! {
    static FILE_SYSTEM: OnceCell<WasmFileSystem> = OnceCell::new();
    static WARNINGS: OnceCell<VectorWarningEmitterIO> = OnceCell::new();
}

/// Should be called once to setup any state that persists across compilation
/// cycles.
#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn init(debug: bool) {
    console_error_panic_hook::set_once();

    if debug {
        let _ = tracing_wasm::try_set_as_global_default();
    }
}

/// Reset the virtual file system to an empty state.
///
#[wasm_bindgen]
pub fn reset_filesystem() {
    let fs = get_filesystem();
    fs.reset();
}

fn get_filesystem() -> WasmFileSystem {
    FILE_SYSTEM.with(|cell| cell.get_or_init(WasmFileSystem::new).clone())
}

fn get_warnings() -> VectorWarningEmitterIO {
    WARNINGS.with(|cell| cell.get_or_init(VectorWarningEmitterIO::new).clone())
}

/// Write a Gleam module to the `/src` directory of the virtual file system.
///
#[wasm_bindgen]
pub fn write_module(module_name: &str, code: &str) {
    let fs = get_filesystem();
    let path = format!("/src/{}.gleam", module_name);
    fs.write(&Utf8PathBuf::from(path), code)
        .expect("writing file")
}

/// Write a file to the virtual file system.
///
#[wasm_bindgen]
pub fn write_file(path: &str, content: &str) {
    let fs = get_filesystem();
    fs.write(&Utf8PathBuf::from(path), content)
        .expect("writing file")
}

/// Write a non-text file to the virtual file system.
///
#[wasm_bindgen]
pub fn write_file_bytes(path: &str, content: &[u8]) {
    let fs = get_filesystem();
    fs.write_bytes(&Utf8PathBuf::from(path), content)
        .expect("writing file")
}

/// Read a file from the virtual file system.
///
#[wasm_bindgen]
pub fn read_file_bytes(path: &str) -> Option<Vec<u8>> {
    let fs = get_filesystem();
    fs.read_bytes(&Utf8PathBuf::from(path)).ok()
}

/// Run the package compiler. If this succeeds you can use
///
#[wasm_bindgen]
pub fn compile_package(target: &str) -> Result<(), String> {
    let fs = get_filesystem();
    let target = match target.to_lowercase().as_str() {
        "erl" | "erlang" => Target::Erlang,
        "js" | "javascript" => Target::JavaScript,
        _ => {
            let msg = format!("Unknown target `{target}`, expected `erlang` or `javascript`");
            return Err(msg);
        }
    };

    do_compile_package(&fs, target).map_err(|e| e.pretty_string())
}

/// Get the compiled JavaScript output for a given module.
///
/// You need to call `compile_package` before calling this function.
///
pub fn read_compiled_javascript(module_name: &str) -> Option<String> {
    let fs = get_filesystem();
    let path = format!("/build/{}.mjs", module_name);
    fs.read(&Utf8PathBuf::from(path)).ok()
}

/// Get the compiled Erlang output for a given module.
///
/// You need to call `compile_package` before calling this function.
///
pub fn read_compiled_erlang(module_name: &str) -> Option<String> {
    let fs = get_filesystem();
    let path = format!(
        "/build/_gleam_artefacts/{}.erl",
        module_name.replace('/', "@")
    );
    fs.read(&Utf8PathBuf::from(path)).ok()
}

/// Clear any stored warnings. This is performed automatically when before compilation.
///
pub fn reset_warnings() {
    get_warnings().reset();
}

/// Pop the latest warning from the compiler.
///
pub fn pop_warning() -> Option<String> {
    get_warnings().pop().map(|w| w.to_pretty_string())
}

fn do_compile_package(wfs: &WasmFileSystem, target: Target) -> Result<(), Error> {
    let ids = UniqueIdGenerator::new();
    let mut type_manifests = im::HashMap::new();
    let mut defined_modules = im::HashMap::new();
    #[allow(clippy::arc_with_non_send_sync)]
    let warning_emitter = WarningEmitter::new(Arc::new(get_warnings()));
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
        &config,
        Mode::Dev,
        &package,
        &out,
        &lib,
        &target,
        ids,
        wfs.clone(),
    );
    compiler.write_entrypoint = false;
    compiler.write_metadata = false;
    compiler.compile_beam_bytecode = true;
    _ = compiler.compile(
        &warning_emitter,
        &mut type_manifests,
        &mut defined_modules,
        &mut StaleTracker::default(),
        &NullTelemetry,
    )?;

    Ok(())
}
