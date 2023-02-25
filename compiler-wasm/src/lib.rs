use std::{collections::HashMap, ffi::OsStr, path::Path};

use gleam_core::{
    build::{Codegen, Mode, Options, Package, ProjectCompiler, Target},
    config::PackageConfig,
    io::{FileSystemReader, FileSystemWriter},
    manifest::{Base16Checksum, ManifestPackage, ManifestPackageSource},
    paths, Error,
};

use hexpm::version::Version;
use serde::{Deserialize, Serialize};

mod wasm_filesystem;
use wasm_filesystem::WasmFileSystem;

mod log_telemetry;
use log_telemetry::LogTelemetry;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

const PROJECT_NAME: &str = "gleam-wasm";

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompileOptions {
    target: Target,
    source_files: HashMap<String, String>,
    dependencies: Vec<String>,
    mode: Mode,
}

impl Default for CompileOptions {
    fn default() -> Self {
        CompileOptions {
            target: Target::JavaScript,
            mode: Mode::Dev,
            source_files: HashMap::default(),
            dependencies: Vec::default(),
        }
    }
}

/// Compile a set of `source_files` into a different set of source files for the
/// `target` language.
pub fn compile_(options: CompileOptions) -> Result<HashMap<String, String>, String> {
    let mut wfs = WasmFileSystem::new();

    for (path, source) in options.source_files.iter() {
        write_source_file(source, path, &mut wfs);
    }

    let _package =
        compile_project(&mut wfs, options.target, &options).map_err(|e| e.pretty_string())?;

    Ok(gather_compiled_files(&wfs, options.target).unwrap())
}

fn write_source_file<P: AsRef<Path>>(source: &str, path: P, wfs: &mut WasmFileSystem) {
    wfs.write(path.as_ref(), source)
        .expect("should always succeed with the virtual file system");
}

fn manifest_from_name(name: &str) -> ManifestPackage {
    ManifestPackage {
        name: name.to_string(),
        version: Version {
            major: 0,
            minor: 0,
            patch: 0,
            pre: vec![],
            build: None,
        },
        build_tools: vec!["gleam".into()],
        otp_app: None,
        requirements: vec![],
        source: ManifestPackageSource::Hex {
            outer_checksum: Base16Checksum(vec![]),
        },
    }
}

fn compile_project(
    wfs: &mut WasmFileSystem,
    target: Target,
    compile_options: &CompileOptions,
) -> Result<Package, Error> {
    let packages: Vec<ManifestPackage> = compile_options
        .dependencies
        .iter()
        .map(|s| manifest_from_name(s.as_str()))
        .collect();

    let options = Options {
        mode: Mode::Dev,
        target: Some(target),
        codegen: Codegen::All,
    };

    let mut pcompiler = ProjectCompiler::new(
        PackageConfig {
            target,
            name: PROJECT_NAME.into(),
            ..Default::default()
        },
        options,
        packages,
        Box::new(LogTelemetry),
        wfs.clone(),
    );

    pcompiler.compile()
}

fn gather_compiled_files(
    wfs: &WasmFileSystem,
    target: Target,
) -> Result<HashMap<String, String>, ()> {
    let mut files: HashMap<String, String> = HashMap::new();

    let extension_to_search_for = match target {
        Target::Erlang => OsStr::new("erl"),
        Target::JavaScript => OsStr::new("mjs"),
    };

    wfs.read_dir(&paths::build())
        .expect("expect the build directory to exist")
        .into_iter()
        .filter_map(|result| result.ok())
        .filter(|dir_entry| dir_entry.as_path().extension() == Some(extension_to_search_for))
        .for_each(|dir_entry| {
            let path = dir_entry.as_path();
            let contents: String = wfs.read(path).expect("iterated dir entries should exist");
            let path = path.to_str().unwrap().replace('\\', "/");

            files.insert(path, contents);
        });

    Ok(files)
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

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn compile(options: JsValue) -> JsValue {
    match serde_wasm_bindgen::from_value(options) {
        Ok(compile_options) => {
            let result = compile_(compile_options);
            serde_wasm_bindgen::to_value(&result)
        }
        Err(error) => serde_wasm_bindgen::to_value::<Result<HashMap<String, String>, String>>(
            &Err(format!("Invalid options passed to `compile`: `{}`", error)),
        ),
    }
    .expect("should never fail")
}

#[cfg(target_arch = "wasm32")]
#[cfg(test)]
mod test {
    use super::*;
    use wasm_bindgen_test::wasm_bindgen_test;

    fn source(source: &str) -> HashMap<String, String> {
        let mut source_files = HashMap::new();
        source_files.insert("./src/main.gleam".into(), source.to_string());
        source_files
    }

    fn compile_wrapper(options: CompileOptions) -> Result<HashMap<String, String>, String> {
        init(false);

        let result = compile(serde_wasm_bindgen::to_value(&options).unwrap());
        serde_wasm_bindgen::from_value(result).unwrap()
    }

    #[wasm_bindgen_test]
    fn import_library_compile_javascript_test_wasm() {
        let mut source_files = source(
            r#"
            import some_library

            pub fn main() {
                some_library.function("Hello, world!")
            }
            "#,
        );

        source_files.insert(
            "build/packages/some_library/src/some_library.gleam".into(),
            r#"
            pub fn function(string: String) -> Nil {
                Nil
            }
        "#
            .to_string(),
        );

        source_files.insert(
            "build/packages/some_library/gleam.toml".into(),
            "name = \"some_library\"".into(),
        );

        let result = compile_wrapper(CompileOptions {
            source_files,
            dependencies: vec![String::from("some_library")],
            ..Default::default()
        })
        .unwrap();

        assert_eq!(
            result.get("build/dev/javascript/gleam-wasm/main.mjs"),
            Some(&String::from("import * as $some_library from \"../some_library/some_library.mjs\";\n\nexport function main() {\n  return $some_library.function$(\"Hello, world!\");\n}\n"))
        );
    }

    #[wasm_bindgen_test]
    fn import_library_compile_erlang_test_wasm() {
        let mut source_files = source(
            r#"
            import some_library

            pub fn main() {
                some_library.function("Hello, world!")
            }
            "#,
        );

        source_files.insert(
            "build/packages/some_library/src/some_library.gleam".into(),
            r#"
            pub fn function(string: String) -> Nil {
                Nil
            }
        "#
            .to_string(),
        );

        source_files.insert(
            "build/packages/some_library/gleam.toml".into(),
            "name = \"some_library\"".into(),
        );

        let result = compile_wrapper(CompileOptions {
            source_files,
            target: Target::Erlang,
            dependencies: vec![String::from("some_library")],
            ..Default::default()
        })
        .unwrap();

        assert_eq!(
            result.get("build/dev/erlang/gleam-wasm/_gleam_artefacts/main.erl"),
            Some(&String::from("-module(main).\n-compile([no_auto_import, nowarn_unused_vars]).\n\n-export([main/0]).\n\n-spec main() -> nil.\nmain() ->\n    some_library:function(<<\"Hello, world!\"/utf8>>).\n"))
        );
    }
}
