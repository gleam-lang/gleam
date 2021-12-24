use std::collections::HashMap;
use std::ffi::OsStr;
use std::path::Path;

use gleam_core::Error;
use wasm_bindgen::prelude::*;

use hexpm::version::{Range, Version};

use gleam_core::build::{Mode, Options, Package, ProjectCompiler, Target};
use gleam_core::config::{Dependencies, Docs, ErlangConfig, PackageConfig, Repository};
use gleam_core::io::{FileSystemReader, FileSystemWriter};
use gleam_core::project::{Base16Checksum, ManifestPackage, ManifestPackageSource};

mod wasm_filesystem;
use wasm_filesystem::WasmFileSystem;

mod log_telemetry;
use log_telemetry::LogTelemetry;

mod static_files;

const PROJECT_NAME: &str = "gleam-wasm";

/// Compile a set of `source_files` into a different set of source files for the
/// `target` language.
///
/// The `main_source` file is required.
/// The `additional_source_files` are optional.
pub fn compile(
    main_source: &str,
    additional_source_files: HashMap<String, String>,
    target: Target,
) -> Result<HashMap<String, String>, String> {
    let mut wfs = WasmFileSystem::new();

    write_source_file(main_source, "./src/main.gleam", &mut wfs);

    for (path, source) in additional_source_files.into_iter() {
        write_source_file(&source, &path, &mut wfs);
    }

    let _package = compile_project(&mut wfs, target).map_err(|e| e.pretty_string())?;

    Ok(gather_compiled_files(&wfs, target).unwrap())
}

fn write_source_file<P: AsRef<Path>>(source: &str, path: P, wfs: &mut WasmFileSystem) {
    wfs.writer(path.as_ref())
        .expect("should always succeed with the virtual file system")
        .write(source.as_bytes())
        .expect("should always succeed with the virtual file system");
}

fn compile_project(wfs: &mut WasmFileSystem, target: Target) -> Result<Package, Error> {
    let mut deps = HashMap::new();
    deps.insert("gleam_stdlib".to_string(), Range::new("*".to_string()));

    let packages = vec![ManifestPackage {
        name: "gleam_stdlib".to_string(),
        version: Version {
            major: 0,
            minor: 18,
            patch: 0,
            pre: vec![],
            build: None,
        },
        build_tools: vec!["gleam".to_string()],
        otp_app: Some("gleam_stdlib".to_string()),
        requirements: vec![],
        source: ManifestPackageSource::Hex {
            outer_checksum: Base16Checksum(vec![
                41, 56, 249, 150, 187, 178, 93, 117, 233, 115, 34, 104, 70, 205, 223, 163, 58, 181,
                89, 10, 252, 138, 157, 4, 58, 53, 110, 168, 82, 114, 81, 13,
            ]),
        },
    }];

    let options = Options {
        mode: Mode::Dev,
        target: Some(target),
        perform_codegen: true,
    };

    let pcompiler = ProjectCompiler::new(
        PackageConfig {
            target: target,
            name: PROJECT_NAME.to_string(),
            version: Version::new(1, 0, 0),
            licences: vec![],
            description: "".to_string(),
            documentation: Docs::default(),
            dependencies: deps,
            dev_dependencies: Dependencies::default(),
            repository: Repository::default(),
            links: vec![],
            erlang: ErlangConfig::default(),
        },
        &options,
        &packages,
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

    wfs.read_dir(&Path::new("build"))
        .expect("expect the build directory to exist")
        .filter_map(|result| result.ok())
        .filter(|dir_entry| dir_entry.as_path().extension() == Some(extension_to_search_for))
        .for_each(|dir_entry| {
            println!("gathering: {:?}", dir_entry.as_path());

            let path = dir_entry.as_path();
            let contents: String = wfs.read(path).expect("iterated dir entries should exist");
            let path = path
                .to_str()
                .unwrap()
                .replace("\\", "/")
                .replace("build/packages/", "gleam-packages/")
                .replace("build/dev/javascript/", "gleam-packages/");

            files.insert(path, contents);
        });

    Ok(files)
}

/// Should be called once to setup any state that persists across compilation
/// cycles.
#[wasm_bindgen]
pub fn init() {
    console_error_panic_hook::set_once();

    if cfg!(debug_assertions) {
        wasm_logger::init(wasm_logger::Config::new(log::Level::Debug))
    }
}

#[wasm_bindgen]
pub fn compile_to_js(gleam_source: &str) -> JsValue {
    let result = compile(gleam_source, HashMap::new(), Target::JavaScript);

    JsValue::from_serde(&result).expect("should never fail")
}

#[wasm_bindgen]
pub fn compile_to_erlang(erlang_source: &str) -> JsValue {
    let result = compile(erlang_source, HashMap::new(), Target::Erlang);

    JsValue::from_serde(&result).expect("should never fail")
}

#[test]
fn test_javascript_project_stdlib() {
    let result = compile(
        r#"
    import gleam/io

    pub fn main() {
        io.println("Hello, world!")
    }
    "#,
        HashMap::new(),
        Target::JavaScript,
    )
    .unwrap();

    assert_eq!(
        result.get("gleam-packages/gleam-wasm/dist/main.mjs"),
        Some(&String::from("import * as $io from \"../../gleam_stdlib/dist/gleam/io.mjs\";\n\nexport function main() {\n  return $io.println(\"Hello, world!\");\n}\n"))
    );

    for key in result.keys() {
        println!("{:?}", key);
    }
}

#[test]
fn test_erlang_project_stdlib() {
    let result = compile(
        r#"
    import gleam/io

    pub fn main() {
        io.println("Hello, world!")
    }
    "#,
        HashMap::new(),
        Target::Erlang,
    )
    .unwrap();
}
