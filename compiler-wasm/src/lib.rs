use std::collections::HashMap;
use std::ffi::OsStr;
use std::path::Path;

use gleam_core::Error;
use wasm_bindgen::prelude::*;

use hexpm::version::{Range, Version};

use gleam_core::build::{Package, ProjectCompiler, Target, Telemetry};
use gleam_core::config::{Dependencies, Docs, ErlangConfig, PackageConfig, Repository};
use gleam_core::io::{FileSystemReader, FileSystemWriter};
use gleam_core::project::{Base16Checksum, ManifestPackage, ManifestPackageSource};

mod filesystem;
use filesystem::WasmFileSystem;

const PROJECT_NAME: &str = "gleam-wasm";

#[derive(Debug)]
struct VoidTelemetry;

impl Telemetry for VoidTelemetry {
    fn compiling_package(&self, name: &str) {
        log::info!("Compiling package: {}", name);
    }
}

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

    write_source_file(main_source, "src/main.gleam", &mut wfs);

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

    let pcompiler = ProjectCompiler::new(
        PackageConfig {
            name: PROJECT_NAME.to_string(),
            version: Version::new(1, 0, 0),
            licences: vec![],
            description: "".to_string(),
            docs: Docs::default(),
            dependencies: deps,
            dev_dependencies: Dependencies::default(),
            repository: Repository::default(),
            links: vec![],
            erlang: ErlangConfig::default(),
        },
        &packages,
        Box::new(VoidTelemetry),
        wfs.clone(),
    );

    pcompiler.compile(target)
}

fn gather_compiled_files(
    wfs: &WasmFileSystem,
    target: Target,
) -> Result<HashMap<String, String>, ()> {
    let mut files: HashMap<String, String> = HashMap::new();

    let extension_to_search_for = match target {
        Target::Erlang => Some(OsStr::new("erl")),
        Target::JavaScript => Some(OsStr::new("js")),
    };

    for file in wfs.read_dir(&Path::new("build")).unwrap() {
        let file = file.unwrap();

        if file.path().extension() == extension_to_search_for {
            println!("gathering: {:?}", file.path());

            let contents: String = wfs.read(file.path().as_path()).unwrap();

            let path1 = file.path().to_owned();
            let path = path1
                .to_str()
                .unwrap()
                .replace("\\", "/")
                .replace("build/dev/javascript/", "gleam-packages/");
            files.insert(path, contents);
        }
    }

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
        result.get("gleam-packages/gleam-wasm/main.js"),
        Some(&String::from("import * as $io from \"gleam-packages/gleam_stdlib/gleam/io.js\";\n\nexport function main() {\n  return $io.println(\"Hello, world!\");\n}\n"))
    );

    //let gathered_files = gather_compiled_files(&wfs, Target::JavaScript).unwrap();

    for key in result.keys() {
        println!("{:?}", key);
    }

    //println!("{:?}", result);
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

    // assert_eq!(
    //     result.get("gleam-packages/gleam-wasm/main.js"),
    //     Some(&String::from("import * as $io from \"gleam-packages/gleam_stdlib/gleam/io.js\";\n\nexport function main() {\n  return $io.println(\"Hello, world!\");\n}\n"))
    // );

    //let gathered_files = gather_compiled_files(&wfs, Target::JavaScript).unwrap();

    for key in result.keys() {
        println!("{:?}", key);
    }

    //println!("{:?}", result);
}

// #[test]
// fn test_compile_library() {
//     let mut wfs = WasmFileSystem::new();

//     let _result = compile_std_library(&mut wfs, Target::JavaScript).unwrap();

//     // Writes JS files to the build folder.
//     assert!(wfs.read(Path::new("build/gleam/order.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/order.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/int.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/pair.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/list.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/option.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/result.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/map.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/iterator.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/queue.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/bit_string.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/dynamic.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/function.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/regex.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/string.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/uri.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/set.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/float.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/io.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/bool.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/bit_builder.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/base.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam/string_builder.js")).is_ok());
//     assert!(wfs.read(Path::new("build/gleam.js")).is_ok());

//     let gathered_files = gather_compiled_files(&wfs, Target::JavaScript).unwrap();

//     for key in gathered_files.keys() {
//         //println!("{:?}", key);
//     }
// }
