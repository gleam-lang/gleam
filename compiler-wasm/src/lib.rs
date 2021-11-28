use std::collections::HashMap;
use std::path::Path;

use wasm_bindgen::prelude::*;

use hexpm::version::{Range, Version};

use gleam_core::build::{ProjectCompiler, Target, Telemetry};
use gleam_core::config::{Dependencies, Docs, ErlangConfig, PackageConfig, Repository};
use gleam_core::io::{FileSystemReader, FileSystemWriter};
use gleam_core::project::{Base16Checksum, ManifestPackage, ManifestPackageSource};

mod filesystem;
use filesystem::WasmFileSystem;

#[wasm_bindgen]
pub fn init_panic_hook() {
    console_error_panic_hook::set_once();
}

#[derive(Debug)]
struct VoidTelemetry;

impl Telemetry for VoidTelemetry {
    fn compiling_package(&self, _name: &str) {}
}

#[wasm_bindgen]
pub fn compile_to_js(gleam_source: &str) -> JsValue {
    init_panic_hook();
    wasm_logger::init(wasm_logger::Config::new(log::Level::Debug));

    let imfs = WasmFileSystem::new();

    imfs.writer(&Path::new("src/main.gleam"))
        .unwrap()
        .write(gleam_source.as_bytes())
        .unwrap();

    let mut deps = HashMap::new();
    deps.insert("gleam_stdlib".to_string(), Range::new("*".to_string()));

    let stdlib_package = vec![ManifestPackage {
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
            name: "gleam-playground".to_string(),
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
        &stdlib_package,
        Box::new(VoidTelemetry),
        imfs.clone(),
    );

    let result: Result<HashMap<String, String>, String> =
        match pcompiler.compile(Target::JavaScript) {
            Ok(package) => {
                let mut files_map: HashMap<String, String> = HashMap::new();

                for module in package.modules {
                    files_map.insert(
                        format!("./{}.js", module.name),
                        imfs.read(&Path::new(&format!(
                            "build/dev/javascript/gleam-playground/{}.js",
                            module.name
                        )))
                        .expect("should be found"),
                    );
                }

                if let Ok(entry) = imfs.read(Path::new(&format!(
                    "build/dev/javascript/gleam-playground/gleam.js"
                ))) {
                    files_map.insert("./gleam.js".to_string(), entry);
                }

                if let Ok(entry) = imfs.read(Path::new(&format!("src/main.gleam.js"))) {
                    files_map.insert("./main.gleam.js".to_string(), entry);
                }

                // Copy build files to in-memory-file-system
                let files = [
                    "gleam_stdlib/gleam.js",
                    "gleam_stdlib/gleam/base.js",
                    "gleam_stdlib/gleam/bit_builder.js",
                    "gleam_stdlib/gleam/bit_string.js",
                    "gleam_stdlib/gleam/bool.js",
                    "gleam_stdlib/gleam/dynamic.js",
                    "gleam_stdlib/gleam/float.js",
                    "gleam_stdlib/gleam/function.js",
                    "gleam_stdlib/gleam/int.js",
                    "gleam_stdlib/gleam/io.js",
                    "gleam_stdlib/gleam/iterator.js",
                    "gleam_stdlib/gleam/list.js",
                    "gleam_stdlib/gleam/map.js",
                    "gleam_stdlib/gleam/option.js",
                    "gleam_stdlib/gleam/order.js",
                    "gleam_stdlib/gleam/pair.js",
                    "gleam_stdlib/gleam/queue.js",
                    "gleam_stdlib/gleam/regex.js",
                    "gleam_stdlib/gleam/result.js",
                    "gleam_stdlib/gleam/set.js",
                    "gleam_stdlib/gleam/string_builder.js",
                    "gleam_stdlib/gleam/string.js",
                    "gleam_stdlib/gleam/uri.js",
                ];

                for file in files {
                    let r = imfs
                        .read(Path::new(&format!("build/dev/javascript/{}", file)))
                        .unwrap();
                    files_map.insert(format!("gleam-packages/{}", file), r);
                }

                let r = imfs
                    .read(Path::new("packages/stdlib/src/gleam_stdlib.js"))
                    .unwrap();
                files_map.insert("gleam-packages/gleam_stdlib/gleam_stdlib.js".to_string(), r);

                Ok(files_map)
            }
            Err(error) => Err(error.pretty_string()),
        };

    JsValue::from_serde(&result).unwrap()
}

#[test]
fn test() {
    compile_to_js(
        r#"
    import gleam/io

    pub fn main() {
        io.print("hi")
        42
    }
    "#,
    );
}
