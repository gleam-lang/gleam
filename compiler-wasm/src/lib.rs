use std::collections::HashMap;
use std::ffi::OsStr;
use std::hash::Hash;
use std::path::{Path, PathBuf};

use gleam_core::build::package_compiler::{Options, Source};
use wasm_bindgen::prelude::*;

use hexpm::version::{Range, Version};

use gleam_core::build::{Origin, Package, PackageCompiler, ProjectCompiler, Target, Telemetry};
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
/// The `main_source` file is always required.
/// The `additional_source_files` are optional.
///
/// Libraries are paths in the static file system containing libraries to
/// include with compilation, such as the std-library.
pub fn compile(
    main_source: &str,
    additional_source_files: HashMap<String, String>,
    target: Target,
) -> Result<HashMap<String, String>, ()> {
    let mut wfs = WasmFileSystem::new();

    write_source_file(main_source, "src/main.gleam", &mut wfs);

    for (path, source) in additional_source_files.into_iter() {
        write_source_file(&source, &path, &mut wfs);
    }

    //let _package = compile_std_library(&mut wfs, target).unwrap();
    let _package = compile_project(&mut wfs, target).unwrap();

    gather_compiled_files(&wfs, target)
}

fn write_source_file<P: AsRef<Path>>(source: &str, path: P, wfs: &mut WasmFileSystem) {
    wfs.writer(path.as_ref())
        .expect("should always succeed with the virtual file system")
        .write(source.as_bytes())
        .expect("should always succeed with the virtual file system");
}

fn compile_project(wfs: &mut WasmFileSystem, target: Target) -> Result<Package, ()> {
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

    let mut pcompiler = ProjectCompiler::new(
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

    Ok(pcompiler.compile(target).unwrap())
}

fn gather_compiled_project_files(
    package: &Package,
    wfs: &WasmFileSystem,
    target: Target,
) -> Result<HashMap<String, String>, ()> {
    let mut files: HashMap<String, String> = HashMap::new();

    for module in package.modules.iter() {
        let path = match target {
            Target::JavaScript => format!("./{}.js", module.name),
            Target::Erlang => format!("./{}.erl", module.name),
        };

        let existing_path = match target {
            Target::JavaScript => {
                format!("build/dev/javascript/{}/{}.js", PROJECT_NAME, module.name)
            }
            Target::Erlang => format!(
                "build/dev/erlang/{}/gleam@{}.erl",
                PROJECT_NAME, module.name
            ),
        };

        files.insert(
            path,
            wfs.read(&Path::new(&existing_path))
                .expect("should be found"),
        );
    }

    Ok(files)
}

fn gather_compiled_files(
    //package: &Package,
    wfs: &WasmFileSystem,
    target: Target,
) -> Result<HashMap<String, String>, ()> {
    //let files_map = gather_compiled_project_files(package, wfs, target).unwrap();
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
                .replace("build\\dev\\javascript\\", "gleam-packages/")
                .replace("\\", "/");

            files.insert(
                path,
                // format!(
                //     "gleam-packages/gleam_stdlib/gleam/{}",
                //     file.path().file_name().unwrap().to_str().unwrap()
                // ),
                contents,
            );
        }
    }

    Ok(files)
}

// fn compile_std_library(wfs: &mut WasmFileSystem, target: Target) -> Result<Package, ()> {
//     let mut pcompiler = PackageCompiler::new(
//         Options {
//             target,
//             name: String::from("stdlib"),
//             src_path: PathBuf::from("stdlib/src"),
//             test_path: None,
//             out_path: PathBuf::from("build"),
//             write_metadata: false,
//         },
//         wfs.clone(),
//     );

//     for file in wfs.read_dir(&Path::new("stdlib/src")).unwrap() {
//         let file = file.unwrap();

//         if file.path().extension() == Some(OsStr::new("gleam")) {
//             println!("compiling stdlib: {:?}", file.path());

//             let contents: String = wfs.read(file.path().as_path()).unwrap();
//             let name = file
//                 .path()
//                 .file_stem()
//                 .unwrap()
//                 .to_str()
//                 .unwrap()
//                 .to_string();

//             println!("{:?}", name);

//             "build\\dev\\javascript\\";
//             // becomes
//             "gleam-packages/javascript\\";

//             let path1 = file.path().to_owned();
//             let path = path1
//                 .to_str()
//                 .unwrap()
//                 .replace("build\\dev\\javascript\\", "gleam-packages/javascript\\")
//                 .replace("\\", "/");

//             pcompiler.sources.push(Source {
//                 path: PathBuf::from(path),
//                 name: format!("gleam/{}", name),
//                 code: contents,
//                 origin: Origin::Src, // TODO: is this used?
//             });
//         }
//     }

//     Ok(pcompiler
//         .compile(&mut Vec::new(), &mut HashMap::new(), &mut HashMap::new())
//         .unwrap())
// }

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
    //init_panic_hook();
    //wasm_logger::init(wasm_logger::Config::new(log::Level::Debug));

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

// #[test]
// fn test() {
//     compile_to_js(
//         r#"
//     import gleam/io

//     pub fn main() {
//         io.print("hi")
//         42
//     }
//     "#,
//     );
// }

// #[test]
// fn test_javascript_project_no_stdlib() {
//     let result = compile(
//         r#"
//     pub fn main() {
//         42
//     }
//     "#,
//         HashMap::new(),
//         &[],
//         Target::JavaScript,
//     )
//     .unwrap();

//     assert_eq!(
//         result.get("./main.js"),
//         Some(&String::from("export function main() {\n  return 42;\n}\n"))
//     );

//     println!("{:?}", result);
// }

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
