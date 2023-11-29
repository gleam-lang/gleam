mod wasm_filesystem;

mod log_telemetry;

use camino::{Utf8Path, Utf8PathBuf};
use gleam_core::{
    build::{
        Mode, NullTelemetry, PackageCompiler, StaleTracker, Target, TargetCodegenConfiguration,
    },
    config::PackageConfig,
    io::{FileSystemReader, FileSystemWriter},
    uid::UniqueIdGenerator,
    warning::{NullWarningEmitterIO, WarningEmitter},
    Error,
};
use hexpm::version::Version;
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, sync::Arc};
use wasm_filesystem::WasmFileSystem;

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CompileModulesOptions {
    target: Target,
    modules: HashMap<String, String>,
}

/// Compile a set of `modules` into a different set of source files for the
/// `target` language.
pub fn compile_modules(
    modules: HashMap<String, String>,
    target: Target,
) -> Result<HashMap<String, String>, String> {
    let wfs = WasmFileSystem::new();

    for (name, source) in modules.iter() {
        let mut path = Utf8PathBuf::from("/src");
        path.push(name);
        path.set_extension("gleam");
        write_source_file(source, path, &wfs);
    }

    compile_package(&wfs, target).map_err(|e| e.pretty_string())?;

    Ok(gather_compiled_files(&wfs, target).unwrap())
}

fn write_source_file<P: AsRef<Utf8Path>>(source: &str, path: P, wfs: &WasmFileSystem) {
    wfs.write(path.as_ref(), source)
        .expect("should always succeed with the virtual file system");
}

fn compile_package(wfs: &WasmFileSystem, target: Target) -> Result<(), Error> {
    let ids = UniqueIdGenerator::new();
    let mut type_manifests = im::HashMap::new();
    let mut defined_modules = im::HashMap::new();
    let warnings = WarningEmitter::new(Arc::new(NullWarningEmitterIO));
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
            prelude_location: Utf8PathBuf::from("./todo/prelude/location.mjs"),
        },
    };

    tracing::info!("Compiling package");

    let lib = Utf8PathBuf::from("/build/lib");
    let out = Utf8PathBuf::from("/build/out");
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
        &warnings,
        &mut type_manifests,
        &mut defined_modules,
        &mut StaleTracker::default(),
        &NullTelemetry,
    )?;

    Ok(())
}

fn gather_compiled_files(
    wfs: &WasmFileSystem,
    target: Target,
) -> Result<HashMap<String, String>, ()> {
    let mut files: HashMap<String, String> = HashMap::new();

    let extension_to_search_for = match target {
        Target::Erlang => "erl",
        Target::JavaScript => "mjs",
    };

    wfs.read_dir(Utf8Path::new("/build/out"))
        .expect("expect the build directory to exist")
        .into_iter()
        .filter_map(|result| result.ok())
        .filter(|dir_entry| dir_entry.as_path().extension() == Some(extension_to_search_for))
        .filter(|dir_entry| dir_entry.as_path().file_name() != Some("gleam@@compile.erl"))
        .for_each(|dir_entry| {
            let path = dir_entry.as_path();
            let contents: String = wfs.read(path).expect("iterated dir entries should exist");
            let path = path
                .as_str()
                .replace('\\', "/")
                .strip_prefix(match target {
                    Target::JavaScript => "/build/out/",
                    Target::Erlang => "/build/out/_gleam_artefacts/",
                })
                .unwrap_or_else(|| path.as_str())
                .to_string();

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
#[wasm_bindgen(js_name = "compile_modules")]
pub fn js_compile_modules(options: JsValue) -> JsValue {
    match serde_wasm_bindgen::from_value::<CompileModulesOptions>(options) {
        Ok(options) => {
            let result = compile_modules(options.modules, options.target);
            serde_wasm_bindgen::to_value(&result)
        }
        Err(error) => serde_wasm_bindgen::to_value::<Result<HashMap<String, String>, String>>(
            &Err(format!("Invalid options passed to `compile`: `{}`", error)),
        ),
    }
    .expect("should never fail")
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;
    use std::collections::HashMap;

    fn source(source: &str) -> HashMap<String, String> {
        let mut modules = HashMap::new();
        modules.insert("main".into(), source.to_string());
        modules
    }

    #[test]
    fn import_library_compile_javascript_test_wasm() {
        let mut modules = source(
            r#"
            import some/library

            pub fn main() {
              library.fun("Hello, world!")
            }
            "#,
        );

        modules.insert(
            "some/library".into(),
            r#"
            pub fn fun(string: String) -> Nil {
              Nil
            }
        "#
            .to_string(),
        );

        let result = compile_modules(modules, Target::JavaScript).unwrap();

        assert_eq!(
            result.keys().sorted().collect_vec(),
            vec!["gleam.mjs", "main.mjs", "some/library.mjs"]
        );
        assert_eq!(
            result.get("gleam.mjs"),
            Some(&"export * from \"./todo/prelude/location.mjs\";\n".to_string())
        );
        assert_eq!(
            result.get("main.mjs"),
            Some(
                &"import * as $library from \"./some/library.mjs\";

export function main() {
  return $library.fun(\"Hello, world!\");
}
"
                .to_string()
            )
        );
        assert_eq!(
            result.get("some/library.mjs"),
            Some(
                &"export function fun(string) {
  return undefined;
}
"
                .to_string()
            )
        );
    }

    #[test]
    fn import_library_compile_erlang_test_wasm() {
        let mut modules = source(
            r#"
            import some/library

            pub fn main() {
                library.fun("Hello, world!")
            }
            "#,
        );

        modules.insert(
            "some/library".into(),
            r#"
            pub fn fun(string: String) -> Nil {
                Nil
            }
        "#
            .to_string(),
        );

        let result = compile_modules(modules, Target::Erlang).unwrap();

        assert_eq!(
            result.keys().sorted().collect_vec(),
            vec!["main.erl", "some@library.erl"]
        );
        assert_eq!(
            result.get("main.erl"),
            Some(
                &"-module(main).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([main/0]).

-spec main() -> nil.
main() ->
    some@library:'fun'(<<\"Hello, world!\"/utf8>>).
"
                .into()
            )
        );
        assert_eq!(
            result.get("some@library.erl"),
            Some(
                &"-module(some@library).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export(['fun'/1]).

-spec 'fun'(binary()) -> nil.
'fun'(String) ->
    nil.
"
                .into()
            )
        );
    }
}

#[cfg(target_arch = "wasm32")]
#[cfg(test)]
mod wasm_tests {
    use super::*;
    use itertools::Itertools;
    use std::collections::HashMap;
    use wasm_bindgen_test::wasm_bindgen_test;

    fn source(source: &str) -> HashMap<String, String> {
        let mut modules = HashMap::new();
        modules.insert("main".into(), source.to_string());
        modules
    }

    fn compile_wrapper(options: CompileModulesOptions) -> Result<HashMap<String, String>, String> {
        init(false);

        let result = js_compile_modules(serde_wasm_bindgen::to_value(&options).unwrap());
        serde_wasm_bindgen::from_value(result).unwrap()
    }

    #[wasm_bindgen_test]
    fn import_library_compile_javascript_test_wasm() {
        let mut modules = source(
            r#"
            import some/library

            pub fn main() {
                library.fun("Hello, world!")
            }
            "#,
        );

        modules.insert(
            "some/library".into(),
            r#"
            pub fn fun(string: String) -> Nil {
              Nil
            }
        "#
            .to_string(),
        );

        let result = compile_wrapper(CompileModulesOptions {
            modules,
            target: Target::JavaScript,
        })
        .unwrap();

        assert_eq!(
            result.keys().sorted().collect_vec(),
            vec!["gleam.mjs", "main.mjs", "some/library.mjs"]
        );
        assert_eq!(
            result.get("gleam.mjs"),
            Some(&"export * from \"./todo/prelude/location.mjs\";\n".to_string())
        );
        assert_eq!(
            result.get("main.mjs"),
            Some(
                &"import * as $library from \"./some/library.mjs\";

export function main() {
  return $library.fun(\"Hello, world!\");
}
"
                .to_string()
            )
        );
        assert_eq!(
            result.get("some/library.mjs"),
            Some(
                &"export function fun(string) {
  return undefined;
}
"
                .to_string()
            )
        );
    }

    #[wasm_bindgen_test]
    fn import_library_compile_erlang_test_wasm() {
        let mut modules = source(
            r#"
            import some/library

            pub fn main() {
                library.fun("Hello, world!")
            }
            "#,
        );

        modules.insert(
            "some/library".into(),
            r#"
            pub fn fun(string: String) -> Nil {
                Nil
            }
        "#
            .to_string(),
        );

        let result = compile_wrapper(CompileModulesOptions {
            modules,
            target: Target::Erlang,
        })
        .unwrap();

        assert_eq!(
            result.keys().sorted().collect_vec(),
            vec!["main.erl", "some@library.erl"]
        );
        assert_eq!(
            result.get("main.erl"),
            Some(
                &"-module(main).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([main/0]).

-spec main() -> nil.
main() ->
    some@library:'fun'(<<\"Hello, world!\"/utf8>>).
"
                .into()
            )
        );
        assert_eq!(
            result.get("some@library.erl"),
            Some(
                &"-module(some@library).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export(['fun'/1]).

-spec 'fun'(binary()) -> nil.
'fun'(String) ->
    nil.
"
                .into()
            )
        );
    }
}
