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
    reset_filesystem();
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
    reset_filesystem();
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
