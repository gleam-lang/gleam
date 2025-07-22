use super::*;

use wasm_bindgen_test::wasm_bindgen_test;

#[wasm_bindgen_test]
fn test_reset_filesystem() {
    reset_filesystem(0);
    assert_eq!(read_file_bytes(0, "hello"), None);
    write_file_bytes(0, "hello", vec![1, 2, 3].as_slice());
    assert_eq!(read_file_bytes(0, "hello"), Some(vec![1, 2, 3]));
    reset_filesystem(0);
    assert_eq!(read_file_bytes(0, "hello"), None);
}

#[wasm_bindgen_test]
fn test_write_module() {
    reset_filesystem(0);
    assert_eq!(read_file_bytes(0, "/src/some/module.gleam"), None);
    write_module(0, "some/module", "const x = 1");
    assert_eq!(
        read_file_bytes(0, "/src/some/module.gleam"),
        Some(vec![99, 111, 110, 115, 116, 32, 120, 32, 61, 32, 49]),
    );
    reset_filesystem(0);
    assert_eq!(read_file_bytes(0, "/src/some/module.gleam"), None);
}

#[wasm_bindgen_test]
fn test_compile_package_bad_target() {
    reset_filesystem(0);
    assert!(compile_package(0, "ruby").is_err());
}

#[wasm_bindgen_test]
fn test_compile_package_empty() {
    reset_filesystem(0);
    assert!(compile_package(0, "javascript").is_ok());
}

#[wasm_bindgen_test]
fn test_compile_package_js() {
    reset_filesystem(0);
    write_module(0, "one/two", "pub const x = 1");
    write_module(0, "up/down", "import one/two pub fn go() { two.x }");
    assert!(compile_package(0, "javascript").is_ok());

    assert_eq!(
        read_compiled_javascript(0, "one/two"),
        Some("export const x = 1;\n".into())
    );

    assert_eq!(
        read_compiled_javascript(0, "up/down"),
        Some(
            r#"import * as $two from "../one/two.mjs";

export function go() {
  return $two.x;
}
"#
            .into()
        )
    );

    // And now an error!
    write_module(0, "up/down", "import one/two/three");
    assert!(compile_package(0, "javascript").is_err());

    // Let's fix that.
    write_module(0, "up/down", "pub const y = 1");
    assert!(compile_package(0, "javascript").is_ok());
    assert_eq!(
        read_compiled_javascript(0, "up/down"),
        Some("export const y = 1;\n".into())
    );
}

#[wasm_bindgen_test]
fn test_compile_package_js_unsupported_feature() {
    reset_filesystem(0);
    write_module(
        0,
        "one",
        r#"
fn wibble() { <<0:16-native>> }
pub fn main() { wibble() }
"#,
    );

    assert!(
        compile_package(0, "javascript")
            .unwrap_err()
            .contains("The javascript target does not support")
    );
}

#[wasm_bindgen_test]
fn test_warnings() {
    reset_filesystem(0);
    write_module(0, "one", "const x = 1");
    assert!(pop_warning(0).is_none());

    assert!(compile_package(0, "javascript").is_ok());
    assert!(pop_warning(0).is_some());
    assert!(pop_warning(0).is_none());
}
