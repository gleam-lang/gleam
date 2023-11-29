use super::*;
use wasm_bindgen_test::wasm_bindgen_test;

#[wasm_bindgen_test]
fn test_reset_filesystem() {
    reset_filesystem();
    assert_eq!(read_file_bytes("hello"), None);
    write_file_bytes("hello", vec![1, 2, 3].as_slice());
    assert_eq!(read_file_bytes("hello"), Some(vec![1, 2, 3]));
    reset_filesystem();
    assert_eq!(read_file_bytes("hello"), None);
}

#[wasm_bindgen_test]
fn test_write_module() {
    reset_filesystem();
    assert_eq!(read_file_bytes("/src/some/module.gleam"), None);
    write_module("some/module", "const x = 1");
    assert_eq!(
        read_file_bytes("/src/some/module.gleam"),
        Some(vec![99, 111, 110, 115, 116, 32, 120, 32, 61, 32, 49]),
    );
    reset_filesystem();
    assert_eq!(read_file_bytes("/src/some/module.gleam"), None);
}

#[wasm_bindgen_test]
fn test_compile_package_bad_target() {
    reset_filesystem();
    assert!(compile_package("ruby").is_err());
}

#[wasm_bindgen_test]
fn test_compile_package_empty() {
    reset_filesystem();
    assert!(compile_package("javascript").is_ok());
}

#[wasm_bindgen_test]
fn test_compile_package_js() {
    reset_filesystem();
    write_module("one/two", "pub const x = 1");
    write_module("up/down", "import one/two pub fn go() { two.x }");
    assert!(compile_package("javascript").is_ok());

    assert_eq!(
        read_compiled_javascript("one/two"),
        Some("export const x = 1;\n".into())
    );

    assert_eq!(
        read_compiled_javascript("up/down"),
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
    write_module("up/down", "import one/two/three");
    assert!(compile_package("javascript").is_err());

    // Let's fix that.
    write_module("up/down", "pub const y = 1");
    assert!(compile_package("javascript").is_ok());
    assert_eq!(
        read_compiled_javascript("up/down"),
        Some("export const y = 1;\n".into())
    );
}
