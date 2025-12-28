use crate::assert_source_map;

#[test]
fn sourcemap_function_definition() {
    assert_source_map!(
        "pub fn add_2(x) {
  x + 2
}"
    )
}

#[test]
fn sourcemap_custom_type_definition() {
    assert_source_map!(
        "pub type Wibble {
  Wibble
  Wobble(field: Int)
  Wabble(Wibble)
}"
    )
}

#[test]
fn sourcemap_import_module() {
    assert_source_map!(
        ("rocket_ship", r#"pub fn launch() { 1 }"#),
        r#"import rocket_ship.{launch}
pub fn go() { launch() }
"#,
    )

}
