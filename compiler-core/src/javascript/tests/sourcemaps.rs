use crate::assert_source_map;

#[test]
fn sourcemap_function_definition() {
    assert_source_map!(
        "
// my add function
pub fn add_2(x) {
  x + 2
}"
    )
}

#[test]
fn sourcemap_function_definition_with_variable_assignment() {
    assert_source_map!(
        "
// my function
pub fn wibble() {
  let wibble = 1;
  wibble + 2
}"
    )
}

#[test]
fn sourcemap_custom_type_definition() {
    assert_source_map!(
        "
// my custom type
pub type Wibble {
  Wibble
  Wobble(field: Int)
  Wabble(Wibble)
}"
    )
}

#[test]
fn sourcemap_module_constant() {
    assert_source_map!(
        "
// my constant
pub const wibble = 1
const wobble = 2
pub const wabble = 3"
    )
}