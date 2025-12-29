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
  let wibble = 1
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

#[test]
fn sourcemap_assert() {
    assert_source_map!(
        "
pub fn main() {
  let x = True
  assert x
}
"
    )
}

#[test]
fn sourcemap_let_assert() {
    assert_source_map!(
        r#"
pub fn go(x) {
  let assert #(wibble, wobble) = x
}
"#
    )
}

#[test]
fn sourcemap_case_destructure_assignment_statement() {
    assert_source_map!(
        "
pub type Wibble {
  Wibble(Int, Int)
}

pub fn go(x) {
  case Wibble(1, 2) {
    Wibble(wibble, wobble) -> wibble + wobble
  }
}
"
    )
}