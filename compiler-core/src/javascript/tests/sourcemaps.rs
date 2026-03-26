use crate::assert_source_map;

#[test]
fn sourcemap_function_definition() {
    assert_source_map!(
        "
/// my add function
pub fn add_2(x) {
  x + 2
}"
    )
}

#[test]
fn sourcemap_function_definitions_with_unused() {
    assert_source_map!(
        "
/// my add function
pub fn add_2(x) {
  x + 2
}

fn unused(x) {
  x + 1
}

pub fn add_3(x) {
  x + 3
}
"
    )
}

#[test]
fn sourcemap_function_definition_with_variable_assignment() {
    assert_source_map!(
        "
/// my function
pub fn wibble() {
  let wibble = 1
  wibble + 2
}"
    )
}

#[test]
fn sourcemap_function_definition_with_string_with_newline_escaped() {
    assert_source_map!(
        "
/// my function
pub fn wibble() {
  let wibble = \"hello\\nworld\"
  wibble <> \"!\"
}"
    )
}

#[test]
fn sourcemap_function_definition_with_string_with_newline() {
    assert_source_map!(
        "
/// my function
pub fn wibble() {
  let wibble = \"hello
world\"
  wibble <> \"!\"
}"
    )
}

#[test]
fn sourcemap_custom_type_definition() {
    assert_source_map!(
        "
/// my custom type
pub type Wibble {
  /// Wibble
  Wibble
  /// Wobble
  Wobble(field: Int)
  /// Wabble
  Wabble(Wibble)
}"
    )
}

#[test]
fn sourcemap_custom_type_definition_with_unused() {
    assert_source_map!(
        "
/// my custom type
pub type Wibble {
  /// Wibble
  Wibble
}

type Unused {
  Unused
}

pub type Wobble {
  Wobble(Wibble)
}
"
    )
}

#[test]
fn sourcemap_module_constant() {
    assert_source_map!(
        "
/// my constant
pub const wibble = 1
/// wobble
const wobble = 2
/// wabble
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

#[test]
fn sourcemap_with_complex_case_expression() {
    assert_source_map!(
        "
pub fn go(one, other) {
  case one, other {
    Ok(1), Error(_) -> 1
    Ok(_), _ -> 2
    Error(_), Ok(2) -> 3
    _, _ -> 4
  }
}
"
    )
}

#[test]
fn sourcemap_pipe() {
    assert_source_map!(
        "
fn add_2(x) {
  x + 2
}

pub fn go(x) {
  x |> add_2
}
"
    )
}

#[test]
fn sourcemap_use() {
    assert_source_map!(
        "
fn add_3_to_result(i, f) {
    f(i) + 3
}

pub fn go(x) {
  use a <- add_3_to_result(1)
  a + 2
}
"
    )
}

#[test]
fn sourcemap_with_list() {
    assert_source_map!(
        "
pub fn go(x) {
  [1, 2, 3]
}
"
    )
}

#[test]
fn sourcemap_with_tuple() {
    assert_source_map!(
        "
pub fn go(x) {
  #(1, 2, 3)
}
"
    )
}

#[test]
fn sourcemap_with_bitarray() {
    assert_source_map!(
        "
pub fn go(x) {
  <<256:int>>
}
"
    )
}

#[test]
fn sourcemap_with_tail_recursive_functions() {
    assert_source_map!(
        "
pub fn wibble(lon, acc) {
  case lon {
    [] -> 0
    [n, ..rest] -> wibble(rest, acc + n)
  }
}
"
    )
}
