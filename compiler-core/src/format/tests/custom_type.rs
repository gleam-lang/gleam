use crate::assert_format;

#[test]
fn custom_type_0() {
    assert_format!(
        "type WowThisTypeHasJustTheLongestName(
  some_long_type_variable,
  and_another,
  and_another_again,
) {
  Make
}
"
    );
}
#[test]
fn custom_type_1() {
    assert_format!(
        "type Result(a, e) {
  Ok(a)
  Error(e)
}
"
    );
}
#[test]
fn custom_type_2() {
    assert_format!(
        "type Result(a, e) {
  Ok(value: a)
  Error(error: e)
}
"
    );
}
#[test]
fn custom_type_3() {
    assert_format!(
        "type SillyResult(a, e) {
  Ok(
    first_value_with_really_long_name: a,
    second_value_with_really_long_name: a,
  )
  Error(error: e)
}
"
    );
}
#[test]
fn custom_type_4() {
    assert_format!(
        "type SillyResult(a, e) {
  Ok(
    first_value_with_really_long_name: a,
    second_value_with_really_long_name: List(
      #(Int, fn(a, a, a, a, a, a, a) -> List(a)),
    ),
  )
  Error(error: e)
}
"
    );
}
#[test]
fn custom_type_5() {
    assert_format!(
        "type X {
  X(
    start: fn() -> a_reall_really_long_name_goes_here,
    stop: fn() -> a_reall_really_long_name_goes_here,
  )
}
"
    );
}
#[test]
fn custom_type_6() {
    assert_format!(
        "pub opaque type X {
  X
}
"
    );
}
#[test]
fn custom_type_7() {
    assert_format!(
        "///
pub type Option(a) {
  None
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/1757
#[test]
fn multiple_line_custom_type_constructor_field_doc_comments() {
    assert_format!(
        r#"pub type Thingy {
  Thingy(
    /// One?
    /// One!
    one: One,
    /// Two?
    /// Two!
    two: Two,
  )
}
"#
    );
}

#[test]
fn deprecated_custom_type() {
    assert_format!(
        r#"@deprecated("Deprecated type")
pub type One {
  One
}
"#
    );
}

#[test]
fn doc_comments_7_test() {
    assert_format!(
        r#"import one

/// one
///two
type Whatever {
  Whatever
}
"#
    );
}

#[test]
fn comments1() {
    assert_format!(
        r#"import one

// one
//two
type Whatever {
  Whatever
}
"#
    );
}

#[test]
fn comments2() {
    assert_format!(
        r#"import one

// one
//two
/// three
type Whatever {
  Whatever
}
"#
    );
}

#[test]
fn comments6() {
    assert_format!(
        r#"// one
//two
type Thingy
"#
    );
}

#[test]
fn comments7() {
    assert_format!(
        r#"// one
//two
type Thingy
"#
    );
}

#[test]
fn comments8() {
    assert_format!(
        r#"// one
//two
type Whatever {
  Whatever
}
"#
    );
}

#[test]
fn comments10() {
    assert_format!(
        r#"// zero
import one

// one
//two
type Whatever {
  Whatever
}
"#
    );
}

#[test]
fn comments11() {
    assert_format!(
        "fn main() {
  // Hello
  \"world\"
}
"
    );
}

#[test]
fn comment21() {
    assert_format!(
        "pub type Spec {
  Spec(
    // Hello
    hello: Int,
    // World
    world: Int,
  )
}
"
    );
}

#[test]
fn commented_constructors() {
    assert_format!(
        "pub type Number {
  // 1
  One
  // 2
  Two
  // 3
  Three
  // ???
  More
}
"
    );
}

#[test]
fn commented_constructors1() {
    assert_format!(
        "pub type Number {
  /// 1
  One
  /// 2
  Two
  /// 3
  Three
  /// ???
  More
}
"
    );
}

#[test]
fn commented_constructors2() {
    assert_format!(
        "pub type Number {
  // a
  /// 1
  One
  // b
  /// 2
  Two
  // c
  /// 3
  Three
  // defg
  /// ???
  More
}
"
    );
}

#[test]
fn commented_constructors3() {
    assert_format!(
        "pub type Number {
  /// 1
  One(value: Int)
  /// > 1
  Many(value: Int)
}
"
    );
}

#[test]
fn deprecated_variant_1() {
    assert_format!(
        r#"pub type One {
  @deprecated("Deprecated type")
  One
}
"#
    );
}

#[test]
fn deprecated_variant_2() {
    assert_format!(
        r#"pub type One {
  @deprecated("Deprecated type")
  One(Int, Int, Int, Int, Int, Int, Int)
}
"#
    );
}

#[test]
fn deprecated_variant_3() {
    assert_format!(
        r#"pub type One {
  @deprecated("Deprecated type with a very long message")
  One(Int, Int, Int, Int, Int, Int, Int)
}
"#
    );
}

#[test]
fn deprecated_variant_4() {
    assert_format!(
        r#"pub type One {
  @deprecated("Deprecated type with a very long message

It even has multiple lines!
")
  One(Int, Int, Int, Int, Int, Int, Int)
}
"#
    );
}

#[test]
fn external_custom_type() {
    assert_format!(
        r#"@external(erlang, "erlang", "map")
@external(javascript, "../dict.d.mts", "Dict")
pub type Dict(key, value)
"#
    );
}
