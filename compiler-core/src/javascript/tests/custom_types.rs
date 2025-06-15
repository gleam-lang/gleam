use crate::javascript::tests::CURRENT_PACKAGE;
use crate::{assert_js, assert_ts_def};

#[test]
fn zero_arity_literal() {
    assert_js!(
        r#"
pub type Mine {
    This
    ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant
}

pub fn go() {
    This
    ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant
}
"#,
    );
}

#[test]
fn zero_arity_const() {
    assert_js!(
        r#"
pub type Mine {
    This
    ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant
}

pub const this = This
pub const that = ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant
"#,
    );
}

#[test]
fn zero_arity_imported() {
    assert_js!(
        ("other", r#"pub type One { Two }"#),
        r#"import other
pub fn main() {
  other.Two
}"#,
    );
}

#[test]
fn zero_arity_imported_typscript() {
    assert_ts_def!(
        (CURRENT_PACKAGE, "other", r#"pub type One { Two }"#),
        r#"import other
pub fn main() {
  other.Two
}"#,
    );
}

#[test]
fn zero_arity_imported_unqualified() {
    assert_js!(
        ("other", r#"pub type One { Two }"#),
        r#"import other.{Two}
pub fn main() {
  Two
}"#,
    );
}

#[test]
fn zero_arity_imported_unqualified_typescript() {
    assert_ts_def!(
        (CURRENT_PACKAGE, "other", r#"pub type One { Two }"#),
        r#"import other.{Two}
pub fn main() {
  Two
}"#,
    );
}

#[test]
fn zero_arity_imported_unqualified_aliased() {
    assert_js!(
        ("other", r#"pub type One { Two }"#),
        r#"import other.{Two as Three}
pub fn main() {
  Three
}"#
    );
}

#[test]
fn zero_arity_imported_unqualified_aliased_typescript() {
    assert_ts_def!(
        (CURRENT_PACKAGE, "other", r#"pub type One { Two }"#),
        r#"import other.{Two as Three}
pub fn main() {
  Three
}"#
    );
}

#[test]
fn const_zero_arity_imported() {
    assert_js!(
        ("other", r#"pub type One { Two }"#),
        r#"import other
pub const x = other.Two
"#,
    );
}

#[test]
fn const_zero_arity_imported_unqualified() {
    assert_js!(
        ("other", r#"pub type One { Two }"#),
        r#"import other.{Two}
pub const a = Two
"#,
    );
}

#[test]
fn const_with_fields() {
    assert_js!(
        r#"
pub type Mine {
  Mine(a: Int, b: Int)
}

pub const labels = Mine(b: 2, a: 1)
pub const no_labels = Mine(3, 4)
"#,
    );
}

#[test]
fn const_with_fields_typescript() {
    assert_ts_def!(
        r#"
pub type Mine {
  Mine(a: Int, b: Int)
}

pub const labels = Mine(b: 2, a: 1)
pub const no_labels = Mine(3, 4)
"#,
    );
}

#[test]
fn unnamed_fields() {
    assert_js!(
        r#"
pub type Ip {
    Ip(String)
}

pub const local = Ip("0.0.0.0")

pub fn build(x) {
    x("1.2.3.4")
}

pub fn go() {
    build(Ip)
    Ip("5.6.7.8")
}

pub fn destructure(x) {
  let Ip(raw) = x
  raw
}
"#,
    );
}

#[test]
fn unnamed_fields_typescript() {
    assert_ts_def!(
        r#"
pub type Ip{
    Ip(String)
}

pub const local = Ip("0.0.0.0")

"#,
    );
}

#[test]
fn long_name_variant_without_labels() {
    assert_js!(
        r#"
pub type TypeWithALongNameAndSeveralArguments{
  TypeWithALongNameAndSeveralArguments(String, String, String, String, String)
}


pub fn go() {
  TypeWithALongNameAndSeveralArguments
}
"#,
    );
}

#[test]
fn long_name_variant_mixed_labels_typescript() {
    assert_ts_def!(
        r#"
pub type TypeWithALongNameAndSeveralArguments{
  TypeWithALongNameAndSeveralArguments(String, String, String, a: String, b: String)
}

pub const local = TypeWithALongNameAndSeveralArguments("one", "two", "three", "four", "five")
"#,
    );
}

#[test]
fn custom_type_with_named_fields() {
    assert_js!(
        r#"
pub type Cat {
  Cat(name: String, cuteness: Int)
}

pub type Box {
  Box(occupant: Cat)
}

pub const felix = Cat("Felix", 12)
pub const tom = Cat(cuteness: 1, name: "Tom")

pub fn go() {
  Cat("Nubi", 1)
  Cat(2, name: "Nubi")
  Cat(cuteness: 3, name: "Nubi")
}

pub fn update(cat) {
  Cat(..cat, name: "Sid")
  Cat(..cat, name: "Bartholemew Wonder Puss the Fourth !!!!!!!!!!!!!!!!")
  Cat(..new_cat(), name: "Molly")
  let box = Box(occupant: cat)
  Cat(..box.occupant, cuteness: box.occupant.cuteness + 1)
}

pub fn access(cat: Cat) {
  cat.cuteness
}

pub fn new_cat() {
  Cat(name: "Beau", cuteness: 11)
}
"#,
    );
}

#[test]
fn destructure_custom_type_with_named_fields() {
    assert_js!(
        r#"
pub type Cat {
  Cat(name: String, cuteness: Int)
}

pub fn go(cat) {
  let Cat(x, y) = cat
  let Cat(name: x, ..) = cat
  let assert Cat(cuteness: 4, name: x) = cat
  x
}

"#,
    )
}

#[test]
fn destructure_custom_type_with_mixed_fields_first_unlabelled() {
    assert_js!(
        r#"
pub type Cat {
  Cat(String, cuteness: Int)
}

pub fn go(cat) {
  let Cat(x, y) = cat
  let Cat(cuteness: y, ..) = cat
  let Cat(x, cuteness: y) = cat
  x
}

"#,
    )
}

#[test]
fn nested_pattern_with_labels() {
    assert_js!(
        r#"pub type Box(x) { Box(a: Int, b: x) }
pub fn go(x) {
  case x {
    Box(a: _, b: Box(a: a, b: b)) -> a + b
    _ -> 1
  }
}
"#,
    );
}

#[test]
fn imported_no_label() {
    assert_js!(
        ("other", r#"pub type One { Two(Int) }"#),
        r#"import other
pub fn main() {
  other.Two(1)
}"#,
    );
}

#[test]
fn imported_ignoring_label() {
    assert_js!(
        ("other", r#"pub type One { Two(field: Int) }"#),
        r#"import other
pub fn main() {
  other.Two(1)
}"#,
    );
}

#[test]
fn imported_using_label() {
    assert_js!(
        ("other", r#"pub type One { Two(field: Int) }"#),
        r#"import other
pub fn main() {
  other.Two(field: 1)
}"#,
    );
}

#[test]
fn imported_multiple_fields() {
    assert_js!(
        ("other", r#"pub type One { Two(a: Int, b: Int, c: Int) }"#),
        r#"import other
pub fn main() {
  other.Two(b: 2, c: 3, a: 1)
}"#,
    );
}

#[test]
fn unqualified_imported_no_label() {
    assert_js!(
        ("other", r#"pub type One { Two(Int) }"#),
        r#"import other.{Two}
pub fn main() {
  Two(1)
}"#,
    );
}

#[test]
fn unqualified_imported_no_label_typescript() {
    assert_ts_def!(
        (CURRENT_PACKAGE, "other", r#"pub type One { Two(Int) }"#),
        r#"import other.{Two}
pub fn main() {
  Two(1)
}"#,
    );
}

#[test]
fn unqualified_imported_ignoring_label() {
    assert_js!(
        ("other", r#"pub type One { Two(field: Int) }"#),
        r#"import other.{Two}
pub fn main() {
  Two(1)
}"#,
    );
}

#[test]
fn unqualified_imported_using_label() {
    assert_js!(
        ("other", r#"pub type One { Two(field: Int) }"#),
        r#"import other.{Two}
pub fn main() {
  Two(field: 1)
}"#,
    );
}

#[test]
fn unqualified_imported_multiple_fields() {
    assert_js!(
        ("other", r#"pub type One { Two(a: Int, b: Int, c: Int) }"#),
        r#"import other.{Two}
pub fn main() {
  Two(b: 2, c: 3, a: 1)
}"#,
    );
}

#[test]
fn constructor_as_value() {
    assert_js!(
        ("other", r#"pub type One { Two(a: Int, b: Int, c: Int) }"#),
        r#"import other
pub fn main() {
  other.Two
}"#,
    );
}

#[test]
fn unqualified_constructor_as_value() {
    assert_js!(
        ("other", r#"pub type One { Two(a: Int, b: Int, c: Int) }"#),
        r#"import other.{Two}
pub fn main() {
  Two
}"#,
    );
}

#[test]
fn const_imported_no_label() {
    assert_js!(
        ("other", r#"pub type One { Two(Int) }"#),
        r#"import other
pub const main = other.Two(1)
"#,
    );
}

#[test]
fn const_imported_ignoring_label() {
    assert_js!(
        ("other", r#"pub type One { Two(field: Int) }"#),
        r#"import other
pub const main = other.Two(1)
"#,
    );
}

#[test]
fn const_imported_using_label() {
    assert_js!(
        ("other", r#"pub type One { Two(field: Int) }"#),
        r#"import other
pub const main = other.Two(field: 1)
"#,
    );
}

#[test]
fn const_imported_multiple_fields() {
    assert_js!(
        ("other", r#"pub type One { Two(a: Int, b: Int, c: Int) }"#),
        r#"import other
pub const main = other.Two(b: 2, c: 3, a: 1)
"#,
    );
}

#[test]
fn const_unqualified_imported_no_label() {
    assert_js!(
        ("other", r#"pub type One { Two(Int) }"#),
        r#"import other.{Two}
pub const main = Two(1)
"#,
    );
}

#[test]
fn const_unqualified_imported_ignoring_label() {
    assert_js!(
        ("other", r#"pub type One { Two(field: Int) }"#),
        r#"import other.{Two}
pub const main = Two(1)
"#,
    );
}

#[test]
fn const_unqualified_imported_using_label() {
    assert_js!(
        ("other", r#"pub type One { Two(field: Int) }"#),
        r#"import other.{Two}
pub const main = Two(field: 1)
"#,
    );
}

#[test]
fn const_unqualified_imported_multiple_fields() {
    assert_js!(
        ("other", r#"pub type One { Two(a: Int, b: Int, c: Int) }"#),
        r#"import other.{Two}
pub const main = Two(b: 2, c: 3, a: 1)
"#,
    );
}

#[test]
fn imported_pattern() {
    assert_js!(
        ("other", r#"pub type One { Two(a: Int, b: Int, c: Int) }"#),
        r#"import other.{Two}

pub fn main(x) {
  case x {
    Two(a: 1, ..) -> 1
    other.Two(b: 2, c: c, ..) -> c
    _ -> 3
  }
}
"#,
    );
}

#[test]
fn keyword_label_name() {
    assert_js!(
        r#"pub type Thing {
  Thing(in: Int, class: Nil)
}
"#,
    );
}

#[test]
fn qualified() {
    assert_js!(
        ("other", r#"pub type One { One }"#),
        r#"import other

pub fn main() {
  other.One
}
"#,
    );
}

#[test]
fn unapplied_record_constructors_typescript() {
    assert_ts_def!(
        r#"pub type Cat { Cat(name: String) }

pub fn return_unapplied_cat() {
  Cat
}
"#
    );
}

#[test]
fn opaque_types_typescript() {
    assert_ts_def!(
        r#"pub opaque type Animal {
  Cat(goes_outside: Bool)
  Dog(plays_fetch: Bool)
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1650
#[test]
fn types_must_be_rendered_before_functions() {
    assert_js!(
        r#"
pub fn one() { One }
pub type One { One }
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2386
#[test]
fn new_type_import_syntax() {
    assert_js!(
        ("package", "a", r#"pub type A { A }"#),
        r#"
import a.{type A, A}

pub fn main() {
  A
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3813
#[test]
fn record_with_field_named_constructor() {
    assert_js!(
        r#"
pub type Thing {
  Thing(constructor: Nil)
}

pub fn main() {
  let a = Thing(constructor: Nil)
  let b = Thing(..a, constructor: Nil)
  b.constructor
}
"#
    );
}

#[test]
fn record_with_field_named_then() {
    assert_js!(
        r#"
pub type Thing {
  Thing(then: Nil)
}

pub fn main() {
  let a = Thing(then: Nil)
  let b = Thing(..a, then: Nil)
  b.then
}
"#
    );
}
#[test]
fn record_access_in_guard_with_reserved_field_name() {
    assert_js!(
        r#"
pub type Thing {
  Thing(constructor: Nil)
}

pub fn main() {
  let a = Thing(constructor: Nil)
  case Nil {
      Nil if a.constructor == Nil -> a.constructor
      _ -> Nil
  }
}
"#
    );
}

#[test]
fn record_access_in_pattern_with_reserved_field_name() {
    assert_js!(
        r#"
pub type Thing {
  Thing(constructor: Nil)
}

pub fn main() {
  let a = Thing(constructor: Nil)
  let Thing(constructor: ctor) = a
  case a {
      a if a.constructor == ctor -> Nil
      Thing(constructor:) if ctor == constructor -> Nil
      _ -> Nil
  }
}
"#
    );
}

#[test]
fn constructors_get_their_own_jsdoc() {
    assert_js!(
        r#"
pub type Wibble {
  /// Wibbling!!
  Wibble(field: Int)

  /// Wobbling!!
  Wobble(field: Int)
}
"#
    );
}
