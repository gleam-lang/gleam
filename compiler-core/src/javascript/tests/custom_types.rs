// TODO: snapshots for tests that use another module

use crate::assert_js;
use crate::javascript::tests::CURRENT_PACKAGE;

#[test]
fn zero_arity_literal() {
    assert_js!(
        r#"
type Mine {
    This
    ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant
}

fn go() {
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
type Mine {
    This
    ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant
}

const this = This;
const that = ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant;
"#,
    );
}

#[test]
fn zero_arity_imported() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two }"#
        ),
        r#"import other
pub fn main() {
  other.Two
}"#,
    );
}

#[test]
fn zero_arity_imported_unqualified() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two }"#
        ),
        r#"import other.{Two}
pub fn main() {
  Two
}"#,
    );
}

#[test]
fn zero_arity_imported_unqualified_aliased() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two }"#
        ),
        r#"import other.{Two as Three}
pub fn main() {
  Three
}"#
    );
}

#[test]
fn const_zero_arity_imported() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two }"#
        ),
        r#"import other
const x = other.Two
"#,
    );
}

#[test]
fn const_zero_arity_imported_unqualified() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two }"#
        ),
        r#"import other.{Two}
const a = Two
"#,
    );
}

// TODO
// #[test]
// fn const_zero_arity_imported_unqualified_aliased() {
//     assert_js!(
//         ( CURRENT_PACKAGE, vec!["other".to_string()], r#"pub type One { Two }"#),
//         r#"import other.{Two as Three}
// const a = Three
// "#,
//         r#"// const a = { type: "Two" };

// import * as Other from "../other.js";
// const { Two as Three } = other;
// "#
//     );
// }

#[test]
fn const_with_fields() {
    assert_js!(
        r#"
type Mine {
  Mine(a: Int, b: Int)
}

const labels = Mine(b: 2, a: 1)
const no_labels = Mine(3, 4)
"#,
    );
}

#[test]
fn unnamed_fields() {
    assert_js!(
        r#"
type Ip{
    Ip(String)
}

const local = Ip("0.0.0.0")

fn build(x) {
    x("1.2.3.4")
}

fn go() {
    build(Ip)
    Ip("5.6.7.8")
}

fn destructure(x) {
  let Ip(raw) = x
  raw
}
"#,
    );
}

#[test]
fn long_name_variant_without_labels() {
    assert_js!(
        r#"
type TypeWithALongNameAndSeveralArguments{
  TypeWithALongNameAndSeveralArguments(String, String, String, String, String)
}


fn go() {
  TypeWithALongNameAndSeveralArguments
}
"#,
    );
}

#[test]
fn custom_type_with_named_fields() {
    assert_js!(
        r#"
type Cat {
  Cat(name: String, cuteness: Int)
}

type Box {
  Box(occupant: Cat)
}

const felix = Cat("Felix", 12)
const tom = Cat(cuteness: 1, name: "Tom")

fn go() {
  Cat("Nubi", 1)
  Cat(2, name: "Nubi")
  Cat(cuteness: 3, name: "Nubi")
}

fn update(cat) {
  Cat(..cat, name: "Sid")
  Cat(..cat, name: "Bartholemew Wonder Puss the Fourth !!!!!!!!!!!!!!!!")
  Cat(..new_cat(), name: "Molly")
  let box = Box(occupant: cat)
  Cat(..box.occupant, cuteness: box.occupant.cuteness + 1)
}

fn access(cat: Cat) {
  cat.cuteness
}

fn new_cat() {
  Cat(name: "Beau", cuteness: 11)
}
"#,
    );
}

#[test]
fn destructure_custom_type_with_named_fields() {
    assert_js!(
        r#"
type Cat {
  Cat(name: String, cuteness: Int)
}

fn go(cat) {
  let Cat(x, y) = cat
  let Cat(name: x, ..) = cat
  let Cat(cuteness: 4, name: x) = cat
  x
}

"#,
    )
}

#[test]
fn nested_pattern_with_labels() {
    assert_js!(
        r#"pub type Box(x) { Box(a: Int, b: x) }
fn go(x) {
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
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(Int) }"#
        ),
        r#"import other
pub fn main() {
  other.Two(1)
}"#,
    );
}

#[test]
fn imported_ignoring_label() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(field: Int) }"#
        ),
        r#"import other
pub fn main() {
  other.Two(1)
}"#,
    );
}

#[test]
fn imported_using_label() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(field: Int) }"#
        ),
        r#"import other
pub fn main() {
  other.Two(field: 1)
}"#,
    );
}

#[test]
fn imported_multiple_fields() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(a: Int, b: Int, c: Int) }"#
        ),
        r#"import other
pub fn main() {
  other.Two(b: 2, c: 3, a: 1)
}"#,
    );
}

#[test]
fn unqualified_imported_no_label() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(Int) }"#
        ),
        r#"import other.{Two}
pub fn main() {
  Two(1)
}"#,
    );
}

#[test]
fn unqualified_imported_ignoring_label() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(field: Int) }"#
        ),
        r#"import other.{Two}
pub fn main() {
  Two(1)
}"#,
    );
}

#[test]
fn unqualified_imported_using_label() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(field: Int) }"#
        ),
        r#"import other.{Two}
pub fn main() {
  Two(field: 1)
}"#,
    );
}

#[test]
fn unqualified_imported_multiple_fields() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(a: Int, b: Int, c: Int) }"#
        ),
        r#"import other.{Two}
pub fn main() {
  Two(b: 2, c: 3, a: 1)
}"#,
    );
}

#[test]
fn constructor_as_value() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(a: Int, b: Int, c: Int) }"#
        ),
        r#"import other
pub fn main() {
  other.Two
}"#,
    );
}

#[test]
fn unqualified_constructor_as_value() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(a: Int, b: Int, c: Int) }"#
        ),
        r#"import other.{Two}
pub fn main() {
  Two
}"#,
    );
}

#[test]
fn const_imported_no_label() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(Int) }"#
        ),
        r#"import other
pub const main = other.Two(1)
"#,
    );
}

#[test]
fn const_imported_ignoring_label() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(field: Int) }"#
        ),
        r#"import other
pub const main = other.Two(1)
"#,
    );
}

#[test]
fn const_imported_using_label() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(field: Int) }"#
        ),
        r#"import other
pub const main = other.Two(field: 1)
"#,
    );
}

#[test]
fn const_imported_multiple_fields() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(a: Int, b: Int, c: Int) }"#
        ),
        r#"import other
pub const main = other.Two(b: 2, c: 3, a: 1)
"#,
    );
}

#[test]
fn const_unqualified_imported_no_label() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(Int) }"#
        ),
        r#"import other.{Two}
pub const main = Two(1)
"#,
    );
}

#[test]
fn const_unqualified_imported_ignoring_label() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(field: Int) }"#
        ),
        r#"import other.{Two}
pub const main = Two(1)
"#,
    );
}

#[test]
fn const_unqualified_imported_using_label() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(field: Int) }"#
        ),
        r#"import other.{Two}
pub const main = Two(field: 1)
"#,
    );
}

#[test]
fn const_unqualified_imported_multiple_fields() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(a: Int, b: Int, c: Int) }"#
        ),
        r#"import other.{Two}
pub const main = Two(b: 2, c: 3, a: 1)
"#,
    );
}

#[test]
fn imported_pattern() {
    assert_js!(
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { Two(a: Int, b: Int, c: Int) }"#
        ),
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
        (
            CURRENT_PACKAGE,
            vec!["other".to_string()],
            r#"pub type One { One }"#
        ),
        r#"import other

pub fn main() {
  other.One
}
"#,
    );
}
