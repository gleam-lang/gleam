use crate::{assert_module_error, assert_module_infer, assert_warning};

// https://github.com/gleam-lang/gleam/issues/2215
#[test]
fn generic_phantom() {
    assert_module_infer!(
        r#"
pub type Test(a) {
  MakeTest(field: Test(Int))
}
"#,
        vec![("MakeTest", "fn(Test(Int)) -> Test(a)")]
    );
}

#[test]
fn deprecated_type() {
    assert_warning!(
        r#"
@deprecated("Dont use this!")
pub type Cat {
  Cat(name: String, cuteness: Int)
}

pub fn name() -> String {
  let c = Cat("Numi", 20)
  c.name
}
        "#
    );
}

#[test]
fn deprecated_all_varients_type() {
    assert_module_error!(
        r#"
pub type Numbers {
  @deprecated("1")
  One
  @deprecated("2")
  Two
}
"#
    );
}

#[test]
fn deprecated_varients_type() {
    assert_warning!(
        r#"
pub type Numbers {
  @deprecated("1")
  One
  Two
}

pub fn num() {
  let _one = One
  let _two = Two
  Nil
}
"#
    );
}

#[test]
fn depreacted_type_deprecate_varient_err() {
    assert_module_error!(
        r#"
@deprecated("2")
pub type Numbers {
  @deprecated("1")
  One
  Two
}

pub fn num() {
  let _two = Two
  Nil
}
"#
    );
}

#[test]
fn fault_tolerance() {
    // An error in a custom type does not stop analysis
    assert_module_error!(
        r#"
pub type Cat {
  Cat(UnknownType)
}

pub type Kitten = AnotherUnknownType
        "#
    );
}

#[test]
fn duplicate_variable_error_does_not_stop_analysis() {
    // Both these aliases have errors! We do not stop on the first one.
    assert_module_error!(
        r#"
type Two(a, a) {
  Two(a, a)
}

type Three(a, a) {
  Three
}
"#
    );
}

#[test]
fn conflict_with_import() {
    // We cannot declare a type with the same name as an imported type
    assert_module_error!(
        ("wibble", "pub type A { B }"),
        "import wibble.{type A} type A { C }",
    );
}

#[test]
fn generic_record_update1() {
    // A record update on polymorphic types with a field of different type
    assert_module_infer!(
        "
pub type Box(a) {
  Box(value: a, i: Int)
}

pub fn update_box(box: Box(Int), value: String) {
  Box(..box, value: value)
}",
        vec![
            ("Box", "fn(a, Int) -> Box(a)"),
            ("update_box", "fn(Box(Int), String) -> Box(String)")
        ]
    );
}

#[test]
fn generic_record_update2() {
    // A record update on polymorphic types with generic fields of a different type
    assert_module_infer!(
        "
pub type Box(a) {
  Box(value: a, i: Int)
}
pub fn update_box(box: Box(a), value: b) {
  Box(..box, value: value)
}",
        vec![
            ("Box", "fn(a, Int) -> Box(a)"),
            ("update_box", "fn(Box(a), b) -> Box(b)")
        ]
    );
}

#[test]
fn inferred_variant_record_update_change_type_parameter() {
    assert_module_infer!(
        r#"
pub type Box(a) {
  Locked(password: String, value: a)
  Unlocked(password: String, value: a)
}

pub fn main() {
  let box = Locked("ungu€$$4bLe", 11)
  case box {
    Locked(..) as box -> Locked(..box, value: True)
    Unlocked(..) as box -> Unlocked(..box, value: False)
  }
}
"#,
        vec![
            ("Locked", "fn(String, a) -> Box(a)"),
            ("Unlocked", "fn(String, a) -> Box(a)"),
            ("main", "fn() -> Box(Bool)")
        ]
    );
}

#[test]
fn pattern_match_correct_labeled_field() {
    assert_module_error!(
        r#"
type Fish {
  Starfish()
  Jellyfish(name: String, jiggly: Bool)
}

fn handle_fish(fish: Fish) {
  case fish {
    Starfish() -> False
    Jellyfish(jiggly:) -> jiggly  // <- error is here
  }
}
"#
    );
}

#[test]
fn pattern_match_correct_pos_field() {
    assert_module_error!(
        r#"
type Fish {
  Starfish()
  Jellyfish(String, Bool)
}

fn handle_fish(fish: Fish) {
  case fish {
    Starfish() -> False
    Jellyfish(jiggly) -> jiggly
  }
}
"#
    );
}
