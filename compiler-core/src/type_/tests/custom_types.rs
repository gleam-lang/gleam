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
