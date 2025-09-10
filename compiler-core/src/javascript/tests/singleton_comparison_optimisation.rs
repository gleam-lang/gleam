use crate::assert_js;

#[test]
fn singleton_record_equality() {
    assert_js!(
        r#"
pub type Wibble {
  Wibble
  Wobble
}

pub fn is_wibble(w: Wibble) -> Bool {
  w == Wibble
}
"#,
    );
}

#[test]
fn singleton_record_inequality() {
    assert_js!(
        r#"
pub type Wibble {
  Wibble
  Wobble
}

pub fn is_not_wibble(w: Wibble) -> Bool {
  w != Wibble
}
"#,
    );
}

#[test]
fn singleton_record_reverse_order() {
    assert_js!(
        r#"
pub type Wibble {
  Wibble
  Wobble
}

pub fn is_wibble_reverse(w: Wibble) -> Bool {
  Wibble == w
}
"#,
    );
}

#[test]
fn non_singleton_record_equality() {
    assert_js!(
        r#"
pub type Person {
  Person(name: String, age: Int)
}

pub fn same_person(p1: Person, p2: Person) -> Bool {
  p1 == p2
}
"#,
    );
}

#[test]
fn multiple_singleton_constructors() {
    assert_js!(
        r#"
pub type Status {
  Loading
  Success
  Error
}

pub fn is_loading(s: Status) -> Bool {
  s == Loading
}

pub fn is_success(s: Status) -> Bool {
  s == Success
}
"#,
    );
}

#[test]
fn mixed_singleton_and_non_singleton() {
    assert_js!(
        r#"
pub type Result {
  Ok(value: Int)
  Error
}

pub fn is_error(r: Result) -> Bool {
  r == Error
}
"#,
    );
}

#[test]
fn singleton_in_case_guard() {
    assert_js!(
        r#"
pub type State {
  Active
  Inactive
}

pub fn process(s: State) -> String {
  case s {
    state if state == Active -> "active"
    _ -> "inactive"
  }
}
"#,
    );
}
