use crate::assert_format;

// https://github.com/gleam-lang/gleam/issues/5143
#[test]
pub fn constant_with_deprecated_attribute() {
    assert_format!(
        r#"@deprecated("Use tau instead")
pub const pi = 3.14
"#
    );
}

#[test]
fn const_record_update_simple() {
    assert_format!(
        r#"pub type Counter {
  Counter(a: Int, b: Int)
}

pub const c = Counter(0, 0)

pub const c2 = Counter(..c, a: 1, b: 2)
"#
    );
}

#[test]
fn const_record_update_long() {
    assert_format!(
        r#"pub type Counter {
  Counter(loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong: Int)
}

pub const c = Counter(0)

pub const c2 = Counter(
  ..c,
  loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong: 1,
)
"#
    );
}

#[test]
fn const_record_update_with_module() {
    assert_format!(
        r#"pub type Counter {
  Counter(a: Int, b: Int)
}

pub const c = Counter(0, 0)

pub const c2 = mod.Counter(..c, a: 1)
"#
    );
}
