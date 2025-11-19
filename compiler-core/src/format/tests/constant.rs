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
