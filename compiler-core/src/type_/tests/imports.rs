use crate::assert_infer_with_module;

use super::*;

// https://github.com/gleam-lang/gleam/issues/1760
#[test]
fn import_value_with_same_name_as_imported_module() {
    assert_infer_with_module!(
        ("other", "pub const other = 1"),
        "
import other.{other}
pub const a = other
",
        vec![("a", "Int")],
    );
}

#[test]
fn imported_constant_record() {
    assert_infer_with_module!(
        ("one/two", "pub type Thing { Thing(Int) }"),
        "
import one/two

pub const a = two.Thing(1)
",
        vec![("a", "Thing")],
    );
}
