use crate::assert_infer_with_module;

use super::*;

// https://github.com/gleam-lang/gleam/issues/1760
#[test]
fn import_value_with_same_name_as_imported_module() {
    assert_infer_with_module!(
        (vec!["other".to_string()], "pub const other = 1"),
        "
import other.{other}
pub const a = other
",
        vec![("a", "Int")],
    );
}
