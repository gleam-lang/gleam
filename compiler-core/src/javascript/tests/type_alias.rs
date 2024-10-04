use crate::assert_ts_def;

#[test]
fn type_alias() {
    assert_ts_def!(
        r#"
pub type Headers = List(#(String, String))
"#,
    );
}

#[test]
fn private_type_in_opaque_type() {
    assert_ts_def!(
        r#"
type PrivateType {
  PrivateType
}

pub opaque type OpaqueType {
  OpaqueType(PrivateType)
}
"#,
    );
}

#[test]
fn import_indirect_type_alias() {
    assert_ts_def!(
        (
            "wibble",
            "wibble",
            r#"
pub type Wibble {
  Wibble(Int)
}
"#
        ),
        (
            "wobble",
            "wobble",
            r#"
import wibble
pub type Wobble = wibble.Wibble
"#
        ),
        r#"
import wobble

pub fn main(x: wobble.Wobble) {
  Nil
}
"#,
    );
}
