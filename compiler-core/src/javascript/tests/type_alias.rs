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
