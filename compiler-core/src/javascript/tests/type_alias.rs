use crate::assert_ts_def;

#[test]
fn type_alias() {
    assert_ts_def!(
        r#"
pub type Headers = List(#(String, String))
"#,
    );
}
