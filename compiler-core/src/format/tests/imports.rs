use crate::assert_format;

#[test]
fn types_and_values() {
    assert_format!(
        "import one/two.{type Abc, type Bcd, Abc, Bcd, abc, bcd}
"
    );
}
