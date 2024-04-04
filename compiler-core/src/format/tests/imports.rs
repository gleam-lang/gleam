use crate::assert_format;

#[test]
fn types_and_values() {
    assert_format!(
        "import one/two.{type Abc, type Bcd, Abc, Bcd, abc, bcd}
"
    );
}

#[test]
fn discarded_import() {
    assert_format!(
        "import one/two as _three
"
    );
}

#[test]
fn discarded_import_with_unqualified() {
    assert_format!(
        "import one/two.{type Abc, Bcd, abc} as _three
"
    );
}
