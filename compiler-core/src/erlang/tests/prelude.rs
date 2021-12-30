use crate::assert_erl;

#[test]
fn qualified_prelude() {
    assert_erl!(
        "import gleam
pub type X { X(gleam.Int) }
"
    );

    assert_erl!(
        "import gleam
pub fn x() { gleam.Ok(1) }
"
    );
}

