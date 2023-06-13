use crate::assert_erl;

#[test]
fn phantom() {
    assert_erl!("pub type Map(k, v)");
}
