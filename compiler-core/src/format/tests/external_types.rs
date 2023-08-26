use crate::assert_format;

#[test]
fn example1() {
    assert_format!("type Private\n");
}

#[test]
fn example2() {
    assert_format!("type Box(a)\n");
}

#[test]
fn example3() {
    assert_format!("type Box(a, b, zero)\n");
}

#[test]
fn example4() {
    assert_format!("pub type Private\n");
}

#[test]
fn example5() {
    assert_format!("pub type Box(a)\n");
}

#[test]
fn example6() {
    assert_format!("pub type Box(a, b, zero)\n");
}
