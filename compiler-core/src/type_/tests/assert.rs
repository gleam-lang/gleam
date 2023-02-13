use crate::assert_infer;

#[test]
fn empty_list() {
    assert_infer!("assert [] = [] 1", "Int");
}

#[test]
fn list_one() {
    assert_infer!("assert [a] = [1] a", "Int");
}

#[test]
fn list_two() {
    assert_infer!("assert [a, 2] = [1] a", "Int");
}

#[test]
fn list_spread() {
    assert_infer!("assert [a, ..] = [1] a", "Int");
}

#[test]
fn list_spread_discard() {
    assert_infer!("assert [a, .._] = [1] a", "Int");
}

#[test]
fn list_spread_discard_comma_after() {
    assert_infer!("assert [a, .._,] = [1] a", "Int");
}

#[test]
fn in_fn() {
    assert_infer!("fn(x) { assert [a] = x a }", "fn(List(a)) -> a");
}

#[test]
fn in_fn_list_int() {
    assert_infer!("fn(x) { assert [a] = x a + 1 }", "fn(List(Int)) -> Int");
}

#[test]
fn discard_named() {
    assert_infer!("assert _x = 1 2.0", "Float");
}

#[test]
fn discard() {
    assert_infer!("assert _ = 1 2.0", "Float");
}

#[test]
fn tuple() {
    assert_infer!("assert #(tag, x) = #(1.0, 1) x", "Int");
}

#[test]
fn tuple_in_fn() {
    assert_infer!("fn(x) { assert #(a, b) = x a }", "fn(#(a, b)) -> a");
}

#[test]
fn annotation() {
    assert_infer!("assert 5: Int = 5 5", "Int");
}

#[test]
fn new_syntax() {
    assert_infer!("let assert Ok(x) = Error(1)", "Result(a, Int)");
}
