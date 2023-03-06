use crate::assert_infer;

#[test]
fn empty_list() {
    assert_infer!("let assert [] = [] 1", "Int");
}

#[test]
fn list_one() {
    assert_infer!("let assert [a] = [1] a", "Int");
}

#[test]
fn list_two() {
    assert_infer!("let assert [a, 2] = [1] a", "Int");
}

#[test]
fn list_spread() {
    assert_infer!("let assert [a, ..] = [1] a", "Int");
}

#[test]
fn list_spread_discard() {
    assert_infer!("let assert [a, .._] = [1] a", "Int");
}

#[test]
fn list_spread_discard_comma_after() {
    assert_infer!("let assert [a, .._,] = [1] a", "Int");
}

#[test]
fn in_fn() {
    assert_infer!("fn(x) { let assert [a] = x a }", "fn(List(a)) -> a");
}

#[test]
fn in_fn_list_int() {
    assert_infer!("fn(x) { let assert [a] = x a + 1 }", "fn(List(Int)) -> Int");
}

#[test]
fn discard_named() {
    assert_infer!("let assert _x = 1 2.0", "Float");
}

#[test]
fn discard() {
    assert_infer!("let assert _ = 1 2.0", "Float");
}

#[test]
fn tuple() {
    assert_infer!("let assert #(tag, x) = #(1.0, 1) x", "Int");
}

#[test]
fn tuple_in_fn() {
    assert_infer!("fn(x) { let assert #(a, b) = x a }", "fn(#(a, b)) -> a");
}

#[test]
fn annotation() {
    assert_infer!("let assert 5: Int = 5 5", "Int");
}

#[test]
fn new_syntax() {
    assert_infer!("let assert Ok(x) = Error(1)", "Result(a, Int)");
}

#[test]
fn expression() {
    assert_infer!("let assert x = 1", "Int");
}

#[test]
fn expression1() {
    assert_infer!("let assert x = let assert x = 1", "Int");
}

#[test]
fn expression2() {
    assert_infer!("let assert x = { let assert x = 1. }", "Float");
}

#[test]
fn expression3() {
    assert_infer!("let assert 1 = 1", "Int");
}
