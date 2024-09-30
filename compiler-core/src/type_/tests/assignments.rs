use crate::assert_infer;

#[test]
fn let_() {
    assert_infer!("let x = 1 2", "Int");
}

#[test]
fn let_1() {
    assert_infer!("let x = 1 x", "Int");
}

#[test]
fn let_2() {
    assert_infer!("let x = 2.0 x", "Float");
}

#[test]
fn let_3() {
    assert_infer!("let x = 2 let y = x y", "Int");
}

#[test]
fn let_4() {
    assert_infer!(
        "let #(#(_, _) as x, _) = #(#(0, 1.0), []) x",
        "#(Int, Float)"
    );
}

#[test]
fn let_5() {
    assert_infer!("let x: String = \"\" x", "String");
}

#[test]
fn let_6() {
    assert_infer!("let x: #(Int, Int) = #(5, 5) x", "#(Int, Int)",);
}

#[test]
fn let_7() {
    assert_infer!("let x: #(Int, Float) = #(5, 5.0) x", "#(Int, Float)",);
}

#[test]
fn let_8() {
    assert_infer!("let assert [1, 2, ..x]: List(Int) = [1,2,3] x", "List(Int)",);
}

#[test]
fn let_9() {
    assert_infer!(
        "let assert #(5, [..x]): #(Int, List(Int)) = #(5, [1,2,3]) x",
        "List(Int)",
    );
}

#[test]
fn let_10() {
    assert_infer!(
        "let assert #(5.0, [..x]): #(Float, List(Int)) = #(5.0, [1,2,3]) x",
        "List(Int)",
    );
}

#[test]
fn let_11() {
    assert_infer!("let x: List(_) = [] x", "List(a)");
}

#[test]
fn let_12() {
    assert_infer!("let x: List(_) = [1] x", "List(Int)");
}

#[test]
fn let_13() {
    assert_infer!("let assert [a] = [1] a", "Int");
}

#[test]
fn let_14() {
    assert_infer!("let assert [a, 2] = [1] a", "Int");
}

#[test]
fn let_15() {
    assert_infer!("let assert [a, .. b] = [1] a", "Int");
}

#[test]
fn let_16() {
    assert_infer!("let assert [a, .. _] = [1] a", "Int");
}

#[test]
fn let_17() {
    assert_infer!("fn(x) { let assert [a] = x a }", "fn(List(a)) -> a");
}

#[test]
fn let_18() {
    assert_infer!("fn(x) { let assert [a] = x a + 1 }", "fn(List(Int)) -> Int");
}

#[test]
fn let_19() {
    assert_infer!("let _x = 1 2.0", "Float");
}

#[test]
fn let_20() {
    assert_infer!("let _ = 1 2.0", "Float");
}

#[test]
fn let_21() {
    assert_infer!("let #(tag, x) = #(1.0, 1) x", "Int");
}

#[test]
fn let_22() {
    assert_infer!("fn(x) { let #(a, b) = x a }", "fn(#(a, b)) -> a");
}

#[test]
fn let_23() {
    assert_infer!("let assert [] = [] 1", "Int");
}

#[test]
fn let_24() {
    assert_infer!("let assert Ok(..) = Ok(10)", "Result(Int, a)");
}

#[test]
fn let_25() {
    assert_infer!("let assert \"hello\" as a <> _ = \"\" a", "String");
}

// // https://github.com/gleam-lang/gleam/issues/1991
// #[test]
// fn block() {
//     assert_infer!("let x = 1 { let x = 1.0 } x", "Int");
// }
