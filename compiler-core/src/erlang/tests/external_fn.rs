use crate::assert_erl;

#[test]
fn integration_test1_3() {
    assert_erl!(r#"pub external fn run() -> Int = "Elixir.MyApp" "run""#);
}

#[test]
fn integration_test7() {
    assert_erl!(
        r#"pub external fn receive() -> Int = "try" "and"
                    pub fn catch(x) { receive() }"#
    );
}

#[test]
fn integration_test14() {
    // Private external function calls are inlined
    assert_erl!(
        r#"external fn go(x: Int, y: Int) -> Int = "m" "f"
pub fn x() { go(x: 1, y: 2) go(y: 3, x: 4) }"#
    );
}

#[test]
fn integration_test4() {
    // Public external function calls are inlined but the wrapper function is
    // also printed in the erlang output and exported
    assert_erl!(
        r#"pub external fn go(x: Int, y: Int) -> Int = "m" "f"
                    fn x() { go(x: 1, y: 2) go(y: 3, x: 4) }"#
    );
}

#[test]
fn integration_test15() {
    // Private external function references are inlined
    assert_erl!(
        r#"external fn go(x: Int, y: Int) -> Int = "m" "f"
pub fn x() { go }"#
    );
}
