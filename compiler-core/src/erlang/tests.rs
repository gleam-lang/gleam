mod assert;
mod bit_strings;
mod guards;
mod numbers;
mod patterns;
mod pipes;
mod records;
mod reserved;
mod statement_if;
mod todo;
mod try_;
mod variables;

#[macro_export]
macro_rules! assert_erl {
    ($src:expr $(,)?) => {{
        use crate::{
            build::Origin,
            erlang::module,
            line_numbers::LineNumbers,
            type_::{build_prelude, infer_module},
            uid::UniqueIdGenerator,
        };
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["the_app".to_string()];
        let mut modules = im::HashMap::new();
        let ids = UniqueIdGenerator::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), build_prelude(&ids));
        let ast = infer_module(
            crate::build::Target::Erlang,
            &ids,
            ast,
            Origin::Src,
            "thepackage",
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let mut output = String::new();
        let line_numbers = LineNumbers::new($src);
        module(&ast, &line_numbers, &mut output).unwrap();
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};
}

#[test]
fn integration_test() {
    assert_erl!(
        r#"pub fn go() {
let x = #(100000000000000000, #(2000000000, 3000000000000, 40000000000), 50000, 6000000000)
  x
}"#
    );

    assert_erl!(
        r#"pub fn go() {
  let y = 1
  let y = 2
  y
}"#
    );

    // hex, octal, and binary literals
    assert_erl!(
        r#"pub fn go() {
    let fifteen = 0xF
    let nine = 0o11
    let ten = 0b1010
  fifteen
}"#
    );

    assert_erl!(
        r#"pub fn go() {
  let y = 1
  let y = 2
  y
}"#
    );
}

#[test]
fn integration_test1() {
    assert_erl!(r#"pub fn t() { True }"#);

    assert_erl!(
        r#"pub type Money { Pound(Int) }
                    fn pound(x) { Pound(x) }"#
    );

    assert_erl!(r#"pub fn loop() { loop() }"#);

    assert_erl!(r#"pub external fn run() -> Int = "Elixir.MyApp" "run""#);

    assert_erl!(
        r#"fn inc(x) { x + 1 }
                    pub fn go() { 1 |> inc |> inc |> inc }"#
    );

    assert_erl!(
        r#"fn add(x, y) { x + y }
                    pub fn go() { 1 |> add(_, 1) |> add(2, _) |> add(_, 3) }"#
    );

    assert_erl!(
        r#"pub fn and(x, y) { x && y }
pub fn or(x, y) { x || y }
pub fn modulo(x, y) { x % y }
pub fn fdiv(x, y) { x /. y }
            "#
    );
}

#[test]
fn integration_test2() {
    assert_erl!(
        r#"pub fn second(list) { case list { [x, y] -> y z -> 1 } }
pub fn tail(list) { case list { [x, ..xs] -> xs z -> list } }
            "#
    );

    assert_erl!("pub fn tail(list) { case list { [x, ..] -> x } }");

    assert_erl!(r#"pub fn x() { let x = 1 let x = x + 1 x }"#);

    assert_erl!(
        r#"pub external fn receive() -> Int = "try" "and"
                    pub fn catch(x) { receive() }"#
    );

    // Translation of Float-specific BinOp into variable-type Erlang term comparison.
    assert_erl!(r#"pub fn x() { 1. <. 2.3 }"#);

    // Custom type creation
    assert_erl!(
        r#"pub type Pair(x, y) { Pair(x: x, y: y) } pub fn x() { Pair(1, 2) Pair(3., 4.) }"#
    );

    assert_erl!(r#"type Null { Null } fn x() { Null }"#);
}

#[test]
fn integration_test3() {
    assert_erl!(
        r#"type Point { Point(x: Int, y: Int) }
                fn y() { fn() { Point }()(4, 6) }"#
    );

    assert_erl!(
        r#"type Point { Point(x: Int, y: Int) }
                fn x() { Point(x: 4, y: 6) Point(y: 1, x: 9) }"#
    );

    assert_erl!(r#"type Point { Point(x: Int, y: Int) } fn x(y) { let Point(a, b) = y a }"#);

    //https://github.com/gleam-lang/gleam/issues/1106
    assert_erl!(
        r#"pub type State{ Start(Int) End(Int) }
            pub fn build(constructor : fn(Int) -> a) -> a { constructor(1) }
            pub fn main() { build(End) }"#
    );

    // Private external function calls are simply inlined
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

    // Private external function references are inlined
    assert_erl!(
        r#"external fn go(x: Int, y: Int) -> Int = "m" "f"
pub fn x() { go }"#
    );

    assert_erl!(
        r#"fn go(x xx, y yy) { xx }
pub fn x() { go(x: 1, y: 2) go(y: 3, x: 4) }"#
    );

    // https://github.com/gleam-lang/gleam/issues/289
    assert_erl!(
        r#"
type User { User(id: Int, name: String, age: Int) }
fn create_user(user_id) { User(age: 22, id: user_id, name: "") }
                    "#
    );
}

#[test]
fn integration_test5() {
    assert_erl!(r#"pub fn run() { case 1, 2 { a, b -> a } }"#);

    assert_erl!(
        r#"type X { X(x: Int, y: Float) }
                    fn x() { X(x: 1, y: 2.) X(y: 3., x: 4) }"#
    );

    assert_erl!(
        r#"
pub fn go(a) {
  let a = a + 1
  a
}

                    "#
    );

    assert_erl!(
        r#"
pub fn go(a) {
  let a = 1
  a
}

                    "#
    );
}

#[test]
fn integration_test6() {
    // https://github.com/gleam-lang/gleam/issues/358
    assert_erl!(
        r#"
pub fn factory(f, i) {
  f(i)
}

pub type Box {
  Box(i: Int)
}

pub fn main() {
  factory(Box, 0)
}
"#
    );
}

#[test]
fn integration_test7() {
    // https://github.com/gleam-lang/gleam/issues/384
    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    _ -> {
      let a = 1
      a
    }
  }
  let a = 2
  a
}
"#
    );
}

#[test]
fn binop_parens() {
    // Parentheses are added for binop subexpressions
    assert_erl!(
        r#"
pub fn main() {
    let a = 2 * {3 + 1} / 2
    let b = 5 + 3 / 3 * 2 - 6 * 4
    b
}
"#
    );
}

#[test]
fn field_access_function_call() {
    // Parentheses are added when calling functions returned by record access
    assert_erl!(
        r#"
type FnBox {
  FnBox(f: fn(Int) -> Int)
}
fn main() {
    let b = FnBox(f: fn(x) { x })
    b.f(5)
}
"#
    );

    // Parentheses are added when calling functions returned by tuple access
    assert_erl!(
        r#"
pub fn main() {
    let t = #(fn(x) { x })

    t.0(5)
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/777
#[test]
fn block_assignment() {
    assert_erl!(
        r#"
pub fn main() {
  let x = {
    1
    2
  }
  x
}
"#
    );
}

#[test]
fn recursive_type() {
    // TODO: we should be able to generalise `id` and we should be
    // able to handle recursive types. Either of these type features
    // would make this module type check OK.
    assert_erl!(
        r#"
fn id(x) {
  x
}

pub fn main() {
  id(id)
}
"#
    );
}

#[test]
fn tuple_access_in_guard() {
    assert_erl!(
        r#"
pub fn main() {
    let key = 10
    let x = [#(10, 2), #(1, 2)]
    case x {
        [first, ..rest] if first.0 == key -> "ok"
        _ -> "ko"
    }
}
"#
    );
}

#[test]
fn variable_name_underscores_preserved() {
    assert_erl!(
        "pub fn a(name_: String) -> String {
    let name__ = name_
    let name = name__
    let one_1 = 1
    let one1 = one_1
    name
}"
    );
}

#[test]
fn allowed_string_escapes() {
    assert_erl!(r#"pub fn a() { "\n" "\r" "\t" "\\" "\"" "\e" "\\^" }"#);
}

// https://github.com/gleam-lang/gleam/issues/1006
#[test]
fn keyword_constructors() {
    assert_erl!("pub type X { Div }");

    assert_erl!("pub type X { Fun(Int) }");
}

#[test]
fn discard_in_assert() {
    assert_erl!(
        "pub fn x(y) {
  assert Ok(_) = y
  1
}"
    );
}

// https://github.com/gleam-lang/gleam/issues/1424
#[test]
fn operator_pipe_right_hand_side() {
    assert_erl!(
        "fn id(x) {
  x
}
        
pub fn bool_expr(x, y) {
  y || x |> id 
}"
    );
}

#[test]
fn negation() {
    assert_erl!(
        "pub fn negate(x) {
  !x
}"
    )
}

#[test]
fn negation_block() {
    assert_erl!(
        "pub fn negate(x) {
  !{
    123
    x
  }
}"
    )
}
