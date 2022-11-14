mod assert;
mod bit_strings;
mod case;
mod external_fn;
mod guards;
mod numbers;
mod patterns;
mod pipes;
mod records;
mod reserved;
mod statement_if;
mod strings;
mod todo;
mod try_;
mod use_;
mod variables;

#[macro_export]
macro_rules! assert_erl {
    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr $(,)?) => {{
        use $crate::{erlang::module, line_numbers::LineNumbers, uid::UniqueIdGenerator};
        let mut modules = im::HashMap::new();
        let ids = UniqueIdGenerator::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), $crate::type_::build_prelude(&ids));
        let (mut ast, _) = $crate::parse::parse_module($dep_src).expect("dep syntax error");
        ast.name = $dep_name;
        let dep = $crate::type_::infer_module(
            $crate::build::Target::JavaScript,
            &ids,
            ast,
            $crate::build::Origin::Src,
            $dep_package,
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let _ = modules.insert($dep_name.join("/"), dep.type_info);
        let (mut ast, _) = $crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my".to_string(), "mod".to_string()];
        let ast = $crate::type_::infer_module(
            $crate::build::Target::Erlang,
            &ids,
            ast,
            $crate::build::Origin::Src,
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

    ($src:expr $(,)?) => {{
        use $crate::{
            build::Origin,
            erlang::module,
            line_numbers::LineNumbers,
            type_::{build_prelude, infer_module},
            uid::UniqueIdGenerator,
        };
        let (mut ast, _) = $crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["the_app".to_string()];
        let mut modules = im::HashMap::new();
        let ids = UniqueIdGenerator::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), build_prelude(&ids));
        let ast = infer_module(
            $crate::build::Target::Erlang,
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
}

#[test]
fn integration_test1_1() {
    assert_erl!(
        r#"pub type Money { Pound(Int) }
                    fn pound(x) { Pound(x) }"#
    );
}

#[test]
fn integration_test1_2() {
    assert_erl!(r#"pub fn loop() { loop() }"#);
}

#[test]
fn integration_test1_4() {
    assert_erl!(
        r#"fn inc(x) { x + 1 }
                    pub fn go() { 1 |> inc |> inc |> inc }"#
    );
}

#[test]
fn integration_test1_5() {
    assert_erl!(
        r#"fn add(x, y) { x + y }
                    pub fn go() { 1 |> add(_, 1) |> add(2, _) |> add(_, 3) }"#
    );
}

#[test]
fn integration_test1_6() {
    assert_erl!(
        r#"pub fn and(x, y) { x && y }
pub fn or(x, y) { x || y }
pub fn remainder(x, y) { x % y }
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
}

#[test]
fn integration_test5() {
    assert_erl!("pub fn tail(list) { case list { [x, ..] -> x } }");
}

#[test]
fn integration_test6() {
    assert_erl!(r#"pub fn x() { let x = 1 let x = x + 1 x }"#);
}

#[test]
fn integration_test8() {
    // Translation of Float-specific BinOp into variable-type Erlang term comparison.
    assert_erl!(r#"pub fn x() { 1. <. 2.3 }"#);
}

#[test]
fn integration_test9() {
    // Custom type creation
    assert_erl!(
        r#"pub type Pair(x, y) { Pair(x: x, y: y) } pub fn x() { Pair(1, 2) Pair(3., 4.) }"#
    );
}

#[test]
fn integration_test10() {
    assert_erl!(r#"type Null { Null } fn x() { Null }"#);
}

#[test]
fn integration_test3() {
    assert_erl!(
        r#"type Point { Point(x: Int, y: Int) }
                fn y() { fn() { Point }()(4, 6) }"#
    );
}

#[test]
fn integration_test11() {
    assert_erl!(
        r#"type Point { Point(x: Int, y: Int) }
                fn x() { Point(x: 4, y: 6) Point(y: 1, x: 9) }"#
    );
}

#[test]
fn integration_test12() {
    assert_erl!(r#"type Point { Point(x: Int, y: Int) } fn x(y) { let Point(a, b) = y a }"#);
}
//https://github.com/gleam-lang/gleam/issues/1106

#[test]
fn integration_test13() {
    assert_erl!(
        r#"pub type State{ Start(Int) End(Int) }
            pub fn build(constructor : fn(Int) -> a) -> a { constructor(1) }
            pub fn main() { build(End) }"#
    );
}

#[test]
fn integration_test16() {
    assert_erl!(
        r#"fn go(x xx, y yy) { xx }
pub fn x() { go(x: 1, y: 2) go(y: 3, x: 4) }"#
    );
}

#[test]
fn integration_test17() {
    // https://github.com/gleam-lang/gleam/issues/289
    assert_erl!(
        r#"
type User { User(id: Int, name: String, age: Int) }
fn create_user(user_id) { User(age: 22, id: user_id, name: "") }
                    "#
    );
}

#[test]
fn integration_test18() {
    assert_erl!(r#"pub fn run() { case 1, 2 { a, b -> a } }"#);
}

#[test]
fn integration_test19() {
    assert_erl!(
        r#"type X { X(x: Int, y: Float) }
                    fn x() { X(x: 1, y: 2.) X(y: 3., x: 4) }"#
    );
}

#[test]
fn integration_test20() {
    assert_erl!(
        r#"
pub fn go(a) {
  let a = a + 1
  a
}

                    "#
    );
}

#[test]
fn integration_test21() {
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
fn integration_test22() {
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
fn integration_test23() {
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

// https://github.com/gleam-lang/gleam/issues/1655
#[test]
fn tail_maybe_expr_block() {
    assert_erl!(
        "pub fn a() {
  let fake_tap = fn(x) { x }
  let b = [99]
  [
    1,
    2,
    ..b
    |> fake_tap
  ]
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/1587
#[test]
fn guard_variable_rewriting() {
    assert_erl!(
        "pub fn main() {
  case 1.0 {
    a if a <. 0.0 -> {
      let a = a
      a
    }
  }
}
"
    )
}

// https://github.com/gleam-lang/gleam/issues/1816
#[test]
fn function_argument_shadowing() {
    assert_erl!(
        "pub fn main(a) {
  Box
}

pub type Box {
  Box(Int)
}
"
    )
}
