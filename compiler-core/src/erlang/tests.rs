mod assert;
mod records;
mod statement_if;
mod todo;
mod variables;

#[macro_export]
macro_rules! assert_erl {
    ($src:expr $(,)?) => {{
        use crate::{
            build::Origin,
            erlang::module,
            line_numbers::LineNumbers,
            type_::{build_prelude, infer_module},
        };
        use std::collections::HashMap;
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["the_app".to_string()];
        let mut modules = HashMap::new();
        let mut uid = 0;
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), build_prelude(&mut uid));
        let ast = infer_module(
            crate::build::Target::Erlang,
            &mut 0,
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
fn bit_string_discard() {
    // https://github.com/gleam-lang/gleam/issues/704

    assert_erl!(
        r#"
pub fn bitstring_discard(x) -> Bool {
 case x {
  <<_:utf8, rest:binary>> -> True
   _ -> False
 }
}
                    "#
    );

    assert_erl!(
        r#"
pub fn bitstring_discard(x) -> Bool {
 case x {
  <<_discardme:utf8, rest:binary>> -> True
   _ -> False
 }
}
                    "#
    );
}

#[test]
fn bit_string_declare_and_use_var() {
    assert_erl!(
        r#"pub fn go(x) {
  let <<name_size:8, name:binary-size(name_size)>> = x
  name
}"#
    );
}

#[test]
fn clause_guards() {
    // Clause guards
    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    x if x == args -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    x if {x != x} == {args == args} -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    x if x && x || x == x && x -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1, 0 {
    x, y if x > y -> 1
    _, _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1, 0 {
    x, y if x >= y -> 1
    _, _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1, 0 {
    x, y if x < y -> 1
    _, _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1, 0 {
    x, y if x <= y -> 1
    _, _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1.0, 0.1 {
    x, y if x >. y -> 1
    _, _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1.0, 0.1 {
    x, y if x >=. y -> 1
    _, _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = 0.123
  case x {
    99.9854 -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = 0.123
  case x {
    _ if x == 3.14 -> 1
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = 0.123
  case x {
    _ if 0.123 <. x -> 1
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main(x) {
  case x {
    _ if x == [1, 2, 3] -> 1
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = 0
  case x {
    0 -> 1
    _ -> 0
  }
}
"#
    );

    // Tuple literals in guards

    assert_erl!(
        r#"
pub fn main() {
  let x = #(1, 2, 3)
  case x {
    _ if x == #(1, 2, 3) -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = #(1, 2, 3)
  case x {
    _ if x == #(1, 2, 3) -> 1
    _ if x == #(2, 3, 4) -> 2
    _ -> 0
  }
}
"#
    );

    // Int literals in guards

    assert_erl!(
        r#"
pub fn main() {
  let x = 0
  case x {
    _ if x == 0 -> 1
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = 0
  case x {
    _ if 0 < x -> 1
  }
}
"#
    );

    // String literals in guards

    assert_erl!(
        r#"
pub fn main() {
  case "test" {
    x if x == "test" -> 1
  }
}
"#
    );

    // Record literals in guards

    assert_erl!(
        r#"
    type Test { Test(x: Int, y: Float) }
    pub fn main() {
      let x = Test(1, 3.0)
      case x {
        _ if x == Test(1, 1.0) -> 1
        _ if x == Test(y: 2.0, x: 2) -> 2
        _ if x != Test(2, 3.0) -> 2
        _ -> 0
      }
    }
"#
    );

    // Float vars in guards

    assert_erl!(
        r#"
pub fn main() {
  case 0.1, 1.0 {
    x, y if x <. y -> 1
    _, _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  case 0.1, 1.0 {
    x, y if x <=. y -> 1
    _, _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    [x] | [x, _] if x -> 1
    _ -> 0
  }
}
"#
    );
}

#[test]
fn record_accessors() {
    // We can use record accessors for types with only one constructor
    assert_erl!(
        r#"
pub type Person { Person(name: String, age: Int) }
pub fn get_age(person: Person) { person.age }
pub fn get_name(person: Person) { person.name }
"#
    );
}

#[test]
fn record_spread() {
    // Test binding to a record field with the spread operator
    assert_erl!(
        r#"
type Triple {
    Triple(a: Int, b: Int, c: Int)
}

fn main() {
  let triple = Triple(1,2,3)
  let Triple(the_a, ..) = triple
  the_a
}
"#
    );

    // Test binding to a record field with the spread operator and a labelled argument
    assert_erl!(
        r#"
type Triple {
  Triple(a: Int, b: Int, c: Int)
}

fn main() {
  let triple = Triple(1,2,3)
  let Triple(b: the_b, ..) = triple
  the_b
}
"#
    );

    // Test binding to a record field with the spread operator with both a labelled argument and a positional argument
    assert_erl!(
        r#"
type Triple {
  Triple(a: Int, b: Int, c: Int)
}

fn main() {
  let triple = Triple(1,2,3)
  let Triple(the_a, c: the_c, ..) = triple
  the_c
}
"#
    );

    // Test binding to a record field with the spread operator in a match
    assert_erl!(
        r#"
type Triple {
  Triple(a: Int, b: Int, c: Int)
}

fn main() {
  let triple = Triple(1,2,3)
  case triple {
    Triple(b: the_b, ..) -> the_b
  }
}
"#
    );
}

#[test]
fn clever_pipe_rewriting() {
    // a |> b
    assert_erl!(
        r#"
pub fn apply(f: fn(a) -> b, a: a) { a |> f }
"#
    );

    // a |> b(c)
    assert_erl!(
        r#"
pub fn apply(f: fn(a, Int) -> b, a: a) { a |> f(1) }
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
fn try_expr() {
    assert_erl!(
        r#"
pub fn main() {
    try a = Ok(1)
    try b = Ok(2)
    Ok(a + b)
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

#[test]
fn bit_strings() {
    assert_erl!(
        r#"pub fn main() {
  let a = 1
  let simple = <<1, a>>
  let complex = <<4:int-big, 5.0:little-float, 6:native-int>>
  let <<7:2, 8:size(3), b:binary-size(4)>> = <<1>>
  let <<c:8-unit(1), d:binary-size(2)-unit(2)>> = <<1>>

  simple
}
"#
    );

    assert_erl!(
        r#"pub fn x() { 2 }
fn main() {
  let a = -1
  let b = <<a:unit(2)-size(a * 2), a:size(3 + x())-unit(1)>>

  b
}
"#
    );

    assert_erl!(
        r#"pub fn main() {
  let a = 1
  let <<b, 1>> = <<1, a>>
  b
}
"#
    );

    assert_erl!(
        r#"pub fn main() {
  let a = <<"test":utf8>>
  let <<b:utf8_codepoint, "st":utf8>> = a
  b
}
"#
    );

    assert_erl!(
        r#"fn x() { 1 }
pub fn main() {
  let a = <<x():int>>
  a
}
"#
    );
}

#[test]
fn only_guards() {
    assert_erl!(
        r#"
pub const string_value = "constant value"

pub fn main(arg) {
  case arg {
    _ if arg == string_value -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub const bits = <<1, "ok":utf8, 3, 4:50>>

pub fn main(arg) {
  case arg {
    _ if arg == bits -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub const constant = #(1, 2.0)

pub fn main(arg) {
  case arg {
    _ if arg == constant -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub const float_value = 3.14

pub fn main(arg) {
  case arg {
    _ if arg >. float_value -> 1
    _ -> 0
  }
}
"#
    );
}

#[test]
fn constants_in_guards() {
    assert_erl!(
        r#"
pub const string_value = "constant value"
pub const float_value = 3.14
pub const int_value = 42
pub const tuple_value = #(1, 2.0, "3")
pub const list_value = [1, 2, 3]

pub fn main(arg) {
  let _ = list_value
  case arg {
    #(w, x, y, z) if w == tuple_value && x == string_value && y >. float_value && z == int_value -> 1
    _ -> 0
  }
}
"#
    );

    assert_erl!(
        r#"
pub const list = [1, 2, 3]

pub fn main(arg) {
  case arg {
    _ if arg == list -> 1
    _ -> 0
  }
}
"#
    );
}

#[test]
fn alternative_patterns() {
    // reassigning name in alternative patterns
    assert_erl!(
        r#"
pub fn test() {
  let duplicate_name = 1

  case 1 {
    1 | 2 -> {
      let duplicate_name = duplicate_name + 1
      duplicate_name
    }
  }
}"#
    );

    // Alternative patterns with a clause containing vars
    assert_erl!(
        r#"
pub fn test() {
  case Ok(1) {
    Ok(duplicate_name) | Error(duplicate_name) -> duplicate_name
  }
}"#
    );

    // Alternative patterns with a guard clause containing vars
    assert_erl!(
        r#"
pub fn test() {
    let duplicate_name = 1

    case 1 {
        1 | 2 if duplicate_name == 1 -> duplicate_name
    }
}"#
    );

    assert_erl!(
        r#"
pub const constant = Ok(1)

pub fn main(arg) {
  let _ = constant
  case arg {
    _ if arg == constant -> 1
    _ -> 0
  }
}
"#
    );
}

#[test]
fn record_updates() {
    // Record updates
    assert_erl!(
        r#"
pub type Person { Person(name: String, age: Int) }

fn main() {
    let p = Person("Quinn", 27)
    let new_p = Person(..p, age: 28)
    new_p
}
"#
    );

    // Record updates with field accesses
    assert_erl!(
        r#"
pub type Person { Person(name: String, age: Int) }

fn main() {
    let p = Person("Quinn", 27)
    let new_p = Person(..p, age: p.age + 1)
    new_p
}
"#
    );

    // Record updates with multiple fields
    assert_erl!(
        r#"
pub type Person { Person(name: String, age: Int) }

fn main() {
    let p = Person("Quinn", 27)
    let new_p = Person(..p, age: 28, name: "Riley")
    new_p
}
"#
    );

    // Record updates when record is returned from function
    assert_erl!(
        r#"
pub type Person { Person(name: String, age: Int) }

fn main() {
    let new_p = Person(..return_person(), age: 28)
    new_p
}

fn return_person() {
    Person("Quinn", 27)
}
"#
    );

    // Record updates when record is field on another record
    assert_erl!(
        r#"
pub type Car { Car(make: String, model: String, driver: Person) }
pub type Person { Person(name: String, age: Int) }

fn main() {
    let car = Car(make: "Amphicar", model: "Model 770", driver: Person(name: "John Doe", age: 27))
    let new_p = Person(..car.driver, age: 28)
    new_p
}
"#
    );
}

#[test]
fn numbers_with_underscores() {
    assert_erl!(
        r#"
pub fn main() {
  100_000
  100_000.00101
}
"#
    );

    assert_erl!(
        r#"
const i = 100_000
const f = 100_000.00101
pub fn main() {
  i
  f
}
"#
    );

    assert_erl!(
        r#"
pub fn main() {
  let 100_000 = 1
  let 100_000.00101 = 1.
  1
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
fn record_constants() {
    assert_erl!(
        "pub type Test { A }
const test = A
pub fn a() { A }"
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
fn pattern_as() {
    assert_erl!(
        "pub fn a(x) {
  case x {
    Ok(1 as y) -> 1
    _ -> 0
  }
}"
    );
}

#[test]
fn build_in_erlang_type_escaping() {
    assert_erl!("pub external type Map");
}

#[test]
fn escape_erlang_reserved_keywords_in_type_names() {
    // list of all reserved words in erlang
    // http://erlang.org/documentation/doc-5.8/doc/reference_manual/introduction.html
    assert_erl!(
        r#"pub type After { TestAfter }
pub type And { TestAnd }
pub type Andalso { TestAndAlso }
pub type Band { TestBAnd }
pub type Begin { TestBegin }
pub type Bnot { TestBNot }
pub type Bor { TestBOr }
pub type Bsl { TestBsl }
pub type Bsr { TestBsr }
pub type Bxor { TestBXor }
pub type Case { TestCase }
pub type Catch { TestCatch }
pub type Cond { TestCond }
pub type Div { TestDiv }
pub type End { TestEnd }
pub type Fun { TestFun }
pub type If { TestIf }
pub type Let { TestLet }
pub type Not { TestNot }
pub type Of { TestOf }
pub type Or { TestOr }
pub type Orelse { TestOrElse }
pub type Query { TestQuery }
pub type Receive { TestReceive }
pub type Rem { TestRem }
pub type Try { TestTry }
pub type When { TestWhen }
pub type Xor { TestXor }"#
    );
}

#[test]
fn allowed_string_escapes() {
    assert_erl!(r#"pub fn a() { "\n" "\r" "\t" "\\" "\"" "\e" "\\^" }"#);
}

// https://github.com/gleam-lang/gleam/issues/952
#[test]
fn block_expr_into_pipe() {
    assert_erl!(
        r#"fn id(a) { a }
pub fn main() {
  {
    let x = 1
    x
  }
  |> id
}"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1006
#[test]
fn keyword_constructors() {
    assert_erl!("pub type X { Div }");

    assert_erl!("pub type X { Fun(Int) }");
}

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

#[test]
fn pipe_in_list() {
    assert_erl!(
        "pub fn x(f) {
  [
    1 |> f
  ]
}"
    );
}

#[test]
fn pipe_in_tuple() {
    assert_erl!(
        "pub fn x(f) {
  #(
    1 |> f
  )
}"
    );
}

#[test]
fn pipe_in_case_subject() {
    assert_erl!(
        "pub fn x(f) {
  case 1 |> f {
    x -> x
  }
}"
    );
}

#[test]
fn try_in_case_subject() {
    assert_erl!(
        "pub fn x(f) {
  try x = 1 |> f
  Ok(x)
}"
    );
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

// https://github.com/gleam-lang/gleam/issues/1379
#[test]
fn pipe_in_spread() {
    assert_erl!(
        "pub type X {
  X(a: Int, b: Int)
}

fn id(x) {
  x
}
        
pub fn main(x) {
  X(..x, a: 1 |> id)
}"
    );
}

// https://github.com/gleam-lang/gleam/issues/1385
#[test]
fn pipe_in_eq() {
    assert_erl!(
        "fn id(x) {
  x
}
        
pub fn main() {
    1 == 1 |> id
}"
    );
}
