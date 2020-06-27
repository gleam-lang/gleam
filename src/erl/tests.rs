use super::*;

#[test]
fn record_definition_test() {
    assert_eq!(
        record_definition("PetCat", &[&"name", &"is_cute",]),
        "-record(pet_cat, {name, is_cute}).\n".to_string()
    );

    // Reserved words are escaped in record names and fields
    assert_eq!(
        record_definition("div", &[&"receive", &"catch", &"unreserved"]),
        "-record(\'div\', {\'receive\', \'catch\', unreserved}).\n".to_string()
    );
}

#[test]
fn integration_test() {
    macro_rules! assert_erl {
        ($src:expr, $erl:expr $(,)?) => {
            let mut ast = crate::grammar::ModuleParser::new()
                .parse($src)
                .expect("syntax error");
            ast.name = vec!["the_app".to_string()];
            let ast = crate::typ::infer_module(ast, &std::collections::HashMap::new(), &mut vec![])
                .expect("should successfully infer");
            let output = module(&ast);
            assert_eq!(($src, output), ($src, $erl.to_string()));
        };
    }

    assert_erl!(
        r#"fn go() {
let x = tuple(100000000000000000, tuple(2000000000, 3000000000000, 40000000000), 50000, 6000000000)
  x
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

go() ->
    X = {100000000000000000,
         {2000000000, 3000000000000, 40000000000},
         50000,
         6000000000},
    X.
"#,
    );

    assert_erl!(
        r#"fn go() {
  let y = 1
  let y = 2
  y
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

go() ->
    Y = 1,
    Y1 = 2,
    Y1.
"#,
    );

    assert_erl!(
        r#"fn go() {
  assert y = 1
  assert y = 2
  y
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

go() ->
    Y = 1,
    Y1 = 2,
    Y1.
"#,
    );

    assert_erl!(
        r#"pub fn t() { True }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([t/0]).

t() ->
    true.
"#,
    );

    assert_erl!(
        r#"pub type Money { Pound(Int) }
                    fn pound(x) { Pound(x) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

pound(X) ->
    {pound, X}.
"#,
    );

    assert_erl!(
        r#"fn loop() { loop() }"#,
        r#"-module(the_app).
-compile(no_auto_import).

loop() ->
    loop().
"#,
    );

    assert_erl!(
        r#"pub external fn run() -> Int = "Elixir.MyApp" "run""#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([run/0]).

run() ->
    'Elixir.MyApp':run().
"#,
    );

    assert_erl!(
        r#"fn inc(x) { x + 1 }
                    pub fn go() { 1 |> inc |> inc |> inc }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([go/0]).

inc(X) ->
    X + 1.

go() ->
    inc(inc(inc(1))).
"#,
    );

    assert_erl!(
        r#"fn add(x, y) { x + y }
                    pub fn go() { 1 |> add(_, 1) |> add(2, _) |> add(_, 3) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([go/0]).

add(X, Y) ->
    X + Y.

go() ->
    add(add(2, add(1, 1)), 3).
"#,
    );

    assert_erl!(
        r#"fn and(x, y) { x && y }
                    fn or(x, y) { x || y }
                    fn modulo(x, y) { x % y }
            "#,
        r#"-module(the_app).
-compile(no_auto_import).

'and'(X, Y) ->
    X andalso Y.

'or'(X, Y) ->
    X orelse Y.

modulo(X, Y) ->
    X rem Y.
"#,
    );

    assert_erl!(
        r#"fn second(list) { case list { [x, y] -> y z -> 1 } }
                    fn tail(list) { case list { [x, ..xs] -> xs z -> list } }
            "#,
        r#"-module(the_app).
-compile(no_auto_import).

second(List) ->
    case List of
        [X, Y] ->
            Y;

        Z ->
            1
    end.

tail(List) ->
    case List of
        [X | Xs] ->
            Xs;

        Z ->
            List
    end.
"#,
    );

    assert_erl!(
        "fn tail(list) { case list { [x, ..] -> x } }",
        r#"-module(the_app).
-compile(no_auto_import).

tail(List) ->
    case List of
        [X | _] ->
            X
    end.
"#,
    );

    assert_erl!(
        r#"fn x() { let x = 1 let x = x + 1 x }"#,
        r#"-module(the_app).
-compile(no_auto_import).

x() ->
    X = 1,
    X1 = X + 1,
    X1.
"#,
    );

    assert_erl!(
        r#"pub external fn receive() -> Int = "try" "and"
                    pub fn catch(x) { receive() }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export(['receive'/0, 'catch'/1]).

'receive'() ->
    'try':'and'().

'catch'(X) ->
    'try':'and'().
"#,
    );

    // Translation of Float-specific BinOp into variable-type Erlang term comparison.
    assert_erl!(
        r#"fn x() { 1. <. 2.3 }"#,
        r#"-module(the_app).
-compile(no_auto_import).

x() ->
    1.0 < 2.3.
"#,
    );

    // Custom type creation
    assert_erl!(
        r#"type Pair(x, y) { Pair(x: x, y: y) } fn x() { Pair(1, 2) Pair(3., 4.) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

x() ->
    {pair, 1, 2},
    {pair, 3.0, 4.0}.
"#,
    );

    assert_erl!(
        r#"type Null { Null } fn x() { Null }"#,
        r#"-module(the_app).
-compile(no_auto_import).

x() ->
    null.
"#,
    );

    assert_erl!(
        r#"type Point { Point(x: Int, y: Int) }
                fn y() { fn() { Point }()(4, 6) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

y() ->
    ((fun() -> fun(A, B) -> {point, A, B} end end)())(4, 6).
"#,
    );

    assert_erl!(
        r#"type Point { Point(x: Int, y: Int) }
                fn x() { Point(x: 4, y: 6) Point(y: 1, x: 9) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

x() ->
    {point, 4, 6},
    {point, 9, 1}.
"#,
    );

    assert_erl!(
        r#"type Point { Point(x: Int, y: Int) } fn x(y) { let Point(a, b) = y a }"#,
        r#"-module(the_app).
-compile(no_auto_import).

x(Y) ->
    {point, A, B} = Y,
    A.
"#,
    );

    // Private external function calls are simply inlined
    assert_erl!(
        r#"external fn go(x: Int, y: Int) -> Int = "m" "f"
                    fn x() { go(x: 1, y: 2) go(y: 3, x: 4) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

x() ->
    m:f(1, 2),
    m:f(4, 3).
"#,
    );

    // Public external function calls are inlined but the wrapper function is
    // also printed in the erlang output and exported
    assert_erl!(
        r#"pub external fn go(x: Int, y: Int) -> Int = "m" "f"
                    fn x() { go(x: 1, y: 2) go(y: 3, x: 4) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([go/2]).

go(A, B) ->
    m:f(A, B).

x() ->
    m:f(1, 2),
    m:f(4, 3).
"#,
    );

    // Private external function references are inlined
    assert_erl!(
        r#"external fn go(x: Int, y: Int) -> Int = "m" "f"
                    fn x() { go }"#,
        r#"-module(the_app).
-compile(no_auto_import).

x() ->
    fun m:f/2.
"#,
    );

    assert_erl!(
        r#"fn go(x xx, y yy) { xx }
                    fn x() { go(x: 1, y: 2) go(y: 3, x: 4) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

go(Xx, Yy) ->
    Xx.

x() ->
    go(1, 2),
    go(4, 3).
"#,
    );

    // https://github.com/gleam-lang/gleam/issues/289
    assert_erl!(
        r#"
type User { User(id: Int, name: String, age: Int) }
fn create_user(user_id) { User(age: 22, id: user_id, name: "") }
                    "#,
        r#"-module(the_app).
-compile(no_auto_import).

create_user(UserId) ->
    {user, UserId, <<""/utf8>>, 22}.
"#,
    );

    assert_erl!(
        r#"fn run() { case 1, 2 { a, b -> a } }"#,
        r#"-module(the_app).
-compile(no_auto_import).

run() ->
    case {1, 2} of
        {A, B} ->
            A
    end.
"#,
    );

    assert_erl!(
        r#"type X { X(x: Int, y: Float) }
                    fn x() { X(x: 1, y: 2.) X(y: 3., x: 4) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

x() ->
    {x, 1, 2.0},
    {x, 4, 3.0}.
"#,
    );

    // https://github.com/gleam-lang/gleam/issues/333
    assert_erl!(
        r#"
fn go(a) {
  case a {
    99 -> {
      let a = a
      1
    }
    _ -> a
  }
}

                    "#,
        r#"-module(the_app).
-compile(no_auto_import).

go(A) ->
    case A of
        99 ->
            A1 = A,
            1;

        _ ->
            A
    end.
"#,
    );

    assert_erl!(
        r#"
fn go(a) {
  let a = a + 1
  a
}

                    "#,
        r#"-module(the_app).
-compile(no_auto_import).

go(A) ->
    A1 = A + 1,
    A1.
"#,
    );

    assert_erl!(
        r#"
fn go(a) {
  let a = 1
  a
}

                    "#,
        r#"-module(the_app).
-compile(no_auto_import).

go(A) ->
    A1 = 1,
    A1.
"#,
    );

    assert_erl!(
        r#"
fn id(x) {
  x
}

fn main() {
  id(id)
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

id(X) ->
    X.

main() ->
    id(fun id/1).
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([factory/2, main/0]).

factory(F, I) ->
    F(I).

main() ->
    factory(fun(A) -> {box, A} end, 0).
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

main(Args) ->
    case Args of
        _ ->
            A = 1,
            A
    end,
    A1 = 2,
    A1.
"#,
    );

    // Clause guards
    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    x if x == args -> 1
    _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

main(Args) ->
    case Args of
        X when X =:= Args ->
            1;

        _ ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    x if {x != x} == {args == args} -> 1
    _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

main(Args) ->
    case Args of
        X when (X =/= X) =:= (Args =:= Args) ->
            1;

        _ ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    x if x && x || x == x && x -> 1
    _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

main(Args) ->
    case Args of
        X when (X andalso X) orelse ((X =:= X) andalso X) ->
            1;

        _ ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1, 0 {
    x, y if x > y -> 1
    _, _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case {1, 0} of
        {X, Y} when X > Y ->
            1;

        {_, _} ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1, 0 {
    x, y if x >= y -> 1
    _, _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case {1, 0} of
        {X, Y} when X >= Y ->
            1;

        {_, _} ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1, 0 {
    x, y if x < y -> 1
    _, _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case {1, 0} of
        {X, Y} when X < Y ->
            1;

        {_, _} ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1, 0 {
    x, y if x <= y -> 1
    _, _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case {1, 0} of
        {X, Y} when X =< Y ->
            1;

        {_, _} ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1.0, 0.1 {
    x, y if x >. y -> 1
    _, _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case {1.0, 0.1} of
        {X, Y} when X > Y ->
            1;

        {_, _} ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  case 1.0, 0.1 {
    x, y if x >=. y -> 1
    _, _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case {1.0, 0.1} of
        {X, Y} when X >= Y ->
            1;

        {_, _} ->
            0
    end.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    X = 0.123,
    case X of
        99.9854 ->
            1;

        _ ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = 0.123
  case x {
    _ if x == 3.14 -> 1
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    X = 0.123,
    case X of
        _ when X =:= 3.14 ->
            1
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = 0.123
  case x {
    _ if 0.123 <. x -> 1
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    X = 0.123,
    case X of
        _ when 0.123 < X ->
            1
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main(x) {
  case x {
    _ if x == [1, 2, 3] -> 1
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

main(X) ->
    case X of
        _ when X =:= [1, 2, 3] ->
            1
    end.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    X = 0,
    case X of
        0 ->
            1;

        _ ->
            0
    end.
"#,
    );

    // Tuple literals in guards

    assert_erl!(
        r#"
pub fn main() {
  let x = tuple(1, 2, 3)
  case x {
    _ if x == tuple(1, 2, 3) -> 1
    _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    X = {1, 2, 3},
    case X of
        _ when X =:= {1, 2, 3} ->
            1;

        _ ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = tuple(1, 2, 3)
  case x {
    _ if x == tuple(1, 2, 3) -> 1
    _ if x == tuple(2, 3, 4) -> 2
    _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    X = {1, 2, 3},
    case X of
        _ when X =:= {1, 2, 3} ->
            1;

        _ when X =:= {2, 3, 4} ->
            2;

        _ ->
            0
    end.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    X = 0,
    case X of
        _ when X =:= 0 ->
            1
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  let x = 0
  case x {
    _ if 0 < x -> 1
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    X = 0,
    case X of
        _ when 0 < X ->
            1
    end.
"#,
    );

    // String literals in guards

    assert_erl!(
        r#"
pub fn main() {
  case "test" {
    x if x == "test" -> 1
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case <<"test"/utf8>> of
        X when X =:= <<"test"/utf8>> ->
            1
    end.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    X = {test, 1, 3.0},
    case X of
        _ when X =:= {test, 1, 1.0} ->
            1;

        _ when X =:= {test, 2, 2.0} ->
            2;

        _ when X =/= {test, 2, 3.0} ->
            2;

        _ ->
            0
    end.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case {0.1, 1.0} of
        {X, Y} when X < Y ->
            1;

        {_, _} ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  case 0.1, 1.0 {
    x, y if x <=. y -> 1
    _, _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    case {0.1, 1.0} of
        {X, Y} when X =< Y ->
            1;

        {_, _} ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main(args) {
  case args {
    [x] | [x, _] if x -> 1
    _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

main(Args) ->
    case Args of
        [X] when X ->
            1;

        [X1, _] when X1 ->
            1;

        _ ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  todo
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

main() ->
    erlang:error({gleam_error, todo}).
"#,
    );

    // We can use record accessors for types with only one constructor
    assert_erl!(
        r#"
pub type Person { Person(name: String, age: Int) }
pub fn get_age(person: Person) { person.age }
pub fn get_name(person: Person) { person.name }
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([get_age/1, get_name/1]).

get_age(Person) ->
    erlang:element(3, Person).

get_name(Person) ->
    erlang:element(2, Person).
"#,
    );

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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

main() ->
    Triple = {triple, 1, 2, 3},
    {triple, TheA, _, _} = Triple,
    TheA.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

main() ->
    Triple = {triple, 1, 2, 3},
    {triple, _, TheB, _} = Triple,
    TheB.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

main() ->
    Triple = {triple, 1, 2, 3},
    {triple, TheA, _, TheC} = Triple,
    TheC.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

main() ->
    Triple = {triple, 1, 2, 3},
    case Triple of
        {triple, _, TheB, _} ->
            TheB
    end.
"#,
    );

    // a |> b
    assert_erl!(
        r#"
pub fn apply(f: fn(a) -> b, a: a) { a |> f }
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([apply/2]).

apply(F, A) ->
    F(A).
"#,
    );

    // a |> b(c)
    assert_erl!(
        r#"
pub fn apply(f: fn(a, Int) -> b, a: a) { a |> f(1) }
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([apply/2]).

apply(F, A) ->
    F(A, 1).
"#,
    );

    // Parentheses are added for binop subexpressions
    assert_erl!(
        r#"
fn main() {
    let a = 2 * {3 + 1} / 2
    let b = 5 + 3 / 3 * 2 - 6 * 4
    b
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

main() ->
    A = (2 * (3 + 1)) div 2,
    B = (5 + ((3 div 3) * 2)) - (6 * 4),
    B.
"#,
    );

    // try
    assert_erl!(
        r#"
fn main() {
    try a = Ok(1)
    try b = Ok(2)
    Ok(a + b)
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

main() ->
    case {ok, 1} of
        {error, GleamTryError} -> {error, GleamTryError};
        {ok, A} ->
            case {ok, 2} of
                {error, GleamTryError1} -> {error, GleamTryError1};
                {ok, B} ->
                    {ok, A + B}
            end
    end.
"#,
    );

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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

main() ->
    B = {fn_box, fun(X) -> X end},
    (erlang:element(2, B))(5).
"#,
    );

    // Parentheses are added when calling functions returned by tuple access
    assert_erl!(
        r#"
fn main() {
    let t = tuple(fn(x) { x })

    t.0(5)
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

main() ->
    T = {fun(X) -> X end},
    (erlang:element(1, T))(5).
"#,
    );

    // BitStrings

    assert_erl!(
        r#"fn main() {
  let a = 1
  let simple = <<1, a>>
  let complex = <<4:int-unsigned-big, 5.0:little-float, 6:native-int-signed>>
  let <<7:2, 8:size(3), b:binary-size(4)>> = <<1>>
  let <<c:unit(1), d:binary-size(2)-unit(2)>> = <<1>>

  simple
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

main() ->
    A = 1,
    Simple = <<1, A>>,
    Complex = <<4/integer-unsigned-big,
                5.0/little-float,
                6/native-integer-signed>>,
    <<7:2, 8:3, B:4/binary>> = <<1>>,
    <<C/unit:1, D:2/binary-unit:2>> = <<1>>,
    Simple.
"#,
    );

    assert_erl!(
        r#"fn x() { 2 }
fn main() {
  let a = 1
  let b = <<a:unit(2)-size(a * 2), a:size(3 + x())-unit(1)>>

  b
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

x() ->
    2.

main() ->
    A = 1,
    B = <<A:(A * 2)/unit:2, A:(3 + x())/unit:1>>,
    B.
"#,
    );

    assert_erl!(
        r#"fn main() {
  let a = 1
  let <<b, 1>> = <<1, a>>
  b
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

main() ->
    A = 1,
    <<B, 1>> = <<1, A>>,
    B.
"#,
    );

    assert_erl!(
        r#"fn main() {
  let a = <<"test":utf8>>
  let <<b:utf8, "st":utf8>> = a
  b
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

main() ->
    A = <<"test"/utf8>>,
    <<B/utf8, "st"/utf8>> = A,
    B.
"#,
    );

    assert_erl!(
        r#"fn x() { 1 }
fn main() {
  let a = <<x():int>>
  a
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

x() ->
    1.

main() ->
    A = <<(x())/integer>>,
    A.
"#,
    );

    assert_erl!(
        r#"
pub const string_value = "constant value"
pub const float_value = 3.14
pub const int_value = 42
pub const tuple_value = tuple(1, 2.0, "3")
pub const list_value = [1, 2, 3]

pub fn main(arg) {
  let _ = list_value
  case arg {
    tuple(w, x, y, z) if w == tuple_value && x == string_value && y >. float_value && z == int_value -> 1
    _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

main(Arg) ->
    _ = [1, 2, 3],
    case Arg of
        {W,
         X,
         Y,
         Z} when (((W =:= {1,
                           2.0,
                           <<"3"/utf8>>}) andalso (X =:= <<"constant value"/utf8>>)) andalso (Y > 3.14)) andalso (Z =:= 42) ->
            1;

        _ ->
            0
    end.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

main(Arg) ->
    case Arg of
        _ when Arg =:= [1, 2, 3] ->
            1;

        _ ->
            0
    end.
"#,
    );

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
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([test/0]).

test() ->
    DuplicateName = 1,
    case 1 of
        1 ->
            DuplicateName1 = DuplicateName + 1,
            DuplicateName1;

        2 ->
            DuplicateName1 = DuplicateName + 1,
            DuplicateName1
    end.
"#,
    );
}
