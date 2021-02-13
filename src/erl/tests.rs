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

macro_rules! assert_erl {
    ($src:expr, $erl:expr $(,)?) => {
        // println!("\n\n\n{}\n", $src);
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["the_app".to_string()];
        let ast =
            crate::typ::infer_module(&mut 0, ast, &std::collections::HashMap::new(), &mut vec![])
                .expect("should successfully infer");
        let mut output = String::new();
        let line_numbers = LineNumbers::new($src);
        module(&ast, &line_numbers, &mut output).unwrap();
        assert_eq!(($src, output), ($src, $erl.to_string()));
    };
}

#[test]
fn variable_rewrite() {
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

-spec go(integer()) -> integer().
go(A) ->
    case A of
        99 ->
            A@1 = A,
            1;

        _ ->
            A
    end.
"#,
    );

    // https://github.com/gleam-lang/gleam/issues/772
    assert_erl!(
        "fn main(board) {
  fn(board) { board }
  board
}",
        r#"-module(the_app).
-compile(no_auto_import).

-spec main(H) -> H.
main(Board) ->
    fun(Board@1) -> Board@1 end,
    Board.
"#,
    );

    // https://github.com/gleam-lang/gleam/issues/762
    assert_erl!(
        r#"
fn main(x) {
  fn(x) { x }(x)
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec main(H) -> H.
main(X) ->
    (fun(X@1) -> X@1 end)(X).
"#,
    );

    assert_erl!(
        r#"
fn main(x) {
  x
  |> fn(x) { x }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec main(K) -> K.
main(X) ->
    (fun(X@1) -> X@1 end)(X).
"#,
    );

    // https://github.com/gleam-lang/gleam/issues/788
    assert_erl!(
        r#"fn go() {
  let _r = 1
  let _r = 2
  Nil
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec go() -> nil.
go() ->
    _ = 1,
    _ = 2,
    nil.
"#,
    );
}

#[test]
fn integration_test() {
    assert_erl!(
        r#"fn go() {
let x = tuple(100000000000000000, tuple(2000000000, 3000000000000, 40000000000), 50000, 6000000000)
  x
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec go() -> {integer(),
               {integer(), integer(), integer()},
               integer(),
               integer()}.
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

-spec go() -> integer().
go() ->
    Y = 1,
    Y@1 = 2,
    Y@1.
"#,
    );

    // hex, octal, and binary literals
    assert_erl!(
        r#"fn go() {
    let fifteen = 0xF
    let nine = 0o11
    let ten = 0b1010
  fifteen
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec go() -> integer().
go() ->
    Fifteen = 16#F,
    Nine = 8#11,
    Ten = 2#1010,
    Fifteen.
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

-spec go() -> integer().
go() ->
    Y = 1,
    Y@1 = 2,
    Y@1.
"#,
    );

    assert_erl!(
        r#"pub fn t() { True }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([t/0]).

-spec t() -> boolean().
t() ->
    true.
"#,
    );

    assert_erl!(
        r#"pub type Money { Pound(Int) }
                    fn pound(x) { Pound(x) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export_type([money/0]).

-type money() :: {pound, integer()}.

-spec pound(integer()) -> money().
pound(X) ->
    {pound, X}.
"#,
    );

    assert_erl!(
        r#"fn loop() { loop() }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec loop() -> any().
loop() ->
    loop().
"#,
    );

    assert_erl!(
        r#"pub external fn run() -> Int = "Elixir.MyApp" "run""#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([run/0]).

-spec run() -> integer().
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

-spec inc(integer()) -> integer().
inc(X) ->
    X + 1.

-spec go() -> integer().
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

-spec add(integer(), integer()) -> integer().
add(X, Y) ->
    X + Y.

-spec go() -> integer().
go() ->
    add(add(2, add(1, 1)), 3).
"#,
    );

    assert_erl!(
        r#"fn and(x, y) { x && y }
fn or(x, y) { x || y }
fn modulo(x, y) { x % y }
fn fdiv(x, y) { x /. y }
            "#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec 'and'(boolean(), boolean()) -> boolean().
'and'(X, Y) ->
    X andalso Y.

-spec 'or'(boolean(), boolean()) -> boolean().
'or'(X, Y) ->
    X orelse Y.

-spec modulo(integer(), integer()) -> integer().
modulo(X, Y) ->
    case Y of
        0 -> 0;
        Gleam@denominator -> X rem Gleam@denominator
    end.

-spec fdiv(float(), float()) -> float().
fdiv(X, Y) ->
    case Y of
        0.0 -> 0.0;
        Gleam@denominator -> X / Gleam@denominator
    end.
"#,
    );

    assert_erl!(
        r#"fn second(list) { case list { [x, y] -> y z -> 1 } }
                    fn tail(list) { case list { [x, ..xs] -> xs z -> list } }
            "#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec second(list(integer())) -> integer().
second(List) ->
    case List of
        [X, Y] ->
            Y;

        Z ->
            1
    end.

-spec tail(list(P)) -> list(P).
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

-spec tail(list(K)) -> K.
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

-spec x() -> integer().
x() ->
    X = 1,
    X@1 = X + 1,
    X@1.
"#,
    );

    assert_erl!(
        r#"pub external fn receive() -> Int = "try" "and"
                    pub fn catch(x) { receive() }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export(['receive'/0, 'catch'/1]).

-spec 'receive'() -> integer().
'receive'() ->
    'try':'and'().

-spec 'catch'(any()) -> integer().
'catch'(X) ->
    'try':'and'().
"#,
    );

    // Translation of Float-specific BinOp into variable-type Erlang term comparison.
    assert_erl!(
        r#"fn x() { 1. <. 2.3 }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec x() -> boolean().
x() ->
    1.0 < 2.3.
"#,
    );

    // Custom type creation
    assert_erl!(
        r#"type Pair(x, y) { Pair(x: x, y: y) } fn x() { Pair(1, 2) Pair(3., 4.) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export_type([pair/2]).

-type pair(H, I) :: {pair, H, I}.

-spec x() -> pair(float(), float()).
x() ->
    {pair, 1, 2},
    {pair, 3.0, 4.0}.
"#,
    );

    assert_erl!(
        r#"type Null { Null } fn x() { Null }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export_type([null/0]).

-type null() :: null.

-spec x() -> null().
x() ->
    null.
"#,
    );

    assert_erl!(
        r#"type Point { Point(x: Int, y: Int) }
                fn y() { fn() { Point }()(4, 6) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export_type([point/0]).

-type point() :: {point, integer(), integer()}.

-spec y() -> point().
y() ->
    ((fun() -> fun(A, B) -> {point, A, B} end end)())(4, 6).
"#,
    );

    assert_erl!(
        r#"type Point { Point(x: Int, y: Int) }
                fn x() { Point(x: 4, y: 6) Point(y: 1, x: 9) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export_type([point/0]).

-type point() :: {point, integer(), integer()}.

-spec x() -> point().
x() ->
    {point, 4, 6},
    {point, 9, 1}.
"#,
    );

    assert_erl!(
        r#"type Point { Point(x: Int, y: Int) } fn x(y) { let Point(a, b) = y a }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export_type([point/0]).

-type point() :: {point, integer(), integer()}.

-spec x(point()) -> integer().
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

-spec x() -> integer().
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

-spec go(integer(), integer()) -> integer().
go(A, B) ->
    m:f(A, B).

-spec x() -> integer().
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

-spec x() -> fun((integer(), integer()) -> integer()).
x() ->
    fun m:f/2.
"#,
    );

    assert_erl!(
        r#"fn go(x xx, y yy) { xx }
                    fn x() { go(x: 1, y: 2) go(y: 3, x: 4) }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec go(H, any()) -> H.
go(Xx, Yy) ->
    Xx.

-spec x() -> integer().
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

-export_type([user/0]).

-type user() :: {user, integer(), binary(), integer()}.

-spec create_user(integer()) -> user().
create_user(User_id) ->
    {user, User_id, <<""/utf8>>, 22}.
"#,
    );

    assert_erl!(
        r#"fn run() { case 1, 2 { a, b -> a } }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec run() -> integer().
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

-export_type([x/0]).

-type x() :: {x, integer(), float()}.

-spec x() -> x().
x() ->
    {x, 1, 2.0},
    {x, 4, 3.0}.
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

-spec go(integer()) -> integer().
go(A) ->
    A@1 = A + 1,
    A@1.
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

-spec go(any()) -> integer().
go(A) ->
    A@1 = 1,
    A@1.
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
-export_type([box/0]).

-type box() :: {box, integer()}.

-spec factory(fun((I) -> M), I) -> M.
factory(F, I) ->
    F(I).

-spec main() -> box().
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

-spec main(any()) -> integer().
main(Args) ->
    case Args of
        _ ->
            A = 1,
            A
    end,
    A@1 = 2,
    A@1.
"#,
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
                    "#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([bitstring_discard/1]).

-spec bitstring_discard(bitstring()) -> boolean().
bitstring_discard(X) ->
    case X of
        <<_/utf8, Rest/binary>> ->
            true;

        _ ->
            false
    end.
"#,
    );

    assert_erl!(
        r#"
pub fn bitstring_discard(x) -> Bool {
 case x {
  <<_discardme:utf8, rest:binary>> -> True
   _ -> False
 }
}
                    "#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([bitstring_discard/1]).

-spec bitstring_discard(bitstring()) -> boolean().
bitstring_discard(X) ->
    case X of
        <<_/utf8, Rest/binary>> ->
            true;

        _ ->
            false
    end.
"#,
    );
}

#[test]
fn bit_string_declare_and_use_var() {
    assert_erl!(
        r#"pub fn go(x) {
  let <<name_size:8, name:binary-size(name_size)>> = x
  name
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([go/1]).

-spec go(bitstring()) -> bitstring().
go(X) ->
    <<Name_size:8, Name:Name_size/binary>> = X,
    Name.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

-spec main(any()) -> integer().
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

-spec main(any()) -> integer().
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

-spec main(boolean()) -> integer().
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

-spec main() -> integer().
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

-spec main() -> integer().
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

-spec main() -> integer().
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

-spec main() -> integer().
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

-spec main() -> integer().
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

-spec main() -> integer().
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

-spec main() -> integer().
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

-spec main() -> integer().
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

-spec main() -> integer().
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

-spec main(list(integer())) -> integer().
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

-spec main() -> integer().
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

-spec main() -> integer().
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

-spec main() -> integer().
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

-spec main() -> integer().
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

-spec main() -> integer().
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

-spec main() -> integer().
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
-export_type([test/0]).

-type test() :: {test, integer(), float()}.

-spec main() -> integer().
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

-spec main() -> integer().
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

-spec main() -> integer().
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

-spec main(list(boolean())) -> integer().
main(Args) ->
    case Args of
        [X] when X ->
            1;

        [X, _] when X ->
            1;

        _ ->
            0
    end.
"#,
    );
}

#[test]
fn todo_expr() {
    assert_erl!(
        r#"
pub fn main() {
  todo
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

-spec main() -> any().
main() ->
    erlang:error(#{gleam_error => todo,
                   message => <<"This has not yet been implemented"/utf8>>,
                   module => <<"the_app"/utf8>>,
                   function => <<"main"/utf8>>,
                   line => 3}).
"#,
    );

    assert_erl!(
        r#"
pub fn main() {
  todo("testing")
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/0]).

-spec main() -> any().
main() ->
    erlang:error(#{gleam_error => todo,
                   message => <<"testing"/utf8>>,
                   module => <<"the_app"/utf8>>,
                   function => <<"main"/utf8>>,
                   line => 3}).
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([get_age/1, get_name/1]).
-export_type([person/0]).

-type person() :: {person, binary(), integer()}.

-spec get_age(person()) -> integer().
get_age(Person) ->
    erlang:element(3, Person).

-spec get_name(person()) -> binary().
get_name(Person) ->
    erlang:element(2, Person).
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export_type([triple/0]).

-type triple() :: {triple, integer(), integer(), integer()}.

-spec main() -> integer().
main() ->
    Triple = {triple, 1, 2, 3},
    {triple, The_a, _, _} = Triple,
    The_a.
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

-export_type([triple/0]).

-type triple() :: {triple, integer(), integer(), integer()}.

-spec main() -> integer().
main() ->
    Triple = {triple, 1, 2, 3},
    {triple, _, The_b, _} = Triple,
    The_b.
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

-export_type([triple/0]).

-type triple() :: {triple, integer(), integer(), integer()}.

-spec main() -> integer().
main() ->
    Triple = {triple, 1, 2, 3},
    {triple, The_a, _, The_c} = Triple,
    The_c.
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

-export_type([triple/0]).

-type triple() :: {triple, integer(), integer(), integer()}.

-spec main() -> integer().
main() ->
    Triple = {triple, 1, 2, 3},
    case Triple of
        {triple, _, The_b, _} ->
            The_b
    end.
"#,
    );
}

#[test]
fn clever_pipe_rewriting() {
    // a |> b
    assert_erl!(
        r#"
pub fn apply(f: fn(a) -> b, a: a) { a |> f }
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([apply/2]).

-spec apply(fun((H) -> I), H) -> I.
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

-spec apply(fun((H, integer()) -> I), H) -> I.
apply(F, A) ->
    F(A, 1).
"#,
    );
}

#[test]
fn binop_parens() {
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

-spec main() -> integer().
main() ->
    A = (2 * (3 + 1)) div 2,
    B = (5 + ((3 div 3) * 2)) - (6 * 4),
    B.
"#,
    );
}

#[test]
fn try_expr() {
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

-spec main() -> {ok, integer()} | {error, any()}.
main() ->
    case {ok, 1} of
        {error, Gleam@try_error} -> {error, Gleam@try_error};
        {ok, A} ->
            case {ok, 2} of
                {error, Gleam@try_error@1} -> {error, Gleam@try_error@1};
                {ok, B} ->
                    {ok, A + B}
            end
    end.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export_type([fn_box/0]).

-type fn_box() :: {fn_box, fun((integer()) -> integer())}.

-spec main() -> integer().
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

-spec main() -> integer().
main() ->
    T = {fun(X) -> X end},
    (erlang:element(1, T))(5).
"#,
    );
}

#[test]
fn bit_strings() {
    assert_erl!(
        r#"fn main() {
  let a = 1
  let simple = <<1, a>>
  let complex = <<4:int-big, 5.0:little-float, 6:native-int>>
  let <<7:2, 8:size(3), b:binary-size(4)>> = <<1>>
  let <<c:8-unit(1), d:binary-size(2)-unit(2)>> = <<1>>

  simple
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec main() -> bitstring().
main() ->
    A = 1,
    Simple = <<1, A>>,
    Complex = <<4/integer-big, 5.0/little-float, 6/native-integer>>,
    <<7:2, 8:3, B:4/binary>> = <<1>>,
    <<C:8/unit:1, D:2/binary-unit:2>> = <<1>>,
    Simple.
"#,
    );

    assert_erl!(
        r#"fn x() { 2 }
fn main() {
  let a = -1
  let b = <<a:unit(2)-size(a * 2), a:size(3 + x())-unit(1)>>

  b
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec x() -> integer().
x() ->
    2.

-spec main() -> bitstring().
main() ->
    A = -1,
    B = <<A:(lists:max([(A * 2), 0]))/unit:2,
          A:(lists:max([(3 + x()), 0]))/unit:1>>,
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

-spec main() -> integer().
main() ->
    A = 1,
    <<B, 1>> = <<1, A>>,
    B.
"#,
    );

    assert_erl!(
        r#"fn main() {
  let a = <<"test":utf8>>
  let <<b:utf8_codepoint, "st":utf8>> = a
  b
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec main() -> integer().
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

-spec x() -> integer().
x() ->
    1.

-spec main() -> bitstring().
main() ->
    A = <<(x())/integer>>,
    A.
"#,
    );
}

#[test]
fn constants_in_guards() {
    assert_erl!(
        r#"
pub const string_value = "constant value"

pub fn main(arg) {
  case arg {
    _ if arg == string_value -> 1
    _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

-spec main(binary()) -> integer().
main(Arg) ->
    case Arg of
        _ when Arg =:= <<"constant value"/utf8>> ->
            1;

        _ ->
            0
    end.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

-spec main(bitstring()) -> integer().
main(Arg) ->
    case Arg of
        _ when Arg =:= <<1, "ok"/utf8, 3, 4:50>> ->
            1;

        _ ->
            0
    end.
"#,
    );

    assert_erl!(
        r#"
pub const constant = tuple(1, 2.0)

pub fn main(arg) {
  case arg {
    _ if arg == constant -> 1
    _ -> 0
  }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

-spec main({integer(), float()}) -> integer().
main(Arg) ->
    case Arg of
        _ when Arg =:= {1, 2.0} ->
            1;

        _ ->
            0
    end.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

-spec main(float()) -> integer().
main(Arg) ->
    case Arg of
        _ when Arg > 3.14 ->
            1;

        _ ->
            0
    end.
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

-spec main({{integer(), float(), binary()}, binary(), float(), integer()}) -> integer().
main(Arg) ->
    _ = [1, 2, 3],
    case Arg of
        {W, X, Y, Z} when (((W =:= {1, 2.0, <<"3"/utf8>>}) andalso (X =:= <<"constant value"/utf8>>)) andalso (Y > 3.14)) andalso (Z =:= 42) ->
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

-spec main(list(integer())) -> integer().
main(Arg) ->
    case Arg of
        _ when Arg =:= [1, 2, 3] ->
            1;

        _ ->
            0
    end.
"#,
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
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([test/0]).

-spec test() -> integer().
test() ->
    Duplicate_name = 1,
    case 1 of
        1 ->
            Duplicate_name@1 = Duplicate_name + 1,
            Duplicate_name@1;

        2 ->
            Duplicate_name@1 = Duplicate_name + 1,
            Duplicate_name@1
    end.
"#,
    );

    // Alternative patterns with a clause containing vars
    assert_erl!(
        r#"
pub fn test() {
  case Ok(1) {
    Ok(duplicate_name) | Error(duplicate_name) -> duplicate_name
  }
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([test/0]).

-spec test() -> integer().
test() ->
    case {ok, 1} of
        {ok, Duplicate_name} ->
            Duplicate_name;

        {error, Duplicate_name} ->
            Duplicate_name
    end.
"#,
    );

    // Alternative patterns with a guard clause containing vars
    assert_erl!(
        r#"
pub fn test() {
    let duplicate_name = 1

    case 1 {
        1 | 2 if duplicate_name == 1 -> duplicate_name
    }
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([test/0]).

-spec test() -> integer().
test() ->
    Duplicate_name = 1,
    case 1 of
        1 when Duplicate_name =:= 1 ->
            Duplicate_name;

        2 when Duplicate_name =:= 1 ->
            Duplicate_name
    end.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export([main/1]).

-spec main({ok, integer()} | {error, any()}) -> integer().
main(Arg) ->
    _ = {ok, 1},
    case Arg of
        _ when Arg =:= {ok, 1} ->
            1;

        _ ->
            0
    end.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export_type([person/0]).

-type person() :: {person, binary(), integer()}.

-spec main() -> person().
main() ->
    P = {person, <<"Quinn"/utf8>>, 27},
    New_p = erlang:setelement(3, P, 28),
    New_p.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export_type([person/0]).

-type person() :: {person, binary(), integer()}.

-spec main() -> person().
main() ->
    P = {person, <<"Quinn"/utf8>>, 27},
    New_p = erlang:setelement(3, P, erlang:element(3, P) + 1),
    New_p.
"#,
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
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-export_type([person/0]).

-type person() :: {person, binary(), integer()}.

-spec main() -> person().
main() ->
    P = {person, <<"Quinn"/utf8>>, 27},
    New_p = erlang:setelement(2, erlang:setelement(3, P, 28), <<"Riley"/utf8>>),
    New_p.
"#,
    );
}

#[test]
fn numbers_with_underscores() {
    assert_erl!(
        r#"
fn main() {
  100_000
  100_000.00101
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec main() -> float().
main() ->
    100000,
    100000.00101.
"#,
    );

    assert_erl!(
        r#"
const i = 100_000
const f = 100_000.00101
fn main() {
  i
  f
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec main() -> float().
main() ->
    100000,
    100000.00101.
"#,
    );

    assert_erl!(
        r#"
fn main() {
  let 100_000 = 1
  let 100_000.00101 = 1.
  1
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec main() -> integer().
main() ->
    100000 = 1,
    100000.00101 = 1.0,
    1.
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/777
#[test]
fn block_assignment() {
    assert_erl!(
        r#"
fn main() {
  let x = {
    1
    2
  }
  x
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec main() -> integer().
main() ->
    X = begin
        1,
        2
    end,
    X.
"#,
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

fn main() {
  id(id)
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec id(H) -> H.
id(X) ->
    X.

-spec main() -> fun((L) -> L).
main() ->
    id(fun id/1).
"#,
    );
}

#[test]
fn tuple_access_in_guard() {
    assert_erl!(
        r#"
fn main() {
    let key = 10
    let x = [tuple(10, 2), tuple(1, 2)]
    case x {
        [first, ..rest] if first.0 == key -> "ok"
        _ -> "ko"
    }
}
"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec main() -> binary().
main() ->
    Key = 10,
    X = [{10, 2}, {1, 2}],
    case X of
        [First | Rest] when erlang:element(1, First) =:= Key ->
            <<"ok"/utf8>>;

        _ ->
            <<"ko"/utf8>>
    end.
"#,
    );
}

#[test]
fn record_constants() {
    assert_erl!(
        "pub type Test { A }
const test = A
pub fn a() { A }",
        "-module(the_app).
-compile(no_auto_import).

-export([a/0]).
-export_type([test/0]).

-type test() :: a.

-spec a() -> test().
a() ->
    a.
"
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
}",
        "-module(the_app).
-compile(no_auto_import).

-export([a/1]).

-spec a(binary()) -> binary().
a(Name_) ->
    Name__ = Name_,
    Name = Name__,
    One_1 = 1,
    One1 = One_1,
    Name.
"
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
}",
        "-module(the_app).
-compile(no_auto_import).

-export([a/1]).

-spec a({ok, integer()} | {error, any()}) -> integer().
a(X) ->
    case X of
        {ok, 1 = Y} ->
            1;

        _ ->
            0
    end.
"
    );
}

#[test]
fn build_in_erlang_type_escaping() {
    assert_erl!(
        "pub external type Map",
        "-module(the_app).
-compile(no_auto_import).

-export_type([map_/0]).

-type map_() :: any().


"
    );
}

#[test]
fn allowed_string_escapes() {
    assert_erl!(
        r#"fn a() { "\n" "\r" "\t" "\\" "\"" "\\^" }"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec a() -> binary().
a() ->
    <<"\n"/utf8>>,
    <<"\r"/utf8>>,
    <<"\t"/utf8>>,
    <<"\\"/utf8>>,
    <<"\""/utf8>>,
    <<"\\^"/utf8>>.
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/952
#[test]
fn block_expr_into_pipe() {
    assert_erl!(
        r#"fn id(a) { a }
fn main() {
  {
    let x = 1
    x
  }
  |> id
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec id(H) -> H.
id(A) ->
    A.

-spec main() -> integer().
main() ->
    id(
        begin
            X = 1,
            X
        end
    ).
"#
    );
}

#[test]
fn assert() {
    // One var
    assert_erl!(
        r#"fn go() {
  assert Ok(y) = Ok(1)
  y
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec go() -> integer().
go() ->
    Y@1 = case {ok, 1} of
        {ok, Y} -> Y;
        Gleam@Assert ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => Gleam@Assert,
                           module => <<"the_app"/utf8>>,
                           function => <<"go"/utf8>>,
                           line => 2})
    end,
    Y@1.
"#,
    );

    // More vars
    assert_erl!(
        r#"fn go(x) {
  assert [1, a, b, c] = x
  [a, b, c]
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec go(list(integer())) -> list(integer()).
go(X) ->
    {A@1, B@1, C@1} = case X of
        [1, A, B, C] -> {A, B, C};
        Gleam@Assert ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => Gleam@Assert,
                           module => <<"the_app"/utf8>>,
                           function => <<"go"/utf8>>,
                           line => 2})
    end,
    [A@1, B@1, C@1].
"#,
    );

    // Pattern::Let
    assert_erl!(
        r#"fn go(x) {
  assert [1 as a, b, c] = x
  [a, b, c]
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec go(list(integer())) -> list(integer()).
go(X) ->
    {A@1, B@1, C@1} = case X of
        [1 = A, B, C] -> {A, B, C};
        Gleam@Assert ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => Gleam@Assert,
                           module => <<"the_app"/utf8>>,
                           function => <<"go"/utf8>>,
                           line => 2})
    end,
    [A@1, B@1, C@1].
"#,
    );

    // Following asserts use appropriate variable rewrites
    assert_erl!(
        r#"fn go() {
  assert Ok(y) = Ok(1)
  assert Ok(y) = Ok(1)
  y
}"#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec go() -> integer().
go() ->
    Y@1 = case {ok, 1} of
        {ok, Y} -> Y;
        Gleam@Assert ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => Gleam@Assert,
                           module => <<"the_app"/utf8>>,
                           function => <<"go"/utf8>>,
                           line => 2})
    end,
    Y@3 = case {ok, 1} of
        {ok, Y@2} -> Y@2;
        Gleam@Assert@1 ->
            erlang:error(#{gleam_error => assert,
                           message => <<"Assertion pattern match failed"/utf8>>,
                           value => Gleam@Assert@1,
                           module => <<"the_app"/utf8>>,
                           function => <<"go"/utf8>>,
                           line => 3})
    end,
    Y@3.
"#,
    );

    // TODO: patterns that are just vars don't render a case expression
}
