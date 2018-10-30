-module(gleam_type_test).
-include_lib("eunit/include/eunit.hrl").

-include("gleam_records.hrl").

infer(Source) ->
  {ok, Tokens, _} = gleam_tokenizer:string(Source),
  {ok, Ast} = gleam_parser:parse(Tokens),
  gleam_type:infer(Ast).

test_infer(Cases) ->
  TestCase =
    fun({Src, Type}) ->
      {ok, Ast} = infer(Src),
      ActualType = gleam_type:fetch(Ast),
      ?assertEqual(Type, gleam_type:type_to_string(ActualType))
    end,
  lists:foreach(TestCase, Cases).

cannot_unify_test() ->
  Cases = [
    "1 +. 1",

    "{1, 1} + 1",

    "1 + 2.0",

    "1 == 2.0",

    "[1, 2.0]",

    "[1, 2, 3, 4, 5, 'six']",

    "{'ok', 1} != {'ok', 1, 'extra'}",

    "{} == {a = 1}",

    "1 == {a = 1}",

    "{a = 1} != 1",

    "test whatever { 1 == '1' }",

    "enum A = | A "
    "enum B = | B "
    "fn run() { A == B }"
    ,

    "enum A = | A(Int) "
    "fn run() { A(1.0) }"
  ],
  Test =
    fun(Src) ->
      Result = infer(Src),
      ?assertMatch({error, {cannot_unify, _}}, Result)
    end,
  lists:foreach(Test, Cases).

infer_undefined_var_test() ->
  ?assertEqual(infer("one"),
               {error, {var_not_found, #ast_var{name = "one"}}}),
  ?assertEqual(infer("x = x x"),
               {error, {var_not_found, #ast_var{name = "x"}}}).

infer_const_test() ->
  Cases = [
    {"1", "Int"},
    {"'ok'", "Atom"},
    {"1.0", "Float"},
    {"\"123\"", "String"},
    {"one = 1 one", "Int"}
  ],
  test_infer(Cases).

infer_tuple_test() ->
  Cases = [
    {"{0.0}", "Tuple(Float)"},
    {"{'ok', 1}", "Tuple(Atom, Int)"},
    {"{'ok', 1, {1.0, \"\"}}", "Tuple(Atom, Int, Tuple(Float, String))"}
  ],
  test_infer(Cases).

infer_assignment_test() ->
  Cases = [
    {"x = 'unused' 1", "Int"},
    {"x = 'unused' 1.1", "Float"},
    {"x = 'unused' {'ok', 1}", "Tuple(Atom, Int)"},
    {"x = 'ok' x", "Atom"},
    {"x = 5 y = x y", "Int"}
  ],
  test_infer(Cases).

infer_pattern_assignment_test() ->
  Cases = [
    {
     "pub enum Box = | Box(String) "
     "pub fn go(x) { Box(s) = x s }"
     ,
     "module {"
     " fn go(Box) -> String"
     "}"
    },

    {
     "fn(x) { {a, b} = x a + b }"
     ,
     "fn(Tuple(Int, Int)) -> Int"
    }
  ],
  test_infer(Cases).

infer_unknown_var_test() ->
  ?assertEqual({error, {var_not_found, #ast_var{name = "something"}}},
               infer("something")).

infer_fn_test() ->
  Cases = [
    {"fn() { 1 }", "fn() -> Int"},
    {"fn() { 1.1 }", "fn() -> Float"},
    {"fn(x) { 1.1 }", "fn(a) -> Float"},
    {"fn(x) { x }", "fn(a) -> a"},
    {"x = fn(x) { 1.1 } x", "fn(a) -> Float"},
    {"fn(x, y, z) { 1 }", "fn(a, b, c) -> Int"},
    {"fn(x) { y = x y }", "fn(a) -> a"},
    {"fn(x) { {'ok', x} }", "fn(a) -> Tuple(Atom, a)"}
  ],
  test_infer(Cases).

infer_fn_call_test() ->
  Cases = [
    {"id = fn(x) { x } id(1)", "Int"},
    {"two = fn(x) { fn(y) { x } } fun = two(1) fun('ok')", "Int"},
    % Apply
    {"fn(f, x) { f(x) }",
    "fn(fn(a) -> b, a) -> b"},
    % Apply curried
    {"fn(f) { fn(x) { f(x) } }",
    "fn(fn(a) -> b) -> fn(a) -> b"},
    % Curry
    {"fn(f) { fn(x) { fn(y) { f(x, y) } } }",
     "fn(fn(a, b) -> c) -> fn(a) -> fn(b) -> c"},
    % Uncurry
    {"fn(f) { fn(x, y) { ff = f(x) ff(y) } }",
     "fn(fn(a) -> fn(b) -> c) -> fn(a, b) -> c"},
    % Const
    {"fn(x) { fn(y) { x } }",
     "fn(a) -> fn(b) -> a"},
    % Call
    {"fn(f) { f() }",
     "fn(fn() -> a) -> a"},
    % Twice
    {"fn(f, x) { f(f(x)) }",
     "fn(fn(a) -> a, a) -> a"},
    % Recursive id
    {"fn(x) { y = fn(z) { z } y(y) }",
     "fn(a) -> fn(b) -> b"},
    % Pair
    {"fn(x, y) { {x, y} }",
     "fn(a, b) -> Tuple(a, b)"},
    % Pair of one
    {"fn(x) { {x, x} }",
     "fn(a) -> Tuple(a, a)"},
    % Really funky pointless thing
    {"id = fn(a) { a } fn(x) { x(id) }",
     "fn(fn(fn(a) -> a) -> b) -> b"}
  ],
  test_infer(Cases).
% ; ( "fun x -> let y = let z = x(fun x -> x) in z in y" *)
%   , OK "forall[a b] ((a -> a) -> b) -> b" ) *)

infer_not_a_function_test() ->
  ?assertEqual({error, {not_a_function, #type_const{type = "Int"}}},
               infer("x = 1 x(2)")).

infer_wrong_function_arity_test() ->
  Error = {incorrect_number_of_arguments,
           #type_fn{args = [],
                      return = #type_const{type = "Int"}}},
  ?assertEqual({error, Error},
               infer("f = fn() { 1 } f(2)")).

infer_recursive_type_error_test() ->
  ?assertEqual({error, recursive_types},
               infer("fn(x) { y = x y(y) }")).

operators_test() ->
  Cases = [
    {"1 + 1", "Int"},
    {"1 - 1", "Int"},
    {"1 * 1", "Int"},
    {"1 / 1", "Int"},
    {"1.0 +. 1.0", "Float"},
    {"1.0 -. 1.0", "Float"},
    {"1.0 *. 1.0", "Float"},
    {"1.0 /. 1.0", "Float"},
    {"fn(a, b) { a + b }", "fn(Int, Int) -> Int"},
    {"inc = fn(a) { a + 1 } 1 |> inc |> inc", "Int"}
  ],
  test_infer(Cases).

equality_test() ->
  Cases = [
    {"1 == 1", "Bool"},
    {"1.0 == 2.0", "Bool"},
    {"'ok' == 'ko'", "Bool"},
    {"{'ok', 1} == {'ko', 2}", "Bool"},
    {"1 != 1", "Bool"},
    {"1.0 != 2.0", "Bool"},
    {"'ok' != 'ko'", "Bool"},
    {"{'ok', 1} != {'ko', 2}", "Bool"},
    {"x = 1 x == x", "Bool"},
    {"id = fn(x) { x } id == id", "Bool"},
    {"id1 = fn(x) { x } id2 = fn(x) { x } id1 == id2", "Bool"},
    {"id = fn(x) { x } inc = fn(x) { x + 1 } id == inc", "Bool"}
  ],
  test_infer(Cases).

list_test() ->
  Cases = [
    {"[]", "List(a)"},
    {"[1]", "List(Int)"},
    {"[1, 2, 3]", "List(Int)"},
    {"[[]]", "List(List(a))"},
    {"[[1.0, 2.0]]", "List(List(Float))"},
    {"[fn(x) { x }]", "List(fn(a) -> a)"},
    {"[fn(x) { x + 1 }]", "List(fn(Int) -> Int)"},
    {"[{[], []}]", "List(Tuple(List(a), List(b)))"},
    {"[fn(x) { x }, fn(x) { x + 1 }]", "List(fn(Int) -> Int)"},
    {"[fn(x) { x + 1 }, fn(x) { x }]", "List(fn(Int) -> Int)"},
    {"[[], []]", "List(List(a))"},
    {"[[], ['ok']]", "List(List(Atom))"},
    {"1 :: 2 :: []", "List(Int)"},
    {"fn(x) { x } :: []", "List(fn(a) -> a)"},
    {"f = fn(x) { x } [f, f]", "List(fn(a) -> a)"},
    {"x = 1 :: [] 2 :: x", "List(Int)"}
  ],
  test_infer(Cases).

record_test() ->
  Cases = [
    {"{}", "{}"},
    {"{a = 1}", "{a = Int}"},
    {"{a = 1, b = 2}", "{a = Int, b = Int}"},
    {"{a = 1, b = 2.0, c = -1}", "{a = Int, b = Float, c = Int}"},
    {"{a = {a = 'ok'}}", "{a = {a = Atom}}"},
    {"{} == {}", "Bool"},
    {"{a = 1} == {a = 2}", "Bool"},
    {"{a = 1, b = 1} == {a = 1, b = 1}", "Bool"},
    {"{a = fn(x) { x }} == {a = fn(a) { a }}", "Bool"},
    % Different field order
    {"{b = 1, a = 1} == {a = 1, b = 1}", "Bool"}
    % % Additional fields on the right hand side
    % {"{b = 1, a = 1} == {a = 1, b = 1, c = 1}", "Bool"}
  ],
  test_infer(Cases).

record_failures_test() ->
  ?assertEqual({error, {row_does_not_contain_label, "a"}},
               infer("{}.a")),
  ?assertEqual({error, {row_does_not_contain_label, "a"}},
               infer("{b = 1, a = 1} == {b = 2}")).

record_select_test() ->
  Cases = [
    {"{a = 1, b = 2.0}.b",
     "Float"},
    {"r = {a = 1, b = 2} r.a",
     "Int"},
    {"r = {a = 1, b = 2} r.a + r.b",
     "Int"},
    {"fn(x) { x.t }",
     "fn({a | t = b}) -> b"},
    {"f = fn(x) { x } r = {a = f} r.a",
     "fn(a) -> a"},
    {"r = {a = fn(x) { x }} r.a",
     "fn(a) -> a"},
    {"r = {a = fn(x) { x }, b = fn(x) { x }} [r.a, r.b]",
     "List(fn(a) -> a)"},
    {"f = fn(x) { x.t } f({ t = 1 })",
     "Int"},
    {"f = fn(x) { x.t(1) } f({t = fn(x) { x + 1 }})",
     "Int"},
    {"{a = 1, b = 2.0}.a",
     "Int"}
  ],
  test_infer(Cases).

record_extend_test() ->
  Cases = [
    {
     "{ {a = 1.0} | a = 1}",
     "{a = Float, a = Int}" % FIXME: errrr
    },
    {
     "a = {} {a | b = 1}",
     "{b = Int}"
    },
    {
     "a = {b = 1} {a | b = 1.0}.b",
     "Float"
    },
    {
     "fn(r) { { r | x = 1 } }",
     "fn({ a }) -> {a | x = Int}"
    },
    {
     "fn(r) { r.x }",
     "fn({a | x = b}) -> b"
    },
    {
     "fn(r) { r.x + 1 }",
     "fn({a | x = Int}) -> Int"
    },
    {
     "{ {} | a = 1}",
     "{a = Int}"
    }
  ],
  test_infer(Cases).

sequence_test() ->
  Cases = [
    {
     "1.0 2 'three'",
     "Atom"
    }
  ],
  test_infer(Cases).

fn_call_test() ->
  Cases = [
    {
     "inc = fn(x) { x + 1 } inc.(1)",
     "Int"
    }
  ],
  test_infer(Cases).

underscore_fn_test() ->
  Cases = [
    {
     "add = fn(x, y) { x + y } add(_, 2)",
     "fn(Int) -> Int"
    }
  ],
  test_infer(Cases).

throw_raise_test() ->
  Cases = [
    {
    "throw(1)",
     "a"
    },
    {
     "raise('ok')",
     "a"
    },
    {
    "fn() { throw(1) }",
     "fn() -> a"
    },
    {
    "x = fn() { throw(1) } x",
     "fn() -> a"
    }
  ],
  test_infer(Cases).

cast_test() ->
  Cases = [
    {
     "case 1 { | a -> a }",
     "Int"
    },

    {
     "case \"\" { | a -> 0.0 }",
     "Float"
    },

    {
     "case 'ok' { | 'ok' -> 0 | 'error' -> 1 }",
     "Int"
    },

    {
     "fn(x) { case x { | 1 -> 'ok' } }",
     "fn(Int) -> Atom"
    },

    {
     "fn(x, y) { case x { | 1 -> y | a -> {'ok', 2} } }",
     "fn(Int, Tuple(Atom, Int)) -> Tuple(Atom, Int)"
    },

    {
     "case {'ok', 1} { | {'ok', int} -> int | _other -> 0 }",
     "Int"
    },

    {
     "case [] { | [] -> 0 | x :: xs -> x }",
     "Int"
    },

    {
     "case 1 { | 1 -> 'one' | 2 -> 'two' | 3 -> 'dunnno' }",
     "Atom"
    }
  ],
  test_infer(Cases).

module_test() ->
  Cases = [
    {
     "fn private() { 1 }"
     "pub fn public() { 1 }"
     ,
     "module {"
     " fn public() -> Int"
     "}"
    },

    {
     "fn id(x) { x }"
     "pub fn int() { id(1) }"
     "pub fn float() { id(1.0) }"
     ,
     "module {"
     " fn int() -> Int"
     " fn float() -> Float"
     "}"
    },

    {
     "enum Is = | Yes | No "
     "pub fn yes() { Yes }"
     "pub fn no() { No }"
     ,
     "module {"
     " fn yes() -> Is"
     " fn no() -> Is"
     "}"
    },

    {
     "enum Num = | I(Int) "
     "pub fn num(n) { I(n) }"
     ,
     "module {"
     " fn num(Int) -> Num"
     "}"
    },

    {
     "pub fn status() { 'ok' }"
     "pub fn list_of(x) { [x] }"
     "pub fn get_age(person) { person.age }"
     "test whatever { 'ok' }"
     ,
     "module {"
     " fn status() -> Atom"
     " fn list_of(a) -> List(a)"
     " fn get_age({b | age = c}) -> c"
     "}"
    }
  ],
  test_infer(Cases).

enum_test() ->
  Cases = [
    {
     "enum Box(a) = | Box(a) "
     "pub fn int() { Box(1) }"
     "pub fn float() { Box(1.0) }"
     ,
     "module {"
     " fn int() -> Box(Int)"
     " fn float() -> Box(Float)"
     "}"
    },

    {
     "pub enum I = | I(Int)"
     "pub fn open(x) { case x { | I(i) -> i  } }"
     ,
     "module {"
     " fn open(I) -> Int"
     "}"
    }
  ],
  test_infer(Cases).

% TODO: Test enums that won't type check
% TODO: Test that enum constructors with type vars must have the type vars at the top level

% Depends on tuple destructuring
% ; ("choose(fun x y -> x, fun x y -> y)", OK "forall[a] (a, a) -> a") *)
% ; ("choose_curry(fun x y -> x)(fun x y -> y)", OK "forall[a] (a, a) -> a") *)
