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
    "[1, 2, 3, 4, 5, :six]",
    "{:ok, 1} != {:ok, 1, :extra}"
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
    {":ok", "Atom"},
    {"1.0", "Float"},
    {"\"123\"", "String"},
    {"one = 1 one", "Int"}
  ],
  test_infer(Cases).

infer_tuple_test() ->
  Cases = [
    {"{0.0}", "Tuple(Float)"},
    {"{:ok, 1}", "Tuple(Atom, Int)"},
    {"{:ok, 1, {1.0, \"\"}}", "Tuple(Atom, Int, Tuple(Float, String))"}
  ],
  test_infer(Cases).

infer_let_test() ->
  Cases = [
    {"x = :unused 1", "Int"},
    {"x = :unused 1.1", "Float"},
    {"x = :unused {:ok, 1}", "Tuple(Atom, Int)"},
    {"x = :ok x", "Atom"},
    {"x = 5 y = x y", "Int"}
  ],
  test_infer(Cases).

infer_unknown_var_test() ->
  ?assertEqual({error, {var_not_found, #ast_var{name = "something"}}},
               infer("something")).

infer_closure_test() ->
  Cases = [
    {"fn() { 1 }", "fn() { Int }"},
    {"fn() { 1.1 }", "fn() { Float }"},
    {"fn(x) { 1.1 }", "fn(a) { Float }"},
    {"fn(x) { x }", "fn(a) { a }"},
    {"x = fn(x) { 1.1 } x", "fn(a) { Float }"},
    {"fn(x, y, z) { 1 }", "fn(a, b, c) { Int }"},
    {"fn(x) { y = x y }", "fn(a) { a }"},
    {"fn(x) { {:ok, x} }", "fn(a) { Tuple(Atom, a) }"}
  ],
  test_infer(Cases).

infer_closure_call_test() ->
  Cases = [
    {"id = fn(x) { x } id(1)", "Int"},
    {"two = fn(x) { fn(y) { x } } fun = two(1) fun(:ok)", "Int"},
    % Apply
    {"fn(f, x) { f(x) }",
    "fn(fn(a) { b }, a) { b }"},
    % Apply curried
    {"fn(f) { fn(x) { f(x) } }",
    "fn(fn(a) { b }) { fn(a) { b } }"},
    % Curry
    {"fn(f) { fn(x) { fn(y) { f(x, y) } } }",
     "fn(fn(a, b) { c }) { fn(a) { fn(b) { c } } }"},
    % Uncurry
    {"fn(f) { fn(x, y) { ff = f(x) ff(y) } }",
     "fn(fn(a) { fn(b) { c } }) { fn(a, b) { c } }"},
    % Const
    {"fn(x) { fn(y) { x } }",
     "fn(a) { fn(b) { a } }"},
    % Call
    {"fn(f) { f() }",
     "fn(fn() { a }) { a }"},
    % Twice
    {"fn(f, x) { f(f(x)) }",
     "fn(fn(a) { a }, a) { a }"},
    % Recursive id
    {"fn(x) { y = fn(z) { z } y(y) }",
     "fn(a) { fn(b) { b } }"},
    % Pair
    {"fn(x, y) { {x, y} }",
     "fn(a, b) { Tuple(a, b) }"},
    % Pair of one
    {"fn(x) { {x, x} }",
     "fn(a) { Tuple(a, a) }"},
    % Really funky pointless thing
    {"id = fn(a) { a } fn(x) { x(id) }",
     "fn(fn(fn(a) { a }) { b }) { b }"}
  ],
  test_infer(Cases).
% ; ( "fun x -> let y = let z = x(fun x -> x) in z in y" *)
%   , OK "forall[a b] ((a -> a) -> b) -> b" ) *)

infer_not_a_function_test() ->
  ?assertEqual({error, {not_a_function, #type_const{type = "Int"}}},
               infer("x = 1 x(2)")).

infer_wrong_function_arity_test() ->
  Error = {incorrect_number_of_arguments,
           #type_func{args = [],
                      return = #type_const{type = "Int"}}},
  ?assertEqual({error, Error},
               infer("f = fn() { 1 } f(2)")).

infer_recursive_type_error_test() ->
  ?assertEqual({error, recursive_types},
               infer("fn(x) { y = x y(y) }")).

math_operators_test() ->
  Cases = [
    {"1 + 1", "Int"},
    {"1 - 1", "Int"},
    {"1 * 1", "Int"},
    {"1 / 1", "Int"},
    {"1.0 +. 1.0", "Float"},
    {"1.0 -. 1.0", "Float"},
    {"1.0 *. 1.0", "Float"},
    {"1.0 /. 1.0", "Float"},
    {"fn(a, b) { a + b }", "fn(Int, Int) { Int }"}
  ],
  test_infer(Cases).

equality_test() ->
  Cases = [
    {"1 == 1", "Bool"},
    {"1.0 == 2.0", "Bool"},
    {":ok == :ko", "Bool"},
    {"{:ok, 1} == {:ko, 2}", "Bool"},
    {"1 != 1", "Bool"},
    {"1.0 != 2.0", "Bool"},
    {":ok != :ko", "Bool"},
    {"{:ok, 1} != {:ko, 2}", "Bool"},
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
    {"[fn(x) { x }]", "List(fn(a) { a })"},
    {"[fn(x) { x + 1 }]", "List(fn(Int) { Int })"},
    {"[{[], []}]", "List(Tuple(List(a), List(b)))"},
    {"[fn(x) { x }, fn(x) { x + 1 }]", "List(fn(Int) { Int })"},
    {"[fn(x) { x + 1 }, fn(x) { x }]", "List(fn(Int) { Int })"},
    {"[[], []]", "List(List(a))"},
    {"[[], [:ok]]", "List(List(Atom))"},
    {"1 :: 2 :: []", "List(Int)"},
    {"fn(x) { x } :: []", "List(fn(a) { a })"},
    {"x = 1 :: [] 2 :: x", "List(Int)"}
  ],
  test_infer(Cases).

% ; ("let f = fun x -> x in pair(f(one), f(true))", OK "pair[int, bool]") *)
% ; ("fun f -> pair(f(one), f(true))", fail) *)

% Depends on equality
% ; ("let f = fun x -> x in let id = fun y -> y in eq_curry(f)(id)", OK "bool") *)
% ; ("let f = fun x -> x in eq(f, succ)", OK "bool") *)
% ; ("let f = fun x -> x in eq_curry(f)(succ)", OK "bool") *)
% ; ( "let f = fun x y -> let a = eq(x, y) in eq(x, y) in f" *)
%   , OK "forall[a] (a, a) -> bool" ) *)
% ; ( "fun f -> let x = fun g y -> let _ = g(y) in eq(f, g) in x" *)
%   , OK "forall[a b] (a -> b) -> (a -> b, a) -> bool" ) *)

% Depends on tuple destructuring
% ; ("choose(fun x y -> x, fun x y -> y)", OK "forall[a] (a, a) -> a") *)
% ; ("choose_curry(fun x y -> x)(fun x y -> y)", OK "forall[a] (a, a) -> a") *)

% Depends on functions already defined in env
% ; ("let x = id in let y = let z = x(id) in z in y", OK "forall[a] a -> a") *)

% ; ( "fun x -> fun y -> let x = x(y) in x(y)" *)
%   , OK "forall[a b] (a -> a -> b) -> a -> b" ) *)
% ; ("fun x -> let y = fun z -> x(z) in y", OK "forall[a b] (a -> b) -> a -> b") *)
% ; ("fun x -> let y = fun z -> x in y", OK "forall[a b] a -> b -> a") *)
% ; ( "fun x -> fun y -> let x = x(y) in fun x -> y(x)" *)
%   , OK "forall[a b c] ((a -> b) -> c) -> (a -> b) -> a -> b" ) *)
