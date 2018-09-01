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
    {"(0.0)", "(Float)"},
    {"(:ok, 1)", "(Atom, Int)"},
    {"(:ok, 1, (1.0, \"\"))", "(Atom, Int, (Float, String))"}
  ],
  test_infer(Cases).

infer_let_test() ->
  Cases = [
    {"x = :unused 1", "Int"},
    {"x = :unused 1.1", "Float"},
    {"x = :unused (:ok, 1)", "(Atom, Int)"},
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
    {"fn(x) { (:ok, x) }", "fn(a) { (Atom, a) }"}
  ],
  test_infer(Cases).

infer_closure_call_test() ->
  Cases = [
    {"id = fn(x) { x } id(1)", "Int"},
    {"two = fn(x) { fn(y) { x } } fun = two(1) fun(:ok)", "Int"},
    % Pair
    {"fn(x, y) { (x, y) }",
     "fn(a, b) { (a, b) }"},
    % Apply
    {"fn(f) { fn(x) { f(x) } }",
    "fn(fn(a) { b }) { fn(a) { b } }"},
    % Curry
    {"fn(f) { fn(x) { fn(y) { f(x, y) } } }",
     "fn(fn(a, b) { c }) { fn(a) { fn(b) { c } } }"},
    % Const
    {"fn(x) { fn(y) { x } }",
     "fn(a) { fn(b) { a } }"},
    % Call
    {"fn(f) { f() }",
     "fn(fn() { a }) { a }"},
    % Twice
    {"fn(f, x) { f(f(x)) }",
     "fn(fn(a) { a }, a) { a }"}
  ],
  test_infer(Cases).

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

% ; ("fun x -> let y = fun z -> z in y", OK "forall[a b] a -> b -> b") *)
% ; ("let f = fun x -> x in let id = fun y -> y in eq(f, id)", OK "bool") *)
% ; ("let f = fun x -> x in let id = fun y -> y in eq_curry(f)(id)", OK "bool") *)
% ; ("let f = fun x -> x in eq(f, succ)", OK "bool") *)
% ; ("let f = fun x -> x in eq_curry(f)(succ)", OK "bool") *)
% ; ("let f = fun x -> x in pair(f(one), f(true))", OK "pair[int, bool]") *)
% ; ("fun f -> pair(f(one), f(true))", fail) *)
% ; ( "let f = fun x y -> let a = eq(x, y) in eq(x, y) in f" *)
%   , OK "forall[a] (a, a) -> bool" ) *)
% ; ( "let f = fun x y -> let a = eq_curry(x)(y) in eq_curry(x)(y) in f" *)
%   , OK "forall[a] (a, a) -> bool" ) *)
% ; ("id(id)", OK "forall[a] a -> a") *)
% ; ("choose(fun x y -> x, fun x y -> y)", OK "forall[a] (a, a) -> a") *)
% ; ("choose_curry(fun x y -> x)(fun x y -> y)", OK "forall[a] (a, a) -> a") *)
% ; ("let x = id in let y = let z = x(id) in z in y", OK "forall[a] a -> a") *)
% ; ("cons(id, nil)", OK "forall[a] list[a -> a]") *)
% ; ("cons_curry(id)(nil)", OK "forall[a] list[a -> a]") *)
% ; ( "let lst1 = cons(id, nil) in let lst2 = cons(succ, lst1) in lst2" *)
%   , OK "list[int -> int]" ) *)
% ; ( "cons_curry(id)(cons_curry(succ)(cons_curry(id)(nil)))" *)
%   , OK "list[int -> int]" ) *)
% ; ("plus(one, true)", error "cannot unify types int and bool") *)
% ; ("plus(one)", error "unexpected number of arguments") *)
% ; ("fun x -> let y = x in y", OK "forall[a] a -> a") *)
% ; ( "fun x -> let y = let z = x(fun x -> x) in z in y" *)
%   , OK "forall[a b] ((a -> a) -> b) -> b" ) *)
% ; ( "fun x -> fun y -> let x = x(y) in x(y)" *)
%   , OK "forall[a b] (a -> a -> b) -> a -> b" ) *)
% ; ("fun x -> let y = fun z -> x(z) in y", OK "forall[a b] (a -> b) -> a -> b") *)
% ; ("fun x -> let y = fun z -> x in y", OK "forall[a b] a -> b -> a") *)
% ; ( "fun x -> fun y -> let x = x(y) in fun x -> y(x)" *)
%   , OK "forall[a b c] ((a -> b) -> c) -> (a -> b) -> a -> b" ) *)
% ; ("fun x -> let y = x in y(y)", error "recursive types") *)
% ; ("fun x -> let y = fun z -> z in y(y)", OK "forall[a b] a -> b -> b") *)
% ; ("fun x -> x(x)", error "recursive types") *)
% ; ("one(id)", error "expected a function") *)
% ; ( "fun f -> let x = fun g y -> let _ = g(y) in eq(f, g) in x" *)
%   , OK "forall[a b] (a -> b) -> (a -> b, a) -> bool" ) *)
% ; ("let const = fun x -> fun y -> x in const", OK "forall[a b] a -> b -> a") *)
% ; ("let apply = fun f x -> f(x) in apply", OK "forall[a b] (a -> b, a) -> b") *)
% ; ( "let apply_curry = fun f -> fun x -> f(x) in apply_curry" *)
%   , OK "forall[a b] (a -> b) -> a -> b" ) ] *)
