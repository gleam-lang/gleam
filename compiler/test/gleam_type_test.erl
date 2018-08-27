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

infer_const_test() ->
  Cases = [
    {"1", "Int"},
    {":ok", "Atom"},
    {"1.0", "Float"},
    {"\"123\"", "String"}
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
    {"fn(x) { y = x y }", "fn(a) { a }"}
  ],
  test_infer(Cases).

infer_closure_call_test() ->
  Cases = [
    {"id = fn(x) { x } id(1)", "Int"},
    {"two = fn(x) { fn(y) { x } } fun = two(1) fun(:ok)", "Int"}
  ],
  test_infer(Cases).
