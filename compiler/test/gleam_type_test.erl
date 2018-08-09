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
      ?assertEqual(Type, gleam_type:fetch(Ast))
    end,
  lists:foreach(TestCase, Cases).

infer_const_test() ->
  Cases = [
    {"1", #type_const{type = int}},
    {":ok", #type_const{type = atom}},
    {"1.0", #type_const{type = float}},
    {"\"123\"", #type_const{type = string}}
  ],
  test_infer(Cases).

infer_tuple_test() ->
  Cases = [
    {"(:ok, 1, (1.0, \"\"))",
     #type_tuple{elems = [#type_const{type = atom},
                          #type_const{type = int},
                          #type_tuple{elems = [#type_const{type = float},
                                               #type_const{type = string}]}]}}
  ],
  test_infer(Cases).

infer_let_test() ->
  Cases = [
    {"x = :unused 1", #type_const{type = int}},
    {"x = :unused 1.1", #type_const{type = float}},
    {"x = :unused (:ok, 1)",
     #type_tuple{elems = [#type_const{type = atom}, #type_const{type = int}]}},
    {"x = :ok x", #type_const{type = atom}},
    {"x = 5 y = x y", #type_const{type = int}}
  ],
  test_infer(Cases).

infer_unknown_var_test() ->
  ?assertEqual({error, {var_not_found, #ast_var{name = "something"}}},
               infer("something")).

