-module(gleam_infer_test).
-include_lib("eunit/include/eunit.hrl").

-include("gleam_records.hrl").

type(Source) ->
  {ok, Tokens, _} = gleam_tokenizer:string(Source),
  {ok, [AST]} = gleam_parser:parse(Tokens),
  gleam_infer:infer(AST).

inter_int_test() ->
  Type = #type_const{type = int},
  Ast = #ast_int{value = 1, type = {ok, Type}},
  ?assertEqual({ok, Ast}, type("1")).

inter_atom_test() ->
  Type = #type_const{type = atom},
  Ast = #ast_atom{value = "ok", type = {ok, Type}},
  ?assertEqual({ok, Ast}, type(":ok")).

inter_float_test() ->
  Type = #type_const{type = float},
  Ast = #ast_float{value = 1.0, type = {ok, Type}},
  ?assertEqual({ok, Ast}, type("1.0")).

inter_string_test() ->
  Type = #type_const{type = string},
  Ast = #ast_string{value = <<"Hi">>, type = {ok, Type}},
  ?assertEqual({ok, Ast}, type("\"Hi\"")).
