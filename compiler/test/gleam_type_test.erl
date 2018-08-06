-module(gleam_type_test).
-include_lib("eunit/include/eunit.hrl").

-include("gleam_records.hrl").

infer(Source) ->
  {ok, Tokens, _} = gleam_tokenizer:string(Source),
  {ok, [Ast]} = gleam_parser:parse(Tokens),
  gleam_type:infer(Ast).

infer_int_test() ->
  Type = #type_const{type = int},
  {ok, Ast} = infer("1"),
  ?assertEqual(Type, gleam_type:fetch(Ast)).

infer_atom_test() ->
  Type = #type_const{type = atom},
  {ok, Ast} = infer(":ok"),
  ?assertEqual(Type, gleam_type:fetch(Ast)).

infer_float_test() ->
  Type = #type_const{type = float},
  {ok, Ast} = infer("1.0"),
  ?assertEqual(Type, gleam_type:fetch(Ast)).

infer_string_test() ->
  Type = #type_const{type = string},
  {ok, Ast} = infer("\"123\""),
  ?assertEqual(Type, gleam_type:fetch(Ast)).

infer_tuple_test() ->
  Type = #type_tuple{elems = [#type_const{type = atom},
                              #type_const{type = int},
                              #type_tuple{elems = [#type_const{type = float},
                                                   #type_const{type = string}]}]},
  {ok, Ast} = infer("(:ok, 1, (1.0, \"\"))"),
  ?assertEqual(Type, gleam_type:fetch(Ast)).

infer_unused_let_test() ->
  Type1 = #type_const{type = int},
  {ok, Ast1} = infer("x = :unused 1"),
  ?assertEqual(Type1, gleam_type:fetch(Ast1)),

  Type2 = #type_const{type = float},
  {ok, Ast2} = infer("x = :unused 1.1"),
  ?assertEqual(Type2, gleam_type:fetch(Ast2)),

  Type3 = #type_tuple{elems = [#type_const{type = atom}, #type_const{type = int}]},
  {ok, Ast3} = infer("x = :unused (:ok, 1)"),
  ?assertEqual(Type3, gleam_type:fetch(Ast3)).
