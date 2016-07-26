-module(gleam_function_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/gleam_records.hrl").

private_function_test() ->
  AST = {function, [{line, 1}], private, size, [{def, [], [], [big]}]},
  Function = gleam_function:from_ast(AST),
  Expected = #gleam_function
  { name = size
  , arity = 0
  , publicity = private
  , clauses = [{[], [big]}]
  },
  ?assertEqual({ok, Expected}, Function).

public_function_test() ->
  AST = {function, [{line, 1}], public, size, [{def, [], [], [big]}]},
  Function = gleam_function:from_ast(AST),
  Expected = #gleam_function
  { name = size
  , arity = 0
  , publicity = public
  , clauses = [{[], [big]}]
  },
  ?assertEqual({ok, Expected}, Function).

single_clause_function_test() ->
  AST = {function, [{line, 1}], public, word,
         [{def, [], [1], [<<"one">>]}]},
  Function = gleam_function:from_ast(AST),
  Expected = #gleam_function
  { name = word
  , arity = 1
  , publicity = public
  , clauses = [{[1], [<<"one">>]}]
  },
  ?assertEqual({ok, Expected}, Function).

multi_clause_function_test() ->
  AST = {function, [{line, 1}], public, word,
         [ {def, [], [1], [<<"one">>]}
         , {def, [], [2], [<<"two">>]}
         , {def, [], [3], [<<"three">>]}
         , {def, [], [4], [<<"four">>]}
         , {def, [], [{variable, [{line, 2}], x}], [<<"Dunno!">>]}
         ]},
  Function = gleam_function:from_ast(AST),
  Expected = #gleam_function
  { name = word
  , arity = 1
  , publicity = public
  , clauses =
    [ {[1], [<<"one">>]}
    , {[2], [<<"two">>]}
    , {[3], [<<"three">>]}
    , {[4], [<<"four">>]}
    , {[{variable, [{line, 2}], x}], [<<"Dunno!">>]}
    ]
  },
  ?assertEqual({ok, Expected}, Function).

mismatched_arity_test() ->
  AST = {function, [{line, 1}], public, word,
         [ {def, [], [1], [one]}
         , {def, [], [1, 2], [two]}
         ]},
  Function = gleam_function:from_ast(AST),
  ?assertEqual({error, arity_mismatch}, Function).
