-module(gleam_ast_example_test).
-include_lib("eunit/include/eunit.hrl").

file_to_ast(FileName) ->
  {ok, Data} = file:read_file(FileName),
  Source = binary_to_list(Data),
  {ok, Tokens, _} = gleam_tokenizer:string(Source),
  {ok, AST} = gleam_parser:parse(Tokens),
  AST.

clauses_test() ->
  AST = file_to_ast("examples/clauses.glm"),
  {ok, ExpectedAST} = file:consult("examples/clauses.ast.erl"),
  ?assertEqual(hd(ExpectedAST), AST).
