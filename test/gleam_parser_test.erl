-module(gleam_parser_test).
-include_lib("eunit/include/eunit.hrl").

tokens(Code)  -> element(2, gleam_tokenizer:string(Code)).
parse(Tokens) -> element(2, gleam_parser:parse(Tokens)).

-define(assertAST(Code, Tokens),
        ?assertMatch(Tokens, parse(tokens(Code)))).

literal_test() ->
  ?assertAST("1",   [1]),
  ?assertAST("1.2", [1.2]),
  ?assertAST(":ok", [ok]),
  ?assertAST("\"Hello world\"",  [<<"Hello world">>]).

list_test() ->
  ?assertAST("[]",        [[]]),
  ?assertAST("[100]",     [[100]]),
  ?assertAST("[  200,]",  [[200]]),
  ?assertAST("[2, 4, 8]", [[2, 4, 8]]),
  ?assertAST("[2,10, ]",  [[2, 10]]).
