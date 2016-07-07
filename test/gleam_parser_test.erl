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
  ?assertAST("\"Hello world\"", [<<"Hello world">>]).

list_test() ->
  ?assertAST("[]",        [[]]),
  ?assertAST("[100]",     [[100]]),
  ?assertAST("[  200,]",  [[200]]),
  ?assertAST("[2, 4, 8]", [[2, 4, 8]]),
  ?assertAST("[self()]",  [[{self, _, []}]]),
  ?assertAST("[2,10, ]",  [[2, 10]]).

tuple_test() ->
  ?assertAST("()",            [{}]),
  ?assertAST("(:54)",         [{'54'}]),
  ?assertAST("(  200,)",      [{200}]),
  ?assertAST("(:ok, 8)",      [{ok, 8}]),
  ?assertAST("(self())",      [{{self, _, []}}]),
  ?assertAST("(\"i\", 10, )", [{<<"i">>, 10}]).

nesting_test() ->
  ?assertAST("[(), ()]", [[{}, {}]]),
  ?assertAST("[[], []]", [[[], []]]),
  ?assertAST("((), ())", [{{}, {}}]),
  ?assertAST("[(:ok, 1), (:error, [()])]",
             [[{ok, 1}, {error, [{}]}]]).

call_test() ->
  ?assertAST("self()",
             [{self, [{line, 1}], []}]),
  ?assertAST("list_to_tuple([1, 2, 3])",
             [{list_to_tuple, _, [[1, 2, 3]]}]),
  ?assertAST("add(4, 3, 2)",
             [{add, _, [4, 3, 2]}]),
  ?assertAST("parse(tokens(:ok))",
             [{parse, _, [{tokens, _, [ok]}]}]).

mod_call_test() ->
  ?assertAST("erlang.time()",
             [{'.', [{line, 1}], [erlang, time], []}]),
  ?assertAST("gen_server.module_info()",
             [{'.', _, [gen_server, module_info], []}]),
  ?assertAST("lists.max([5, 10])",
             [{'.', _, [lists, max], [[5, 10]]}]).

module_test() ->
  ?assertAST("module my_mod",
             [{module, [{line, 1}], my_mod}]),
  ?assertAST("module ppool",
             [{module, [{line, 1}], ppool}]).
