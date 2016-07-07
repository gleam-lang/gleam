-module(gleam_tokenizer_test).
-include_lib("eunit/include/eunit.hrl").

-define(assertTokens(Code, Tokens),
        ?assertMatch(Tokens, element(2, gleam_tokenizer:string(Code)))).

keyword_test() ->
  ?assertTokens("module",  [{module, _}]),
  ?assertTokens("private", [{private, _}]),
  ?assertTokens("public",  [{public, _}]),
  ?assertTokens("def",     [{def, _}]).

numbers_test() ->
  ?assertTokens("1",     [{num, _, 1}]),
  ?assertTokens("1.1",   [{num, _, 1.1}]),
  ?assertTokens("29.12", [{num, _, 29.12}]),
  ?assertTokens("0007",  [{num, _, 7}]) .

strings_test() ->
  ?assertTokens("\"Hi\"",     [{string, _, <<"Hi">>}]),
  ?assertTokens("\"\"",       [{string, _, <<"">>}]),
  ?assertTokens("\" \\\\ \"", [{string, _, <<" \\ ">>}]),
  ?assertTokens("\" \\\" \"", [{string, _, <<" \" ">>}]).

identifier_test() ->
  ?assertTokens("hi",       [{identifier, _, hi}]),
  ?assertTokens("ok?",      [{identifier, _, 'ok?'}]),
  ?assertTokens("do_exec!", [{identifier, _, 'do_exec!'}]).

atom_test() ->
  ?assertTokens(":hi",         [{atom, _, hi}]),
  ?assertTokens(":123",        [{atom, _, '123'}]),
  ?assertTokens(":WHAT_UP?",   [{atom, _, 'WHAT_UP?'}]),
  ?assertTokens(":Hey-there!", [{atom, _, 'Hey-there!'}]).

param_test() ->
  ?assertTokens("(",   [{'(', _}]),
  ?assertTokens(")",   [{')', _}]),
  ?assertTokens("(1)", [{'(', _}, {num, _, 1}, {')', _}]).

square_test() ->
  ?assertTokens("[",   [{'[', _}]),
  ?assertTokens("]",   [{']', _}]),
  ?assertTokens("[0]", [{'[', _}, {num, _, 0}, {']', _}]).

comma_test() ->
  ?assertTokens(",", [{',', _}]),
  ?assertTokens("[1, 2]",
                [{'[', _}, {num, _, 1}, {',', _}, {num, _, 2}, {']', _}]).

brace_test() ->
  ?assertTokens("{",     [{'{', _}]),
  ?assertTokens("}",     [{'}', _}]),
  ?assertTokens("{ 5 }", [{'{', _}, {num, _, 5}, {'}', _}]).

equal_test() ->
  ?assertTokens("=", [{'=', 1}]).

dot_test() ->
  ?assertTokens(".", [{'.', _}]),
  ?assertTokens(
     ":mod.f()",
     [{atom, _, mod}, {'.', _}, {identifier, _, f}, {'(', _}, {')', _}]).
