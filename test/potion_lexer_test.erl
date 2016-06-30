-module(potion_lexer_test).
-include_lib("eunit/include/eunit.hrl").

-define(assertTokens(Code, Tokens),
        ?assertMatch(Tokens, element(2, potion_tokenizer:string(Code)))).

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

dot_test() ->
  ?assertTokens(".", [{'.', _}]),
  ?assertTokens(
     ":mod.f()",
     [{atom, _, mod}, {'.', _}, {identifier, _, f}, {'(', _}, {')', _}]).
