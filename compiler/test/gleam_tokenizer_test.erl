-module(gleam_tokenizer_test).
-include_lib("eunit/include/eunit.hrl").

-define(assertTokens(Code, Tokens),
        ?assertMatch(Tokens, element(2, gleam_tokenizer:string(Code)))).

keyword_test() ->
  ?assertTokens("module",   [{kw_module, _}]),
  ?assertTokens("exposing", [{kw_exposing, _}]),
  ?assertTokens("fn",       [{kw_fn, _}]).

int_test() ->
  ?assertTokens("1",    [{int, _, 1}]),
  ?assertTokens("11",   [{int, _, 11}]),
  ?assertTokens("2912", [{int, _, 2912}]).

float_test() ->
  ?assertTokens("000.7",  [{float, _, 0.7}]),
  ?assertTokens("2.912", [{float, _, 2.912}]).

string_test() ->
  ?assertTokens("\"Hi\"",     [{string, _, <<"Hi">>}]),
  ?assertTokens("\"\"",       [{string, _, <<"">>}]),
  ?assertTokens("\" \\\\ \"", [{string, _, <<" \\ ">>}]),
  ?assertTokens("\" \\\" \"", [{string, _, <<" \" ">>}]).

name_test() ->
  ?assertTokens("hi",      [{name, _, "hi"}]),
  ?assertTokens("ok",      [{name, _, "ok"}]),
  ?assertTokens("do_exec", [{name, _, "do_exec"}]).

atom_test() ->
  ?assertTokens(":hi",         [{atom, _, "hi"}]),
  ?assertTokens(":123",        [{atom, _, "123"}]),
  ?assertTokens(":WHAT_UP?",   [{atom, _, "WHAT_UP?"}]),
  ?assertTokens(":Hey_there!", [{atom, _, "Hey_there!"}]).

param_test() ->
  ?assertTokens("(",   [{'(', _}]),
  ?assertTokens(")",   [{')', _}]),
  ?assertTokens("(1)", [{'(', _}, {int, _, 1}, {')', _}]).

square_test() ->
  ?assertTokens("[",   [{'[', _}]),
  ?assertTokens("]",   [{']', _}]),
  ?assertTokens("[0]", [{'[', _}, {int, _, 0}, {']', _}]).

comma_test() ->
  ?assertTokens(",", [{',', _}]),
  ?assertTokens("[1, 2]",
                [{'[', _}, {int, _, 1}, {',', _}, {int, _, 2}, {']', _}]).

brace_test() ->
  ?assertTokens("{",     [{'{', _}]),
  ?assertTokens("}",     [{'}', _}]),
  ?assertTokens("{ 5 }", [{'{', _}, {int, _, 5}, {'}', _}]).

ops_test() ->
  ?assertTokens("+",  [{'+', _}]),
  ?assertTokens("+",  [{'+', _}]),
  ?assertTokens("-",  [{'-', _}]),
  ?assertTokens("<",  [{'<', _}]),
  ?assertTokens("==", [{'==', _}]),
  ?assertTokens("=",  [{'=', _}]),
  ?assertTokens("/",  [{'/', _}]),
  ?assertTokens("|",  [{'|', _}]).

dot_test() ->
  ?assertTokens(".", [{'.', _}]),
  ?assertTokens(
     "Mod.f()",
     [{upname, _, "Mod"}, {'.', _}, {name, _, "f"}, {'(', _}, {')', _}]).

whitespace_test() ->
  ?assertTokens(";", []),

  ?assertEqual({error,{2,gleam_tokenizer,{illegal,"\t"}},2},
               gleam_tokenizer:string("module Foo\n\t")).
