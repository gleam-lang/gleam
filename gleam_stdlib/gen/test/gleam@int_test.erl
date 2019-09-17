-module(gleam@int_test).
-compile(no_auto_import).

-export([to_string/0, parse/0, to_base_string/0, compare_test/0]).

to_string() ->
    gleam@expect:equal(gleam@int:to_string(123), <<"123">>),
    gleam@expect:equal(gleam@int:to_string(-123), <<"-123">>),
    gleam@expect:equal(gleam@int:to_string(123), <<"123">>).

parse() ->
    gleam@expect:equal(gleam@int:parse(<<"123">>), {ok, 123}),
    gleam@expect:equal(gleam@int:parse(<<"-123">>), {ok, -123}),
    gleam@expect:equal(gleam@int:parse(<<"0123">>), {ok, 123}),
    gleam@expect:equal(gleam@int:parse(<<"">>), {error, nil}),
    gleam@expect:equal(gleam@int:parse(<<"what">>), {error, nil}),
    gleam@expect:equal(gleam@int:parse(<<"1.23">>), {error, nil}).

to_base_string() ->
    gleam@expect:equal(gleam@int:to_base_string(100, 16), <<"64">>),
    gleam@expect:equal(gleam@int:to_base_string(-100, 16), <<"-64">>).

compare_test() ->
    gleam@expect:equal(gleam@int:compare(0, 0), eq),
    gleam@expect:equal(gleam@int:compare(1, 1), eq),
    gleam@expect:equal(gleam@int:compare(0, 1), lt),
    gleam@expect:equal(gleam@int:compare(-2, -1), lt),
    gleam@expect:equal(gleam@int:compare(2, 1), gt),
    gleam@expect:equal(gleam@int:compare(-1, -2), gt).
