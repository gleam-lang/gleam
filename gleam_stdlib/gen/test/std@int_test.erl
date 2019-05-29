-module(std@int_test).
-compile(no_auto_import).

-export([to_string/0, parse/0, to_base_string/0, compare_test/0]).

to_string() ->
    std@expect:equal(std@int:to_string(123), <<"123">>),
    std@expect:equal(std@int:to_string(-123), <<"-123">>),
    std@expect:equal(std@int:to_string(123), <<"123">>).

parse() ->
    std@expect:equal(std@int:parse(<<"123">>), {ok, 123}),
    std@expect:equal(std@int:parse(<<"-123">>), {ok, -123}),
    std@expect:equal(std@int:parse(<<"0123">>), {ok, 123}),
    std@expect:is_error(std@int:parse(<<"">>)),
    std@expect:is_error(std@int:parse(<<"what">>)),
    std@expect:is_error(std@int:parse(<<"1.23">>)).

to_base_string() ->
    std@expect:equal(std@int:to_base_string(100, 16), <<"64">>),
    std@expect:equal(std@int:to_base_string(-100, 16), <<"-64">>).

compare_test() ->
    std@expect:equal(std@int:compare(0, 0), eq),
    std@expect:equal(std@int:compare(1, 1), eq),
    std@expect:equal(std@int:compare(0, 1), lt),
    std@expect:equal(std@int:compare(-2, -1), lt),
    std@expect:equal(std@int:compare(2, 1), gt),
    std@expect:equal(std@int:compare(-1, -2), gt).
