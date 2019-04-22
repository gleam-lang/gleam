-module(float_test).
-compile(no_auto_import).

-export([parse_test/0, to_string_test/0, ceiling_test/0, floor_test/0]).

parse_test() ->
    expect:equal(float:parse(<<"1.23">>), {ok, 1.23}),
    expect:equal(float:parse(<<"5.0">>), {ok, 5.0}),
    expect:equal(float:parse(<<"0.123456789">>), {ok, 0.123456789}),
    expect:is_error(float:parse(<<"">>)),
    expect:is_error(float:parse(<<"what">>)),
    expect:is_error(float:parse(<<"1">>)).

to_string_test() ->
    expect:equal(float:to_string(123.0), <<"123.0">>),
    expect:equal(float:to_string(-8.1), <<"-8.1">>).

ceiling_test() ->
    expect:equal(float:ceiling(8.1), 9.0),
    expect:equal(float:ceiling(-8.1), -8.0),
    expect:equal(float:ceiling(-8.0), -8.0).

floor_test() ->
    expect:equal(float:floor(8.1), 8.0),
    expect:equal(float:floor(-8.1), -9.0),
    expect:equal(float:floor(-8.0), -8.0).
