-module(std@float_test).
-compile(no_auto_import).

-export([parse_test/0, to_string_test/0, ceiling_test/0, floor_test/0, round_test/0, truncate_test/0]).

parse_test() ->
    std@expect:equal(std@float:parse(<<"1.23">>), {ok, 1.23}),
    std@expect:equal(std@float:parse(<<"5.0">>), {ok, 5.0}),
    std@expect:equal(std@float:parse(<<"0.123456789">>), {ok, 0.123456789}),
    std@expect:is_error(std@float:parse(<<"">>)),
    std@expect:is_error(std@float:parse(<<"what">>)),
    std@expect:is_error(std@float:parse(<<"1">>)).

to_string_test() ->
    std@expect:equal(std@float:to_string(123.0), <<"123.0">>),
    std@expect:equal(std@float:to_string(-8.1), <<"-8.1">>).

ceiling_test() ->
    std@expect:equal(std@float:ceiling(8.1), 9.0),
    std@expect:equal(std@float:ceiling(-8.1), -8.0),
    std@expect:equal(std@float:ceiling(-8.0), -8.0).

floor_test() ->
    std@expect:equal(std@float:floor(8.1), 8.0),
    std@expect:equal(std@float:floor(-8.1), -9.0),
    std@expect:equal(std@float:floor(-8.0), -8.0).

round_test() ->
    std@expect:equal(std@float:round(8.1), 8),
    std@expect:equal(std@float:round(8.4), 8),
    std@expect:equal(std@float:round(8.499), 8),
    std@expect:equal(std@float:round(8.5), 9),
    std@expect:equal(std@float:round(-8.1), -8),
    std@expect:equal(std@float:round(-7.5), -8).

truncate_test() ->
    std@expect:equal(std@float:truncate(8.1), 8),
    std@expect:equal(std@float:truncate(8.4), 8),
    std@expect:equal(std@float:truncate(8.499), 8),
    std@expect:equal(std@float:truncate(8.5), 8),
    std@expect:equal(std@float:truncate(-8.1), -8),
    std@expect:equal(std@float:truncate(-7.5), -7).
