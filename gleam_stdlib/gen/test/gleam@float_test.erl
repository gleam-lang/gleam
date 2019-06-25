-module(gleam@float_test).
-compile(no_auto_import).

-export([parse_test/0, to_string_test/0, ceiling_test/0, floor_test/0, round_test/0, truncate_test/0]).

parse_test() ->
    gleam@expect:equal(gleam@float:parse(<<"1.23">>), {ok, 1.23}),
    gleam@expect:equal(gleam@float:parse(<<"5.0">>), {ok, 5.0}),
    gleam@expect:equal(gleam@float:parse(<<"0.123456789">>), {ok, 0.123456789}),
    gleam@expect:is_error(gleam@float:parse(<<"">>)),
    gleam@expect:is_error(gleam@float:parse(<<"what">>)),
    gleam@expect:is_error(gleam@float:parse(<<"1">>)).

to_string_test() ->
    gleam@expect:equal(gleam@float:to_string(123.0), <<"123.0">>),
    gleam@expect:equal(gleam@float:to_string(-8.1), <<"-8.1">>).

ceiling_test() ->
    gleam@expect:equal(gleam@float:ceiling(8.1), 9.0),
    gleam@expect:equal(gleam@float:ceiling(-8.1), -8.0),
    gleam@expect:equal(gleam@float:ceiling(-8.0), -8.0).

floor_test() ->
    gleam@expect:equal(gleam@float:floor(8.1), 8.0),
    gleam@expect:equal(gleam@float:floor(-8.1), -9.0),
    gleam@expect:equal(gleam@float:floor(-8.0), -8.0).

round_test() ->
    gleam@expect:equal(gleam@float:round(8.1), 8),
    gleam@expect:equal(gleam@float:round(8.4), 8),
    gleam@expect:equal(gleam@float:round(8.499), 8),
    gleam@expect:equal(gleam@float:round(8.5), 9),
    gleam@expect:equal(gleam@float:round(-8.1), -8),
    gleam@expect:equal(gleam@float:round(-7.5), -8).

truncate_test() ->
    gleam@expect:equal(gleam@float:truncate(8.1), 8),
    gleam@expect:equal(gleam@float:truncate(8.4), 8),
    gleam@expect:equal(gleam@float:truncate(8.499), 8),
    gleam@expect:equal(gleam@float:truncate(8.5), 8),
    gleam@expect:equal(gleam@float:truncate(-8.1), -8),
    gleam@expect:equal(gleam@float:truncate(-7.5), -7).
