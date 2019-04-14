-module(str_test).
-compile(no_auto_import).

-export([length_test/0, lowercase_test/0, uppercase_test/0, reverse_test/0, split_test/0, replace_test/0, from_int_test/0, parse_int_test/0, parse_float_test/0, base_from_int_test/0, from_float_test/0]).

length_test() ->
    expect:equal(str:length(<<"ß↑e̊">>), 3),
    expect:equal(str:length(<<"Gleam">>), 5),
    expect:equal(str:length(<<"">>), 0).

lowercase_test() ->
    expect:equal(str:lowercase(<<"Gleam">>), <<"gleam">>).

uppercase_test() ->
    expect:equal(str:uppercase(<<"Gleam">>), <<"GLEAM">>).

reverse_test() ->
    expect:equal(str:reverse(<<"Gleam">>), <<"maelG">>).

split_test() ->
    expect:equal(str:split(<<"Gleam,Erlang,Elixir">>, <<",">>),
                 [<<"Gleam">>, <<"Erlang">>, <<"Elixir">>]),
    expect:equal(str:split(<<"Gleam, Erlang,Elixir">>, <<", ">>),
                 [<<"Gleam">>, <<"Erlang,Elixir">>]).

replace_test() ->
    expect:equal(str:replace(<<"Gleam,Erlang,Elixir">>, <<",">>, <<"++">>),
                 <<"Gleam++Erlang++Elixir">>).

from_int_test() ->
    expect:equal(str:from_int(123), <<"123">>),
    expect:equal(str:from_int(-123), <<"-123">>),
    expect:equal(str:from_int(123), <<"123">>).

parse_int_test() ->
    expect:equal(str:parse_int(<<"123">>), {ok, 123}),
    expect:equal(str:parse_int(<<"-123">>), {ok, -123}),
    expect:equal(str:parse_int(<<"0123">>), {ok, 123}),
    expect:is_error(str:parse_int(<<"">>)),
    expect:is_error(str:parse_int(<<"what">>)),
    expect:is_error(str:parse_int(<<"1.23">>)).

parse_float_test() ->
    expect:equal(str:parse_float(<<"1.23">>), {ok, 1.23}),
    expect:equal(str:parse_float(<<"5.0">>), {ok, 5.0}),
    expect:equal(str:parse_float(<<"0.123456789">>), {ok, 0.123456789}),
    expect:is_error(str:parse_float(<<"">>)),
    expect:is_error(str:parse_float(<<"what">>)),
    expect:is_error(str:parse_float(<<"1">>)).

base_from_int_test() ->
    expect:equal(str:base_from_int(100, 16), <<"64">>),
    expect:equal(str:base_from_int(-100, 16), <<"-64">>).

from_float_test() ->
    expect:equal(str:from_float(123.0), <<"123.0">>),
    expect:equal(str:from_float(-8.1), <<"-8.1">>).
