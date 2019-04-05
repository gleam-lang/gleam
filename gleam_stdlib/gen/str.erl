-module(str).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([length/1, lowercase/1, uppercase/1, reverse/1, split/2, replace/3, from_int/1, parse_int/1, base_from_int/2, from_float/1]).

length(A) ->
    string:length(A).

-ifdef(TEST).
length_test() ->
    expect:equal(length(<<"ß↑e̊">>), 3),
    expect:equal(length(<<"Gleam">>), 5),
    expect:equal(length(<<"">>), 0).
-endif.

lowercase(A) ->
    string:lowercase(A).

-ifdef(TEST).
lowercase_test() ->
    expect:equal(lowercase(<<"Gleam">>), <<"gleam">>).
-endif.

uppercase(A) ->
    string:uppercase(A).

-ifdef(TEST).
uppercase_test() ->
    expect:equal(uppercase(<<"Gleam">>), <<"GLEAM">>).
-endif.

reverse(String) ->
    iodata:to_string(iodata:reverse(iodata:new(String))).

-ifdef(TEST).
reverse_test() ->
    expect:equal(reverse(<<"Gleam">>), <<"maelG">>).
-endif.

split(String, On) ->
    list:map(iodata:split(iodata:new(String), On), fun iodata:to_string/1).

-ifdef(TEST).
split_test() ->
    expect:equal(split(<<"Gleam,Erlang,Elixir">>, <<",">>),
                 [<<"Gleam">>, <<"Erlang">>, <<"Elixir">>]),
    expect:equal(split(<<"Gleam, Erlang,Elixir">>, <<", ">>),
                 [<<"Gleam">>, <<"Erlang,Elixir">>]).
-endif.

replace(String, Pattern, With) ->
    iodata:to_string(iodata:replace(iodata:new(String), Pattern, With)).

-ifdef(TEST).
replace_test() ->
    expect:equal(replace(<<"Gleam,Erlang,Elixir">>, <<",">>, <<"++">>),
                 <<"Gleam++Erlang++Elixir">>).
-endif.

from_int(A) ->
    erlang:integer_to_binary(A).

-ifdef(TEST).
from_int_test() ->
    expect:equal(from_int(123), <<"123">>),
    expect:equal(from_int(-123), <<"-123">>),
    expect:equal(from_int(123), <<"123">>).
-endif.

parse_int(A) ->
    gleam__stdlib:parse_int(A).

-ifdef(TEST).
parse_int_test() ->
    expect:equal(parse_int(<<"123">>), {ok, 123}),
    expect:equal(parse_int(<<"-123">>), {ok, -123}),
    expect:equal(parse_int(<<"0123">>), {ok, 123}).
-endif.

base_from_int(A, B) ->
    erlang:integer_to_binary(A, B).

-ifdef(TEST).
base_from_int_test() ->
    expect:equal(base_from_int(100, 16), <<"64">>),
    expect:equal(base_from_int(-100, 16), <<"-64">>).
-endif.

from_float(F) ->
    iodata:to_string(iodata:from_float(F)).

-ifdef(TEST).
from_float_test() ->
    expect:equal(from_float(123.0), <<"123.0">>),
    expect:equal(from_float(-8.1), <<"-8.1">>).
-endif.
