-module(str).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([length/1, lowercase/1, uppercase/1, reverse/1, split/2, replace/3]).

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
