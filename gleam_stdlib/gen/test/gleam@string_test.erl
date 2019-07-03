-module(gleam@string_test).
-compile(no_auto_import).

-export([length_test/0, lowercase_test/0, uppercase_test/0, reverse_test/0, split_test/0, replace_test/0, append_test/0]).

length_test() ->
    gleam@expect:equal(gleam@string:length(<<"ß↑e̊">>), 3),
    gleam@expect:equal(gleam@string:length(<<"Gleam">>), 5),
    gleam@expect:equal(gleam@string:length(<<"">>), 0).

lowercase_test() ->
    gleam@expect:equal(gleam@string:lowercase(<<"Gleam">>), <<"gleam">>).

uppercase_test() ->
    gleam@expect:equal(gleam@string:uppercase(<<"Gleam">>), <<"GLEAM">>).

reverse_test() ->
    gleam@expect:equal(gleam@string:reverse(<<"Gleam">>), <<"maelG">>).

split_test() ->
    gleam@expect:equal(
        gleam@string:split(<<"Gleam,Erlang,Elixir">>, <<",">>),
        [<<"Gleam">>, <<"Erlang">>, <<"Elixir">>]
    ),
    gleam@expect:equal(
        gleam@string:split(<<"Gleam, Erlang,Elixir">>, <<", ">>),
        [<<"Gleam">>, <<"Erlang,Elixir">>]
    ).

replace_test() ->
    gleam@expect:equal(
        gleam@string:replace(<<"Gleam,Erlang,Elixir">>, <<",">>, <<"++">>),
        <<"Gleam++Erlang++Elixir">>
    ).

append_test() ->
    gleam@expect:equal(
        gleam@string:append(<<"Test">>, <<" Me">>),
        <<"Test Me">>
    ).
