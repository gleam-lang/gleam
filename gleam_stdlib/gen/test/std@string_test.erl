-module(std@string_test).
-compile(no_auto_import).

-export([length_test/0, lowercase_test/0, uppercase_test/0, reverse_test/0, split_test/0, replace_test/0, append_test/0]).

length_test() ->
    std@expect:equal(std@string:length(<<"ß↑e̊">>), 3),
    std@expect:equal(std@string:length(<<"Gleam">>), 5),
    std@expect:equal(std@string:length(<<"">>), 0).

lowercase_test() ->
    std@expect:equal(std@string:lowercase(<<"Gleam">>), <<"gleam">>).

uppercase_test() ->
    std@expect:equal(std@string:uppercase(<<"Gleam">>), <<"GLEAM">>).

reverse_test() ->
    std@expect:equal(std@string:reverse(<<"Gleam">>), <<"maelG">>).

split_test() ->
    std@expect:equal(std@string:split(<<"Gleam,Erlang,Elixir">>, <<",">>),
                     [<<"Gleam">>, <<"Erlang">>, <<"Elixir">>]),
    std@expect:equal(std@string:split(<<"Gleam, Erlang,Elixir">>, <<", ">>),
                     [<<"Gleam">>, <<"Erlang,Elixir">>]).

replace_test() ->
    std@expect:equal(std@string:replace(<<"Gleam,Erlang,Elixir">>,
                                        <<",">>,
                                        <<"++">>),
                     <<"Gleam++Erlang++Elixir">>).

append_test() ->
    std@expect:equal(std@string:append(<<"Test">>, <<" Me">>), <<"Test Me">>).
