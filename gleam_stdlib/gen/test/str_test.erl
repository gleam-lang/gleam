-module(str_test).
-compile(no_auto_import).

-export([length_test/0, lowercase_test/0, uppercase_test/0, reverse_test/0, split_test/0, replace_test/0, append_test/0]).

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

append_test() ->
    expect:equal(str:append(<<"Test">>, <<" Me">>), <<"Test Me">>).
