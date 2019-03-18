-module(tuple).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([first/1, second/1]).

first(Tup) ->
    {A, _} = Tup,
    A.

-ifdef(TEST).
first_test() ->
    expect:equal(first({1, 2}), 1).
-endif.

second(Tup) ->
    {_, A} = Tup,
    A.

-ifdef(TEST).
second_test() ->
    expect:equal(second({1, 2}), 2).
-endif.
