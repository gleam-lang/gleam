-module(tuple).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([new/2, first/1, second/1, swap/1, fetch/2]).

new(A, B) ->
    {A, B}.

-ifdef(TEST).
new_test() ->
    expect:equal(new(1, 2), {1, 2}),
    expect:equal(new(2, <<"3">>), {2, <<"3">>}).
-endif.

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

swap(Tup) ->
    {A, B} = Tup,
    {B, A}.

-ifdef(TEST).
swap_test() ->
    expect:equal(swap({1, <<"2">>}), {<<"2">>, 1}).
-endif.

fetch(Haystack, Needle) ->
    list:find(Haystack, fun(Tuple) -> case first(Tuple) =:= Needle of
                      true ->
                          {ok, second(Tuple)};

                      false ->
                          {error, []}
                  end end).

-ifdef(TEST).
fetch_test() ->
    Proplist = [{0, <<"1">>}, {1, <<"2">>}],
    expect:equal(fetch(Proplist, 0), {ok, <<"1">>}),
    expect:equal(fetch(Proplist, 1), {ok, <<"2">>}),
    expect:is_error(fetch(Proplist, 2)).
-endif.
