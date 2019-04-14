-module(tuple_test).
-compile(no_auto_import).

-export([new_test/0, first_test/0, second_test/0, swap_test/0, fetch_test/0]).

new_test() ->
    expect:equal(tuple:new(1, 2), {1, 2}),
    expect:equal(tuple:new(2, <<"3">>), {2, <<"3">>}).

first_test() ->
    expect:equal(tuple:first({1, 2}), 1).

second_test() ->
    expect:equal(tuple:second({1, 2}), 2).

swap_test() ->
    expect:equal(tuple:swap({1, <<"2">>}), {<<"2">>, 1}).

fetch_test() ->
    Proplist = [{0, <<"1">>}, {1, <<"2">>}],
    expect:equal(tuple:fetch(Proplist, 0), {ok, <<"1">>}),
    expect:equal(tuple:fetch(Proplist, 1), {ok, <<"2">>}),
    expect:is_error(tuple:fetch(Proplist, 2)).
