-module(gleam@tuple_test).
-compile(no_auto_import).

-export([new_test/0, first_test/0, second_test/0, swap_test/0, fetch_test/0]).

new_test() ->
    gleam@expect:equal(gleam@tuple:new(1, 2), {1, 2}),
    gleam@expect:equal(gleam@tuple:new(2, <<"3">>), {2, <<"3">>}).

first_test() ->
    gleam@expect:equal(gleam@tuple:first({1, 2}), 1).

second_test() ->
    gleam@expect:equal(gleam@tuple:second({1, 2}), 2).

swap_test() ->
    gleam@expect:equal(gleam@tuple:swap({1, <<"2">>}), {<<"2">>, 1}).

fetch_test() ->
    Proplist = [{0, <<"1">>}, {1, <<"2">>}],
    gleam@expect:equal(gleam@tuple:fetch(Proplist, 0), {ok, <<"1">>}),
    gleam@expect:equal(gleam@tuple:fetch(Proplist, 1), {ok, <<"2">>}),
    gleam@expect:is_error(gleam@tuple:fetch(Proplist, 2)).
