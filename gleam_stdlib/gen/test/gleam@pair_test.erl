-module(gleam@pair_test).
-compile(no_auto_import).

-export([first_test/0, second_test/0, swap_test/0, map_first_test/0, map_second_test/0]).

first_test() ->
    gleam@expect:equal(gleam@pair:first({1, 2}), 1),
    gleam@expect:equal(gleam@pair:first({<<"abc">>, []}), <<"abc">>).

second_test() ->
    gleam@expect:equal(gleam@pair:second({1, 2}), 2),
    gleam@expect:equal(gleam@pair:second({<<"abc">>, []}), []).

swap_test() ->
    gleam@expect:equal(gleam@pair:swap({1, <<"2">>}), {<<"2">>, 1}).

map_first_test() ->
    Inc = fun(A) -> A + 1 end,
    gleam@expect:equal(gleam@pair:map_first({1, 2}, Inc), {2, 2}),
    gleam@expect:equal(gleam@pair:map_first({8, 2}, Inc), {9, 2}),
    gleam@expect:equal(gleam@pair:map_first({0, -2}, Inc), {1, -2}),
    gleam@expect:equal(gleam@pair:map_first({-10, 20}, Inc), {-9, 20}).

map_second_test() ->
    Dec = fun(A) -> A - 1 end,
    gleam@expect:equal(gleam@pair:map_second({1, 2}, Dec), {1, 1}),
    gleam@expect:equal(gleam@pair:map_second({8, 2}, Dec), {8, 1}),
    gleam@expect:equal(gleam@pair:map_second({0, -2}, Dec), {0, -3}),
    gleam@expect:equal(gleam@pair:map_second({-10, 20}, Dec), {-10, 19}).
