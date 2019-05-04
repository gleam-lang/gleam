-module(map_dict_test).
-compile(no_auto_import).

-export([from_list_test/0, has_key_test/0, new_test/0, fetch_test/0, put_test/0, map_values_test/0, keys_test/0, values_test/0, take_test/0, drop_test/0, merge_test/0]).

from_list_test() ->
    expect:equal(map_dict:size(map_dict:from_list([{4, 0}, {1, 0}])), 2).

has_key_test() ->
    expect:false(map_dict:has_key(map_dict:from_list([]), 1)),
    expect:true(map_dict:has_key(map_dict:from_list([{1, 0}]), 1)),
    expect:true(map_dict:has_key(map_dict:from_list([{4, 0}, {1, 0}]), 1)),
    expect:false(map_dict:has_key(map_dict:from_list([{4, 0}, {1, 0}]), 0)).

new_test() ->
    expect:equal(map_dict:size(map_dict:new()), 0),
    expect:equal(map_dict:to_list(map_dict:new()), []).

fetch_test() ->
    Proplist = [{4, 0}, {1, 1}],
    M = map_dict:from_list(Proplist),
    expect:equal(map_dict:fetch(M, 4), {ok, 0}),
    expect:equal(map_dict:fetch(M, 1), {ok, 1}),
    expect:is_error(map_dict:fetch(M, 2)).

put_test() ->
    expect:equal(map_dict:put(map_dict:put(map_dict:put(map_dict:new(),
                                                        <<"a">>,
                                                        0),
                                           <<"b">>,
                                           1),
                              <<"c">>,
                              2),
                 map_dict:from_list([{<<"a">>, 0},
                                     {<<"b">>, 1},
                                     {<<"c">>, 2}])).

map_values_test() ->
    expect:equal(map_dict:map_values(map_dict:from_list([{1, 0},
                                                         {2, 1},
                                                         {3, 2}]),
                                     fun(K, V) -> K + V end),
                 map_dict:from_list([{1, 1}, {2, 3}, {3, 5}])).

keys_test() ->
    expect:equal(map_dict:keys(map_dict:from_list([{<<"a">>, 0},
                                                   {<<"b">>, 1},
                                                   {<<"c">>, 2}])),
                 [<<"a">>, <<"b">>, <<"c">>]).

values_test() ->
    expect:equal(map_dict:values(map_dict:from_list([{<<"a">>, 0},
                                                     {<<"b">>, 1},
                                                     {<<"c">>, 2}])),
                 [0, 1, 2]).

take_test() ->
    expect:equal(map_dict:take(map_dict:from_list([{<<"a">>, 0},
                                                   {<<"b">>, 1},
                                                   {<<"c">>, 2}]),
                               [<<"a">>, <<"b">>, <<"d">>]),
                 map_dict:from_list([{<<"a">>, 0}, {<<"b">>, 1}])).

drop_test() ->
    expect:equal(map_dict:drop(map_dict:from_list([{<<"a">>, 0},
                                                   {<<"b">>, 1},
                                                   {<<"c">>, 2}]),
                               [<<"a">>, <<"b">>, <<"d">>]),
                 map_dict:from_list([{<<"c">>, 2}])).

merge_test() ->
    A = map_dict:from_list([{<<"a">>, 2}, {<<"c">>, 4}, {<<"d">>, 3}]),
    B = map_dict:from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}]),
    expect:equal(map_dict:merge(A, B),
                 map_dict:from_list([{<<"a">>, 0},
                                     {<<"b">>, 1},
                                     {<<"c">>, 2},
                                     {<<"d">>, 3}])),
    expect:equal(map_dict:merge(B, A),
                 map_dict:from_list([{<<"a">>, 2},
                                     {<<"b">>, 1},
                                     {<<"c">>, 4},
                                     {<<"d">>, 3}])).
