-module(gleam@map_dict_test).
-compile(no_auto_import).

-export([from_list_test/0, has_key_test/0, new_test/0, fetch_test/0, put_test/0, map_values_test/0, keys_test/0, values_test/0, take_test/0, drop_test/0, merge_test/0, delete_test/0, update_test/0, fold_test/0]).

from_list_test() ->
    gleam@expect:equal(gleam@map_dict:size(gleam@map_dict:from_list([{4, 0},
                                                                     {1, 0}])),
                       2).

has_key_test() ->
    gleam@expect:false(gleam@map_dict:has_key(gleam@map_dict:from_list([]), 1)),
    gleam@expect:true(gleam@map_dict:has_key(gleam@map_dict:from_list([{1, 0}]),
                                             1)),
    gleam@expect:true(gleam@map_dict:has_key(gleam@map_dict:from_list([{4, 0},
                                                                       {1, 0}]),
                                             1)),
    gleam@expect:false(gleam@map_dict:has_key(gleam@map_dict:from_list([{4, 0},
                                                                        {1, 0}]),
                                              0)).

new_test() ->
    gleam@expect:equal(gleam@map_dict:size(gleam@map_dict:new()), 0),
    gleam@expect:equal(gleam@map_dict:to_list(gleam@map_dict:new()), []).

fetch_test() ->
    Proplist = [{4, 0}, {1, 1}],
    M = gleam@map_dict:from_list(Proplist),
    gleam@expect:equal(gleam@map_dict:fetch(M, 4), {ok, 0}),
    gleam@expect:equal(gleam@map_dict:fetch(M, 1), {ok, 1}),
    gleam@expect:is_error(gleam@map_dict:fetch(M, 2)).

put_test() ->
    gleam@expect:equal(gleam@map_dict:put(gleam@map_dict:put(gleam@map_dict:put(gleam@map_dict:new(),
                                                                                <<"a">>,
                                                                                0),
                                                             <<"b">>,
                                                             1),
                                          <<"c">>,
                                          2),
                       gleam@map_dict:from_list([{<<"a">>, 0},
                                                 {<<"b">>, 1},
                                                 {<<"c">>, 2}])).

map_values_test() ->
    gleam@expect:equal(gleam@map_dict:map_values(gleam@map_dict:from_list([{1,
                                                                            0},
                                                                           {2,
                                                                            1},
                                                                           {3,
                                                                            2}]),
                                                 fun(K, V) -> K + V end),
                       gleam@map_dict:from_list([{1, 1}, {2, 3}, {3, 5}])).

keys_test() ->
    gleam@expect:equal(gleam@map_dict:keys(gleam@map_dict:from_list([{<<"a">>,
                                                                      0},
                                                                     {<<"b">>,
                                                                      1},
                                                                     {<<"c">>,
                                                                      2}])),
                       [<<"a">>, <<"b">>, <<"c">>]).

values_test() ->
    gleam@expect:equal(gleam@map_dict:values(gleam@map_dict:from_list([{<<"a">>,
                                                                        0},
                                                                       {<<"b">>,
                                                                        1},
                                                                       {<<"c">>,
                                                                        2}])),
                       [0, 1, 2]).

take_test() ->
    gleam@expect:equal(gleam@map_dict:take(gleam@map_dict:from_list([{<<"a">>,
                                                                      0},
                                                                     {<<"b">>,
                                                                      1},
                                                                     {<<"c">>,
                                                                      2}]),
                                           [<<"a">>, <<"b">>, <<"d">>]),
                       gleam@map_dict:from_list([{<<"a">>, 0}, {<<"b">>, 1}])).

drop_test() ->
    gleam@expect:equal(gleam@map_dict:drop(gleam@map_dict:from_list([{<<"a">>,
                                                                      0},
                                                                     {<<"b">>,
                                                                      1},
                                                                     {<<"c">>,
                                                                      2}]),
                                           [<<"a">>, <<"b">>, <<"d">>]),
                       gleam@map_dict:from_list([{<<"c">>, 2}])).

merge_test() ->
    A = gleam@map_dict:from_list([{<<"a">>, 2}, {<<"c">>, 4}, {<<"d">>, 3}]),
    B = gleam@map_dict:from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}]),
    gleam@expect:equal(gleam@map_dict:merge(A, B),
                       gleam@map_dict:from_list([{<<"a">>, 0},
                                                 {<<"b">>, 1},
                                                 {<<"c">>, 2},
                                                 {<<"d">>, 3}])),
    gleam@expect:equal(gleam@map_dict:merge(B, A),
                       gleam@map_dict:from_list([{<<"a">>, 2},
                                                 {<<"b">>, 1},
                                                 {<<"c">>, 4},
                                                 {<<"d">>, 3}])).

delete_test() ->
    gleam@expect:equal(gleam@map_dict:delete(gleam@map_dict:delete(gleam@map_dict:from_list([{<<"a">>,
                                                                                              0},
                                                                                             {<<"b">>,
                                                                                              1},
                                                                                             {<<"c">>,
                                                                                              2}]),
                                                                   <<"a">>),
                                             <<"d">>),
                       gleam@map_dict:from_list([{<<"b">>, 1}, {<<"c">>, 2}])).

update_test() ->
    Dict = gleam@map_dict:from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}]),
    IncOrZero = fun(X) -> case X of
            {ok, I} ->
                I + 1;

            {error, _} ->
                0
        end end,
    gleam@expect:equal(gleam@map_dict:update(Dict, <<"a">>, IncOrZero),
                       gleam@map_dict:from_list([{<<"a">>, 1},
                                                 {<<"b">>, 1},
                                                 {<<"c">>, 2}])),
    gleam@expect:equal(gleam@map_dict:update(Dict, <<"b">>, IncOrZero),
                       gleam@map_dict:from_list([{<<"a">>, 0},
                                                 {<<"b">>, 2},
                                                 {<<"c">>, 2}])),
    gleam@expect:equal(gleam@map_dict:update(Dict, <<"z">>, IncOrZero),
                       gleam@map_dict:from_list([{<<"a">>, 0},
                                                 {<<"b">>, 1},
                                                 {<<"c">>, 2},
                                                 {<<"z">>, 0}])).

fold_test() ->
    Dict = gleam@map_dict:from_list([{<<"a">>, 0},
                                     {<<"b">>, 1},
                                     {<<"c">>, 2},
                                     {<<"d">>, 3}]),
    Add = fun(_, V, Acc) -> V + Acc end,
    gleam@expect:equal(gleam@map_dict:fold(Dict, 0, Add), 6),
    Concat = fun(K, _, Acc) -> gleam@string:append(Acc, K) end,
    gleam@expect:equal(gleam@map_dict:fold(Dict, <<"">>, Concat), <<"abcd">>),
    gleam@expect:equal(gleam@map_dict:fold(gleam@map_dict:from_list([]), 0, Add),
                       0).
