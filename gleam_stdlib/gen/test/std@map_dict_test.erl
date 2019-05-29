-module(std@map_dict_test).
-compile(no_auto_import).

-export([from_list_test/0, has_key_test/0, new_test/0, fetch_test/0, put_test/0, map_values_test/0, keys_test/0, values_test/0, take_test/0, drop_test/0, merge_test/0, delete_test/0, update_test/0, fold_test/0]).

from_list_test() ->
    std@expect:equal(std@map_dict:size(std@map_dict:from_list([{4, 0}, {1, 0}])),
                     2).

has_key_test() ->
    std@expect:false(std@map_dict:has_key(std@map_dict:from_list([]), 1)),
    std@expect:true(std@map_dict:has_key(std@map_dict:from_list([{1, 0}]), 1)),
    std@expect:true(std@map_dict:has_key(std@map_dict:from_list([{4, 0},
                                                                 {1, 0}]),
                                         1)),
    std@expect:false(std@map_dict:has_key(std@map_dict:from_list([{4, 0},
                                                                  {1, 0}]),
                                          0)).

new_test() ->
    std@expect:equal(std@map_dict:size(std@map_dict:new()), 0),
    std@expect:equal(std@map_dict:to_list(std@map_dict:new()), []).

fetch_test() ->
    Proplist = [{4, 0}, {1, 1}],
    M = std@map_dict:from_list(Proplist),
    std@expect:equal(std@map_dict:fetch(M, 4), {ok, 0}),
    std@expect:equal(std@map_dict:fetch(M, 1), {ok, 1}),
    std@expect:is_error(std@map_dict:fetch(M, 2)).

put_test() ->
    std@expect:equal(std@map_dict:put(std@map_dict:put(std@map_dict:put(std@map_dict:new(),
                                                                        <<"a">>,
                                                                        0),
                                                       <<"b">>,
                                                       1),
                                      <<"c">>,
                                      2),
                     std@map_dict:from_list([{<<"a">>, 0},
                                             {<<"b">>, 1},
                                             {<<"c">>, 2}])).

map_values_test() ->
    std@expect:equal(std@map_dict:map_values(std@map_dict:from_list([{1, 0},
                                                                     {2, 1},
                                                                     {3, 2}]),
                                             fun(K, V) -> K + V end),
                     std@map_dict:from_list([{1, 1}, {2, 3}, {3, 5}])).

keys_test() ->
    std@expect:equal(std@map_dict:keys(std@map_dict:from_list([{<<"a">>, 0},
                                                               {<<"b">>, 1},
                                                               {<<"c">>, 2}])),
                     [<<"a">>, <<"b">>, <<"c">>]).

values_test() ->
    std@expect:equal(std@map_dict:values(std@map_dict:from_list([{<<"a">>, 0},
                                                                 {<<"b">>, 1},
                                                                 {<<"c">>, 2}])),
                     [0, 1, 2]).

take_test() ->
    std@expect:equal(std@map_dict:take(std@map_dict:from_list([{<<"a">>, 0},
                                                               {<<"b">>, 1},
                                                               {<<"c">>, 2}]),
                                       [<<"a">>, <<"b">>, <<"d">>]),
                     std@map_dict:from_list([{<<"a">>, 0}, {<<"b">>, 1}])).

drop_test() ->
    std@expect:equal(std@map_dict:drop(std@map_dict:from_list([{<<"a">>, 0},
                                                               {<<"b">>, 1},
                                                               {<<"c">>, 2}]),
                                       [<<"a">>, <<"b">>, <<"d">>]),
                     std@map_dict:from_list([{<<"c">>, 2}])).

merge_test() ->
    A = std@map_dict:from_list([{<<"a">>, 2}, {<<"c">>, 4}, {<<"d">>, 3}]),
    B = std@map_dict:from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}]),
    std@expect:equal(std@map_dict:merge(A, B),
                     std@map_dict:from_list([{<<"a">>, 0},
                                             {<<"b">>, 1},
                                             {<<"c">>, 2},
                                             {<<"d">>, 3}])),
    std@expect:equal(std@map_dict:merge(B, A),
                     std@map_dict:from_list([{<<"a">>, 2},
                                             {<<"b">>, 1},
                                             {<<"c">>, 4},
                                             {<<"d">>, 3}])).

delete_test() ->
    std@expect:equal(std@map_dict:delete(std@map_dict:delete(std@map_dict:from_list([{<<"a">>,
                                                                                      0},
                                                                                     {<<"b">>,
                                                                                      1},
                                                                                     {<<"c">>,
                                                                                      2}]),
                                                             <<"a">>),
                                         <<"d">>),
                     std@map_dict:from_list([{<<"b">>, 1}, {<<"c">>, 2}])).

update_test() ->
    Dict = std@map_dict:from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}]),
    IncOrZero = fun(X) -> case X of
            {ok, I} ->
                I + 1;

            {error, _} ->
                0
        end end,
    std@expect:equal(std@map_dict:update(Dict, <<"a">>, IncOrZero),
                     std@map_dict:from_list([{<<"a">>, 1},
                                             {<<"b">>, 1},
                                             {<<"c">>, 2}])),
    std@expect:equal(std@map_dict:update(Dict, <<"b">>, IncOrZero),
                     std@map_dict:from_list([{<<"a">>, 0},
                                             {<<"b">>, 2},
                                             {<<"c">>, 2}])),
    std@expect:equal(std@map_dict:update(Dict, <<"z">>, IncOrZero),
                     std@map_dict:from_list([{<<"a">>, 0},
                                             {<<"b">>, 1},
                                             {<<"c">>, 2},
                                             {<<"z">>, 0}])).

fold_test() ->
    Dict = std@map_dict:from_list([{<<"a">>, 0},
                                   {<<"b">>, 1},
                                   {<<"c">>, 2},
                                   {<<"d">>, 3}]),
    Add = fun(_, V, Acc) -> V + Acc end,
    std@expect:equal(std@map_dict:fold(Dict, 0, Add), 6),
    Concat = fun(K, _, Acc) -> std@string:append(Acc, K) end,
    std@expect:equal(std@map_dict:fold(Dict, <<"">>, Concat), <<"abcd">>),
    std@expect:equal(std@map_dict:fold(std@map_dict:from_list([]), 0, Add), 0).
