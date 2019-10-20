-module(gleam@map_test).
-compile(no_auto_import).

-export([from_list_test/0, has_key_test/0, new_test/0, get_test/0, insert_test/0, map_values_test/0, keys_test/0, values_test/0, take_test/0, drop_test/0, merge_test/0, delete_test/0, update_test/0, fold_test/0]).

from_list_test() ->
    gleam@expect:equal(
        gleam@map:size(gleam@map:from_list([{4, 0}, {1, 0}])),
        2
    ).

has_key_test() ->
    gleam@expect:false(gleam@map:has_key(gleam@map:from_list([]), 1)),
    gleam@expect:true(gleam@map:has_key(gleam@map:from_list([{1, 0}]), 1)),
    gleam@expect:true(
        gleam@map:has_key(gleam@map:from_list([{4, 0}, {1, 0}]), 1)
    ),
    gleam@expect:false(
        gleam@map:has_key(gleam@map:from_list([{4, 0}, {1, 0}]), 0)
    ).

new_test() ->
    gleam@expect:equal(gleam@map:size(gleam@map:new()), 0),
    gleam@expect:equal(gleam@map:to_list(gleam@map:new()), []).

get_test() ->
    Proplist = [{4, 0}, {1, 1}],
    M = gleam@map:from_list(Proplist),
    gleam@expect:equal(gleam@map:get(M, 4), {ok, 0}),
    gleam@expect:equal(gleam@map:get(M, 1), {ok, 1}),
    gleam@expect:equal(gleam@map:get(M, 2), {error, nil}).

insert_test() ->
    gleam@expect:equal(
        gleam@map:insert(
            gleam@map:insert(
                gleam@map:insert(gleam@map:new(), <<"a">>, 0),
                <<"b">>,
                1
            ),
            <<"c">>,
            2
        ),
        gleam@map:from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}])
    ).

map_values_test() ->
    gleam@expect:equal(
        gleam@map:map_values(
            gleam@map:from_list([{1, 0}, {2, 1}, {3, 2}]),
            fun(K, V) -> K + V end
        ),
        gleam@map:from_list([{1, 1}, {2, 3}, {3, 5}])
    ).

keys_test() ->
    gleam@expect:equal(
        gleam@map:keys(
            gleam@map:from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}])
        ),
        [<<"a">>, <<"b">>, <<"c">>]
    ).

values_test() ->
    gleam@expect:equal(
        gleam@map:values(
            gleam@map:from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}])
        ),
        [0, 1, 2]
    ).

take_test() ->
    gleam@expect:equal(
        gleam@map:take(
            gleam@map:from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}]),
            [<<"a">>, <<"b">>, <<"d">>]
        ),
        gleam@map:from_list([{<<"a">>, 0}, {<<"b">>, 1}])
    ).

drop_test() ->
    gleam@expect:equal(
        gleam@map:drop(
            gleam@map:from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}]),
            [<<"a">>, <<"b">>, <<"d">>]
        ),
        gleam@map:from_list([{<<"c">>, 2}])
    ).

merge_test() ->
    A = gleam@map:from_list([{<<"a">>, 2}, {<<"c">>, 4}, {<<"d">>, 3}]),
    B = gleam@map:from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}]),
    gleam@expect:equal(
        gleam@map:merge(A, B),
        gleam@map:from_list(
            [{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}, {<<"d">>, 3}]
        )
    ),
    gleam@expect:equal(
        gleam@map:merge(B, A),
        gleam@map:from_list(
            [{<<"a">>, 2}, {<<"b">>, 1}, {<<"c">>, 4}, {<<"d">>, 3}]
        )
    ).

delete_test() ->
    gleam@expect:equal(
        gleam@map:delete(
            gleam@map:delete(
                gleam@map:from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}]),
                <<"a">>
            ),
            <<"d">>
        ),
        gleam@map:from_list([{<<"b">>, 1}, {<<"c">>, 2}])
    ).

update_test() ->
    Dict = gleam@map:from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}]),
    IncOrZero = fun(X) -> case X of
            {ok, I} ->
                I + 1;

            {error, _} ->
                0
        end end,
    gleam@expect:equal(
        gleam@map:update(Dict, <<"a">>, IncOrZero),
        gleam@map:from_list([{<<"a">>, 1}, {<<"b">>, 1}, {<<"c">>, 2}])
    ),
    gleam@expect:equal(
        gleam@map:update(Dict, <<"b">>, IncOrZero),
        gleam@map:from_list([{<<"a">>, 0}, {<<"b">>, 2}, {<<"c">>, 2}])
    ),
    gleam@expect:equal(
        gleam@map:update(Dict, <<"z">>, IncOrZero),
        gleam@map:from_list(
            [{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}, {<<"z">>, 0}]
        )
    ).

fold_test() ->
    Dict = gleam@map:from_list(
        [{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}, {<<"d">>, 3}]
    ),
    Add = fun(_, V, Acc) -> V + Acc end,
    gleam@expect:equal(gleam@map:fold(Dict, 0, Add), 6),
    Concat = fun(K, _, Acc) -> gleam@string:append(Acc, K) end,
    gleam@expect:equal(gleam@map:fold(Dict, <<"">>, Concat), <<"abcd">>),
    gleam@expect:equal(gleam@map:fold(gleam@map:from_list([]), 0, Add), 0).
