-module(gleamy_structures@map).
-compile([no_auto_import, nowarn_unused_vars]).

-export([new/1, insert/3, find/2, has_key/2, delete/2, count/1, fold/3, filter/2, merge/2, from_list/2, to_list/1, take/2]).

-spec new(fun((GVO, GVO) -> gleam@order:order())) -> gleamy_structures@tree@red_black_tree_kv:tree(GVO, any()).
new(Compare) ->
    gleamy_structures@tree@red_black_tree_kv:new(Compare).

-spec insert(gleamy_structures@tree@red_black_tree_kv:tree(GVS, GVT), GVS, GVT) -> gleamy_structures@tree@red_black_tree_kv:tree(GVS, GVT).
insert(Map, Key, Value) ->
    gleamy_structures@tree@red_black_tree_kv:insert(Map, Key, Value).

-spec find(gleamy_structures@tree@red_black_tree_kv:tree(GVY, GVZ), GVY) -> {ok,
        {GVY, GVZ}} |
    {error, nil}.
find(Map, Key) ->
    gleamy_structures@tree@red_black_tree_kv:find(Map, Key).

-spec has_key(gleamy_structures@tree@red_black_tree_kv:tree(GWE, any()), GWE) -> boolean().
has_key(Map, Key) ->
    case gleamy_structures@tree@red_black_tree_kv:find(Map, Key) of
        {ok, _} ->
            true;

        {error, _} ->
            false
    end.

-spec delete(gleamy_structures@tree@red_black_tree_kv:tree(GWI, GWJ), GWI) -> gleamy_structures@tree@red_black_tree_kv:tree(GWI, GWJ).
delete(Map, Key) ->
    gleamy_structures@tree@red_black_tree_kv:delete(Map, Key).

-spec count(gleamy_structures@tree@red_black_tree_kv:tree(any(), any())) -> integer().
count(Map) ->
    gleamy_structures@tree@red_black_tree_kv:fold(
        Map,
        0,
        fun(A, _, _) -> A + 1 end
    ).

-spec fold(
    gleamy_structures@tree@red_black_tree_kv:tree(GWS, GWT),
    GWW,
    fun((GWW, GWS, GWT) -> GWW)
) -> GWW.
fold(Map, Initial, Reducer) ->
    gleamy_structures@tree@red_black_tree_kv:fold(Map, Initial, Reducer).

-spec filter(
    gleamy_structures@tree@red_black_tree_kv:tree(GWX, GWY),
    fun((GWX, GWY) -> boolean())
) -> gleamy_structures@tree@red_black_tree_kv:tree(GWX, GWY).
filter(Map, Property) ->
    gleamy_structures@tree@red_black_tree_kv:fold(
        Map,
        Map,
        fun(Map@1, K, V) -> case Property(K, V) of
                true ->
                    Map@1;

                false ->
                    gleamy_structures@tree@red_black_tree_kv:delete(Map@1, K)
            end end
    ).

-spec merge(
    gleamy_structures@tree@red_black_tree_kv:tree(GXD, GXE),
    gleamy_structures@tree@red_black_tree_kv:tree(GXD, GXE)
) -> gleamy_structures@tree@red_black_tree_kv:tree(GXD, GXE).
merge(First, Second) ->
    gleamy_structures@tree@red_black_tree_kv:fold(
        First,
        Second,
        fun(A, K, V) ->
            gleamy_structures@tree@red_black_tree_kv:insert(A, K, V)
        end
    ).

-spec from_list(list({GXS, GXT}), fun((GXS, GXS) -> gleam@order:order())) -> gleamy_structures@tree@red_black_tree_kv:tree(GXS, GXT).
from_list(Members, Compare) ->
    gleam@list:fold(
        Members,
        gleamy_structures@tree@red_black_tree_kv:new(Compare),
        fun(Tree, I) ->
            gleamy_structures@tree@red_black_tree_kv:insert(
                Tree,
                erlang:element(1, I),
                erlang:element(2, I)
            )
        end
    ).

-spec to_list(gleamy_structures@tree@red_black_tree_kv:tree(GXX, GXY)) -> list({GXX,
    GXY}).
to_list(Map) ->
    gleamy_structures@tree@red_black_tree_kv:foldr(
        Map,
        [],
        fun(A, K, V) -> [{K, V} | A] end
    ).

-spec take(gleamy_structures@tree@red_black_tree_kv:tree(GXL, GXM), list(GXL)) -> gleamy_structures@tree@red_black_tree_kv:tree(GXL, GXM).
take(Map, Desired) ->
    case Desired of
        [X | Xs] ->
            case gleamy_structures@tree@red_black_tree_kv:find(Map, X) of
                {ok, X@1} ->
                    gleamy_structures@tree@red_black_tree_kv:insert(
                        take(Map, Xs),
                        erlang:element(1, X@1),
                        erlang:element(2, X@1)
                    );

                {error, _} ->
                    take(Map, Xs)
            end;

        [] ->
            gleamy_structures@tree@red_black_tree_kv:clear(Map)
    end.
