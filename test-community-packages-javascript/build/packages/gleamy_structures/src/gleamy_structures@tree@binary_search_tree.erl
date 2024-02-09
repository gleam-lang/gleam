-module(gleamy_structures@tree@binary_search_tree).
-compile([no_auto_import, nowarn_unused_vars]).

-export([new/1, clear/1, insert/2, delete/2, find/2, fold/3, draw/2]).
-export_type([node_/1, tree/1]).

-type node_(FCI) :: empty | {node, node_(FCI), FCI, node_(FCI)}.

-opaque tree(FCJ) :: {tree, node_(FCJ), fun((FCJ, FCJ) -> gleam@order:order())}.

-spec new(fun((FCK, FCK) -> gleam@order:order())) -> tree(FCK).
new(Compare) ->
    {tree, empty, Compare}.

-spec clear(tree(FCM)) -> tree(FCM).
clear(Tree) ->
    {tree, empty, erlang:element(3, Tree)}.

-spec do_insert(node_(FCP), FCP, fun((FCP, FCP) -> gleam@order:order())) -> node_(FCP).
do_insert(Node, Key, Compare) ->
    case Node of
        {node, L, K, R} ->
            case Compare(Key, K) of
                lt ->
                    {node, do_insert(L, Key, Compare), K, R};

                gt ->
                    {node, L, K, do_insert(R, Key, Compare)};

                eq ->
                    {node, L, Key, R}
            end;

        empty ->
            {node, empty, Key, empty}
    end.

-spec insert(tree(FCP), FCP) -> tree(FCP).
insert(Tree, Key) ->
    {tree,
        do_insert(erlang:element(2, Tree), Key, erlang:element(3, Tree)),
        erlang:element(3, Tree)}.

-spec do_delete(node_(FCS), FCS, fun((FCS, FCS) -> gleam@order:order())) -> node_(FCS).
do_delete(Node, Key, Compare) ->
    case Node of
        {node, L, K, R} ->
            case Compare(Key, K) of
                lt ->
                    {node, do_delete(L, Key, Compare), K, R};

                gt ->
                    {node, L, K, do_delete(R, Key, Compare)};

                eq ->
                    case Node of
                        {node, empty, _, R@1} ->
                            R@1;

                        {node, L@1, _, empty} ->
                            L@1;

                        {node, L@2, _, R@2} ->
                            case do_min(R@2, Compare) of
                                {node, _, Mk, _} ->
                                    {node, L@2, Mk, do_delete(R@2, Mk, Compare)};

                                empty ->
                                    empty
                            end;

                        empty ->
                            empty
                    end
            end;

        empty ->
            empty
    end.

-spec delete(tree(FCS), FCS) -> tree(FCS).
delete(Tree, Key) ->
    {tree,
        do_delete(erlang:element(2, Tree), Key, erlang:element(3, Tree)),
        erlang:element(3, Tree)}.

-spec do_find(node_(FCV), FCV, fun((FCV, FCV) -> gleam@order:order())) -> {ok,
        FCV} |
    {error, nil}.
do_find(Node, Key, Compare) ->
    case Node of
        {node, L, K, R} ->
            case Compare(Key, K) of
                lt ->
                    do_find(L, Key, Compare);

                gt ->
                    do_find(R, Key, Compare);

                eq ->
                    {ok, K}
            end;

        empty ->
            {error, nil}
    end.

-spec find(tree(FCV), FCV) -> {ok, FCV} | {error, nil}.
find(Tree, Key) ->
    do_find(erlang:element(2, Tree), Key, erlang:element(3, Tree)).

-spec do_fold(node_(FCZ), FDB, fun((FDB, FCZ) -> FDB)) -> FDB.
do_fold(Node, Acc, Fun) ->
    case Node of
        {node, R, V, L} ->
            Acc@1 = do_fold(R, Acc, Fun),
            Acc@2 = Fun(Acc@1, V),
            Acc@3 = do_fold(L, Acc@2, Fun),
            Acc@3;

        empty ->
            Acc
    end.

-spec fold(tree(FCZ), FDB, fun((FDB, FCZ) -> FDB)) -> FDB.
fold(Tree, Acc, Fun) ->
    do_fold(erlang:element(2, Tree), Acc, Fun).

-spec do_draw(node_(FDC), integer(), fun((FDC) -> binary())) -> binary().
do_draw(Node, Indent, To_string) ->
    case Node of
        {node, L, K, R} ->
            Ls = do_draw(L, Indent + 1, To_string),
            Ks = do_indent(<<(To_string(K))/binary, "\n"/utf8>>, Indent),
            Rs = do_draw(R, Indent + 1, To_string),
            <<<<Ls/binary, Ks/binary>>/binary, Rs/binary>>;

        empty ->
            <<""/utf8>>
    end.

-spec draw(tree(FDC), fun((FDC) -> binary())) -> binary().
draw(Tree, To_string) ->
    do_draw(erlang:element(2, Tree), 0, To_string).

-spec do_min(node_(FCS), fun((FCS, FCS) -> gleam@order:order())) -> node_(FCS).
do_min(Node, Compare) ->
    case Node of
        {node, {node, _, _, _} = L, _, _} ->
            do_min(L, Compare);

        {node, empty, _, _} ->
            Node;

        empty ->
            empty
    end.

-spec do_indent(binary(), integer()) -> binary().
do_indent(Acc, I) ->
    case I of
        0 ->
            Acc;

        I@1 ->
            do_indent(<<".  "/utf8, Acc/binary>>, I@1 - 1)
    end.
