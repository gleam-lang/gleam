-module(gleamy_structures@tree@red_black_tree).
-compile([no_auto_import, nowarn_unused_vars]).

-export([new/1, clear/1, insert/2, delete/2, find/2, fold/3, foldr/3, draw/2]).
-export_type([color/0, node_/1, tree/1, min_del/1]).

-type color() :: r | b | bb.

-type node_(FHR) :: e | ee | {t, color(), node_(FHR), FHR, node_(FHR)}.

-opaque tree(FHS) :: {tree, node_(FHS), fun((FHS, FHS) -> gleam@order:order())}.

-type min_del(FHT) :: {min, FHT, node_(FHT)} | none.

-spec new(fun((FHU, FHU) -> gleam@order:order())) -> tree(FHU).
new(Compare) ->
    {tree, e, Compare}.

-spec clear(tree(FHW)) -> tree(FHW).
clear(Tree) ->
    {tree, e, erlang:element(3, Tree)}.

-spec blacken(node_(FIW)) -> node_(FIW).
blacken(Node) ->
    case Node of
        {t, r, {t, r, _, _, _} = L, Y, C} ->
            {t, b, L, Y, C};

        {t, r, A, X, {t, r, _, _, _} = R} ->
            {t, b, A, X, R};

        T ->
            T
    end.

-spec balance(color(), node_(FIZ), FIZ, node_(FIZ)) -> node_(FIZ).
balance(C, L, V, R) ->
    case {C, L, V, R} of
        {b, {t, r, {t, r, A, X, B}, Y, C@1}, Z, D} ->
            {t, r, {t, b, A, X, B}, Y, {t, b, C@1, Z, D}};

        {b, {t, r, A@1, X@1, {t, r, B@1, Y@1, C@2}}, Z@1, D@1} ->
            {t, r, {t, b, A@1, X@1, B@1}, Y@1, {t, b, C@2, Z@1, D@1}};

        {b, A@2, X@2, {t, r, {t, r, B@2, Y@2, C@3}, Z@2, D@2}} ->
            {t, r, {t, b, A@2, X@2, B@2}, Y@2, {t, b, C@3, Z@2, D@2}};

        {b, A@3, X@3, {t, r, B@3, Y@3, {t, r, C@4, Z@3, D@3}}} ->
            {t, r, {t, b, A@3, X@3, B@3}, Y@3, {t, b, C@4, Z@3, D@3}};

        {bb, A@4, X@4, {t, r, {t, r, B@4, Y@4, C@5}, Z@4, D@4}} ->
            {t, b, {t, b, A@4, X@4, B@4}, Y@4, {t, b, C@5, Z@4, D@4}};

        {bb, {t, r, A@5, X@5, {t, r, B@5, Y@5, C@6}}, Z@5, D@5} ->
            {t, b, {t, b, A@5, X@5, B@5}, Y@5, {t, b, C@6, Z@5, D@5}};

        {C@7, A@6, X@6, B@6} ->
            {t, C@7, A@6, X@6, B@6}
    end.

-spec redden(node_(FJD)) -> node_(FJD).
redden(Node) ->
    case Node of
        {t, b, {t, b, _, _, _} = L, Y, {t, b, _, _, _} = R} ->
            {t, r, L, Y, R};

        T ->
            T
    end.

-spec rotate(color(), node_(FJK), FJK, node_(FJK)) -> node_(FJK).
rotate(C, L, V, R) ->
    case {C, L, V, R} of
        {r, {t, bb, A, X, B}, Y, {t, b, C@1, Z, D}} ->
            balance(b, {t, r, {t, b, A, X, B}, Y, C@1}, Z, D);

        {r, ee, Y@1, {t, b, C@2, Z@1, D@1}} ->
            balance(b, {t, r, e, Y@1, C@2}, Z@1, D@1);

        {r, {t, b, A@1, X@1, B@1}, Y@2, {t, bb, C@3, Z@2, D@2}} ->
            balance(b, A@1, X@1, {t, r, B@1, Y@2, {t, b, C@3, Z@2, D@2}});

        {r, {t, b, A@2, X@2, B@2}, Y@3, ee} ->
            balance(b, A@2, X@2, {t, r, B@2, Y@3, e});

        {b, {t, bb, A@3, X@3, B@3}, Y@4, {t, b, C@4, Z@3, D@3}} ->
            balance(bb, {t, r, {t, b, A@3, X@3, B@3}, Y@4, C@4}, Z@3, D@3);

        {b, ee, Y@5, {t, b, C@5, Z@4, D@4}} ->
            balance(bb, {t, r, e, Y@5, C@5}, Z@4, D@4);

        {b, {t, b, A@4, X@4, B@4}, Y@6, {t, bb, C@6, Z@5, D@5}} ->
            balance(bb, A@4, X@4, {t, r, B@4, Y@6, {t, b, C@6, Z@5, D@5}});

        {b, {t, b, A@5, X@5, B@5}, Y@7, ee} ->
            balance(bb, A@5, X@5, {t, r, B@5, Y@7, e});

        {b, {t, bb, A@6, W, B@6}, X@6, {t, r, {t, b, C@7, Y@8, D@6}, Z@6, E}} ->
            {t,
                b,
                balance(b, {t, r, {t, b, A@6, W, B@6}, X@6, C@7}, Y@8, D@6),
                Z@6,
                E};

        {b, ee, X@7, {t, r, {t, b, C@8, Y@9, D@7}, Z@7, E@1}} ->
            {t, b, balance(b, {t, r, e, X@7, C@8}, Y@9, D@7), Z@7, E@1};

        {b,
            {t, r, A@7, W@1, {t, b, B@7, X@8, C@9}},
            Y@10,
            {t, bb, D@8, Z@8, E@2}} ->
            {t,
                b,
                A@7,
                W@1,
                balance(b, B@7, X@8, {t, r, C@9, Y@10, {t, b, D@8, Z@8, E@2}})};

        {b, {t, r, A@8, W@2, {t, b, B@8, X@9, C@10}}, Y@11, ee} ->
            {t, b, A@8, W@2, balance(b, B@8, X@9, {t, r, C@10, Y@11, e})};

        {C@11, A@9, X@10, B@9} ->
            {t, C@11, A@9, X@10, B@9}
    end.

-spec ins(node_(FHZ), FHZ, fun((FHZ, FHZ) -> gleam@order:order())) -> node_(FHZ).
ins(Node, X, Compare) ->
    case Node of
        e ->
            {t, r, e, X, e};

        {t, C, A, Y, B} ->
            case Compare(X, Y) of
                lt ->
                    balance(C, ins(A, X, Compare), Y, B);

                gt ->
                    balance(C, A, Y, ins(B, X, Compare));

                eq ->
                    {t, C, A, X, B}
            end;

        _ ->
            Node
    end.

-spec insert(tree(FHZ), FHZ) -> tree(FHZ).
insert(Tree, Key) ->
    {tree,
        blacken(ins(erlang:element(2, Tree), Key, erlang:element(3, Tree))),
        erlang:element(3, Tree)}.

-spec del(node_(FIC), FIC, fun((FIC, FIC) -> gleam@order:order())) -> node_(FIC).
del(Node, X, Compare) ->
    case Node of
        e ->
            Node;

        {t, r, e, Y, e} ->
            case Compare(X, Y) of
                eq ->
                    e;

                _ ->
                    Node
            end;

        {t, b, e, Y@1, e} ->
            case Compare(X, Y@1) of
                eq ->
                    ee;

                _ ->
                    Node
            end;

        {t, b, {t, r, e, Y@2, e} = L, Z, e} ->
            case Compare(X, Z) of
                lt ->
                    {t, b, del(L, X, Compare), Z, e};

                gt ->
                    Node;

                eq ->
                    {t, b, e, Y@2, e}
            end;

        {t, C, A, Y@3, B} ->
            case Compare(X, Y@3) of
                lt ->
                    rotate(C, del(A, X, Compare), Y@3, B);

                gt ->
                    rotate(C, A, Y@3, del(B, X, Compare));

                eq ->
                    case min_del(B) of
                        {min, Y1, B1} ->
                            rotate(C, A, Y1, B1);

                        none ->
                            e
                    end
            end;

        _ ->
            Node
    end.

-spec delete(tree(FIC), FIC) -> tree(FIC).
delete(Tree, Key) ->
    {tree,
        del(redden(erlang:element(2, Tree)), Key, erlang:element(3, Tree)),
        erlang:element(3, Tree)}.

-spec do_find(node_(FIF), FIF, fun((FIF, FIF) -> gleam@order:order())) -> {ok,
        FIF} |
    {error, nil}.
do_find(Node, Key, Compare) ->
    case Node of
        {t, _, L, K, R} ->
            case Compare(Key, K) of
                lt ->
                    do_find(L, Key, Compare);

                gt ->
                    do_find(R, Key, Compare);

                eq ->
                    {ok, K}
            end;

        _ ->
            {error, nil}
    end.

-spec find(tree(FIF), FIF) -> {ok, FIF} | {error, nil}.
find(Tree, Key) ->
    do_find(erlang:element(2, Tree), Key, erlang:element(3, Tree)).

-spec do_fold(node_(FIJ), FIL, fun((FIL, FIJ) -> FIL)) -> FIL.
do_fold(Node, Acc, Fun) ->
    case Node of
        {t, _, R, V, L} ->
            Acc@1 = do_fold(R, Acc, Fun),
            Acc@2 = Fun(Acc@1, V),
            Acc@3 = do_fold(L, Acc@2, Fun),
            Acc@3;

        _ ->
            Acc
    end.

-spec fold(tree(FIJ), FIL, fun((FIL, FIJ) -> FIL)) -> FIL.
fold(Tree, Acc, Fun) ->
    do_fold(erlang:element(2, Tree), Acc, Fun).

-spec do_foldr(node_(FIM), FIO, fun((FIO, FIM) -> FIO)) -> FIO.
do_foldr(Node, Acc, Fun) ->
    case Node of
        {t, _, R, V, L} ->
            Acc@1 = do_foldr(L, Acc, Fun),
            Acc@2 = Fun(Acc@1, V),
            Acc@3 = do_foldr(R, Acc@2, Fun),
            Acc@3;

        _ ->
            Acc
    end.

-spec foldr(tree(FIM), FIO, fun((FIO, FIM) -> FIO)) -> FIO.
foldr(Tree, Acc, Fun) ->
    do_foldr(erlang:element(2, Tree), Acc, Fun).

-spec do_draw(node_(FIP), integer(), fun((FIP) -> binary())) -> binary().
do_draw(Node, Indent, To_string) ->
    case Node of
        {t, _, L, K, R} ->
            Ls = do_draw(L, Indent + 1, To_string),
            Ks = do_indent(<<(To_string(K))/binary, "\n"/utf8>>, Indent),
            Rs = do_draw(R, Indent + 1, To_string),
            <<<<Ls/binary, Ks/binary>>/binary, Rs/binary>>;

        _ ->
            <<""/utf8>>
    end.

-spec draw(tree(FIP), fun((FIP) -> binary())) -> binary().
draw(Tree, To_string) ->
    do_draw(erlang:element(2, Tree), 0, To_string).

-spec min_del(node_(any())) -> min_del(any()).
min_del(Node) ->
    case Node of
        {t, r, e, X, e} ->
            {min, X, e};

        {t, b, e, X@1, e} ->
            {min, X@1, ee};

        {t, b, e, X@2, {t, r, e, Y, e}} ->
            {min, X@2, {t, b, e, Y, e}};

        {t, C, A, X@3, B} ->
            case min_del(A) of
                {min, X1, A1} ->
                    {min, X1, rotate(C, A1, X@3, B)};

                none ->
                    none
            end;

        _ ->
            none
    end.

-spec do_indent(binary(), integer()) -> binary().
do_indent(Acc, I) ->
    case I of
        0 ->
            Acc;

        I@1 ->
            do_indent(<<".  "/utf8, Acc/binary>>, I@1 - 1)
    end.
