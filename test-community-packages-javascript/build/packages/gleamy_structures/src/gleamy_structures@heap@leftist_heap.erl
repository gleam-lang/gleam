-module(gleamy_structures@heap@leftist_heap).
-compile([no_auto_import, nowarn_unused_vars]).

-export([new/1, find_min/1, insert/2, delete_min/1]).
-export_type([t/1, heap/1]).

-type t(FXL) :: e | {t, integer(), FXL, t(FXL), t(FXL)}.

-opaque heap(FXM) :: {heap, t(FXM), fun((FXM, FXM) -> gleam@order:order())}.

-spec new(fun((FXN, FXN) -> gleam@order:order())) -> heap(FXN).
new(Compare) ->
    {heap, e, Compare}.

-spec find_min(heap(FXS)) -> {ok, FXS} | {error, nil}.
find_min(Heap) ->
    case erlang:element(2, Heap) of
        {t, _, X, _, _} ->
            {ok, X};

        e ->
            {error, nil}
    end.

-spec make(FYX, t(FYX), t(FYX)) -> t(FYX).
make(X, A, B) ->
    Rank_a = case A of
        {t, R, _, _, _} ->
            R;

        e ->
            0
    end,
    Rank_b = case B of
        {t, R@1, _, _, _} ->
            R@1;

        e ->
            0
    end,
    case Rank_a < Rank_b of
        true ->
            {t, Rank_a + 1, X, B, A};

        _ ->
            {t, Rank_b + 1, X, A, B}
    end.

-spec merge(t(FYB), t(FYB), fun((FYB, FYB) -> gleam@order:order())) -> t(FYB).
merge(H1, H2, Compare) ->
    case {H1, H2} of
        {H, e} ->
            H;

        {e, H@1} ->
            H@1;

        {{t, _, X, A1, B1}, {t, _, Y, A2, B2}} ->
            case Compare(X, Y) of
                gt ->
                    make(Y, A2, merge(H1, B2, Compare));

                _ ->
                    make(X, A1, merge(B1, H2, Compare))
            end
    end.

-spec insert(heap(FXP), FXP) -> heap(FXP).
insert(Heap, Item) ->
    {heap,
        merge(
            {t, 1, Item, e, e},
            erlang:element(2, Heap),
            erlang:element(3, Heap)
        ),
        erlang:element(3, Heap)}.

-spec delete_min(heap(FXW)) -> {ok, {FXW, heap(FXW)}} | {error, nil}.
delete_min(Heap) ->
    case erlang:element(2, Heap) of
        {t, _, X, A, B} ->
            {ok,
                {X,
                    {heap,
                        merge(A, B, erlang:element(3, Heap)),
                        erlang:element(3, Heap)}}};

        e ->
            {error, nil}
    end.
