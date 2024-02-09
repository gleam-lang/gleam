-module(gleamy_structures@heap@pairing_heap).
-compile([no_auto_import, nowarn_unused_vars]).

-export([new/1, find_min/1, insert/2, delete_min/1]).
-export_type([t/1, heap/1]).

-type t(EWV) :: e | {t, EWV, list(t(EWV))}.

-opaque heap(EWW) :: {heap, t(EWW), fun((EWW, EWW) -> gleam@order:order())}.

-spec new(fun((EWX, EWX) -> gleam@order:order())) -> heap(EWX).
new(Compare) ->
    {heap, e, Compare}.

-spec find_min(heap(EXC)) -> {ok, EXC} | {error, nil}.
find_min(Heap) ->
    case erlang:element(2, Heap) of
        {t, X, _} ->
            {ok, X};

        e ->
            {error, nil}
    end.

-spec merge(t(EXL), t(EXL), fun((EXL, EXL) -> gleam@order:order())) -> t(EXL).
merge(X, Y, Compare) ->
    case {X, Y} of
        {X@1, e} ->
            X@1;

        {e, Y@1} ->
            Y@1;

        {{t, Xk, Xs}, {t, Yk, Ys}} ->
            case Compare(Xk, Yk) of
                gt ->
                    {t, Yk, [X | Ys]};

                _ ->
                    {t, Xk, [Y | Xs]}
            end
    end.

-spec insert(heap(EWZ), EWZ) -> heap(EWZ).
insert(Heap, Key) ->
    {heap,
        merge({t, Key, []}, erlang:element(2, Heap), erlang:element(3, Heap)),
        erlang:element(3, Heap)}.

-spec merge_pairs(list(t(EXP)), fun((EXP, EXP) -> gleam@order:order())) -> t(EXP).
merge_pairs(L, Compare) ->
    case L of
        [] ->
            e;

        [H] ->
            H;

        [H1, H2 | Hs] ->
            merge(merge(H1, H2, Compare), merge_pairs(Hs, Compare), Compare)
    end.

-spec delete_min(heap(EXG)) -> {ok, {EXG, heap(EXG)}} | {error, nil}.
delete_min(Heap) ->
    case erlang:element(2, Heap) of
        {t, X, Xs} ->
            {ok,
                {X,
                    {heap,
                        merge_pairs(Xs, erlang:element(3, Heap)),
                        erlang:element(3, Heap)}}};

        e ->
            {error, nil}
    end.
