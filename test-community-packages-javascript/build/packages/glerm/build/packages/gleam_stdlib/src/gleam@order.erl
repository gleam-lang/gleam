-module(gleam@order).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([negate/1, to_int/1, compare/2, max/2, min/2, reverse/1]).
-export_type([order/0]).

-type order() :: lt | eq | gt.

-spec negate(order()) -> order().
negate(Order) ->
    case Order of
        lt ->
            gt;

        eq ->
            eq;

        gt ->
            lt
    end.

-spec to_int(order()) -> integer().
to_int(Order) ->
    case Order of
        lt ->
            -1;

        eq ->
            0;

        gt ->
            1
    end.

-spec compare(order(), order()) -> order().
compare(A, B) ->
    case {A, B} of
        {X, Y} when X =:= Y ->
            eq;

        {lt, _} ->
            lt;

        {eq, gt} ->
            lt;

        {_, _} ->
            gt
    end.

-spec max(order(), order()) -> order().
max(A, B) ->
    case {A, B} of
        {gt, _} ->
            gt;

        {eq, lt} ->
            eq;

        {_, _} ->
            B
    end.

-spec min(order(), order()) -> order().
min(A, B) ->
    case {A, B} of
        {lt, _} ->
            lt;

        {eq, gt} ->
            eq;

        {_, _} ->
            B
    end.

-spec reverse(fun((CF, CF) -> order())) -> fun((CF, CF) -> order()).
reverse(Orderer) ->
    fun(A, B) -> Orderer(B, A) end.
