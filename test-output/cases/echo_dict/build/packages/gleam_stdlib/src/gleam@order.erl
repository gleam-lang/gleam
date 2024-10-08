-module(gleam@order).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([negate/1, to_int/1, compare/2, reverse/1, break_tie/2, lazy_break_tie/2]).
-export_type([order/0]).

-type order() :: lt | eq | gt.

-file("/Users/louis/src/gleam/stdlib/src/gleam/order.gleam", 35).
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

-file("/Users/louis/src/gleam/stdlib/src/gleam/order.gleam", 62).
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

-file("/Users/louis/src/gleam/stdlib/src/gleam/order.gleam", 79).
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

-file("/Users/louis/src/gleam/stdlib/src/gleam/order.gleam", 100).
-spec reverse(fun((I, I) -> order())) -> fun((I, I) -> order()).
reverse(Orderer) ->
    fun(A, B) -> Orderer(B, A) end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/order.gleam", 122).
-spec break_tie(order(), order()) -> order().
break_tie(Order, Other) ->
    case Order of
        lt ->
            Order;

        gt ->
            Order;

        eq ->
            Other
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/order.gleam", 151).
-spec lazy_break_tie(order(), fun(() -> order())) -> order().
lazy_break_tie(Order, Comparison) ->
    case Order of
        lt ->
            Order;

        gt ->
            Order;

        eq ->
            Comparison()
    end.
