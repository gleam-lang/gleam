-module(gleam@order).
-compile(no_auto_import).

-export([reverse/1, to_int/1, compare/2, max/2, min/2]).

reverse(Order) ->
    case Order of
        lt ->
            gt;

        eq ->
            eq;

        gt ->
            lt
    end.

to_int(Order) ->
    case Order of
        lt ->
            -1;

        eq ->
            0;

        gt ->
            1
    end.

compare(A, B) ->
    case {A, B} of
        {lt, lt} ->
            eq;

        {lt, _} ->
            lt;

        {eq, eq} ->
            eq;

        {gt, gt} ->
            eq;

        {eq, gt} ->
            lt;

        {_, _} ->
            gt
    end.

max(A, B) ->
    case {A, B} of
        {gt, _} ->
            gt;

        {eq, lt} ->
            eq;

        {_, _} ->
            B
    end.

min(A, B) ->
    case {A, B} of
        {lt, _} ->
            lt;

        {eq, gt} ->
            eq;

        {_, _} ->
            B
    end.
