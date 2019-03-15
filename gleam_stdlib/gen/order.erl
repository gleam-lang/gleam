-module(order).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

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

-ifdef(TEST).
reverse_test() ->
    expect:equal(reverse(lt), gt),
    expect:equal(reverse(eq), eq),
    expect:equal(reverse(gt), lt).
-endif.

to_int(Order) ->
    case Order of
        lt ->
            -1;

        eq ->
            0;

        gt ->
            1
    end.

-ifdef(TEST).
to_int_test() ->
    expect:equal(to_int(lt), -1),
    expect:equal(to_int(eq), 0),
    expect:equal(to_int(gt), 1).
-endif.

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

        _ ->
            gt
    end.

-ifdef(TEST).
compare_test() ->
    expect:equal(compare(lt, lt), eq),
    expect:equal(compare(lt, eq), lt),
    expect:equal(compare(lt, gt), lt),
    expect:equal(compare(eq, lt), gt),
    expect:equal(compare(eq, eq), eq),
    expect:equal(compare(eq, gt), lt),
    expect:equal(compare(gt, lt), gt),
    expect:equal(compare(gt, eq), gt),
    expect:equal(compare(gt, gt), eq).
-endif.

max(A, B) ->
    case {A, B} of
        {gt, _} ->
            gt;

        {eq, lt} ->
            eq;

        _ ->
            B
    end.

-ifdef(TEST).
max_test() ->
    expect:equal(max(lt, lt), lt),
    expect:equal(max(lt, eq), eq),
    expect:equal(max(lt, gt), gt),
    expect:equal(max(eq, lt), eq),
    expect:equal(max(eq, eq), eq),
    expect:equal(max(eq, gt), gt),
    expect:equal(max(gt, lt), gt),
    expect:equal(max(gt, eq), gt),
    expect:equal(max(gt, gt), gt).
-endif.

min(A, B) ->
    case {A, B} of
        {lt, _} ->
            lt;

        {eq, gt} ->
            eq;

        _ ->
            B
    end.

-ifdef(TEST).
min_test() ->
    expect:equal(min(lt, lt), lt),
    expect:equal(min(lt, eq), lt),
    expect:equal(min(lt, gt), lt),
    expect:equal(min(eq, lt), lt),
    expect:equal(min(eq, eq), eq),
    expect:equal(min(eq, gt), eq),
    expect:equal(min(gt, lt), lt),
    expect:equal(min(gt, eq), eq),
    expect:equal(min(gt, gt), gt).
-endif.
