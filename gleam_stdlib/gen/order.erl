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
    (fun(Capture1) -> expect:equal(Capture1, gt) end)(reverse(lt)),
    (fun(Capture1) -> expect:equal(Capture1, eq) end)(reverse(eq)),
    (fun(Capture1) -> expect:equal(Capture1, lt) end)(reverse(gt)).
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
    (fun(Capture1) -> expect:equal(Capture1, -1) end)(to_int(lt)),
    (fun(Capture1) -> expect:equal(Capture1, 0) end)(to_int(eq)),
    (fun(Capture1) -> expect:equal(Capture1, 1) end)(to_int(gt)).
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
    (fun(Capture1) -> expect:equal(Capture1, eq) end)(compare(lt, lt)),
    (fun(Capture1) -> expect:equal(Capture1, lt) end)(compare(lt, eq)),
    (fun(Capture1) -> expect:equal(Capture1, lt) end)(compare(lt, gt)),
    (fun(Capture1) -> expect:equal(Capture1, gt) end)(compare(eq, lt)),
    (fun(Capture1) -> expect:equal(Capture1, eq) end)(compare(eq, eq)),
    (fun(Capture1) -> expect:equal(Capture1, lt) end)(compare(eq, gt)),
    (fun(Capture1) -> expect:equal(Capture1, gt) end)(compare(gt, lt)),
    (fun(Capture1) -> expect:equal(Capture1, gt) end)(compare(gt, eq)),
    (fun(Capture1) -> expect:equal(Capture1, eq) end)(compare(gt, gt)).
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
    (fun(Capture1) -> expect:equal(Capture1, lt) end)(max(lt, lt)),
    (fun(Capture1) -> expect:equal(Capture1, eq) end)(max(lt, eq)),
    (fun(Capture1) -> expect:equal(Capture1, gt) end)(max(lt, gt)),
    (fun(Capture1) -> expect:equal(Capture1, eq) end)(max(eq, lt)),
    (fun(Capture1) -> expect:equal(Capture1, eq) end)(max(eq, eq)),
    (fun(Capture1) -> expect:equal(Capture1, gt) end)(max(eq, gt)),
    (fun(Capture1) -> expect:equal(Capture1, gt) end)(max(gt, lt)),
    (fun(Capture1) -> expect:equal(Capture1, gt) end)(max(gt, eq)),
    (fun(Capture1) -> expect:equal(Capture1, gt) end)(max(gt, gt)).
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
    (fun(Capture1) -> expect:equal(Capture1, lt) end)(min(lt, lt)),
    (fun(Capture1) -> expect:equal(Capture1, lt) end)(min(lt, eq)),
    (fun(Capture1) -> expect:equal(Capture1, lt) end)(min(lt, gt)),
    (fun(Capture1) -> expect:equal(Capture1, lt) end)(min(eq, lt)),
    (fun(Capture1) -> expect:equal(Capture1, eq) end)(min(eq, eq)),
    (fun(Capture1) -> expect:equal(Capture1, eq) end)(min(eq, gt)),
    (fun(Capture1) -> expect:equal(Capture1, lt) end)(min(gt, lt)),
    (fun(Capture1) -> expect:equal(Capture1, eq) end)(min(gt, eq)),
    (fun(Capture1) -> expect:equal(Capture1, gt) end)(min(gt, gt)).
-endif.
