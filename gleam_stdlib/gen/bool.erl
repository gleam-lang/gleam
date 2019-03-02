-module(bool).
-include_lib("eunit/include/eunit.hrl").

-export([max/2, min/2, to_int/1]).

max(A, B) ->
    case A of
        true ->
            true;

        false ->
            B
    end.

-ifdef(TEST).
max_test() ->
    _ = fun(Capture1) -> expect:equal(Capture1, true) end(max(true, true)),
    _ = fun(Capture1) -> expect:equal(Capture1, true) end(max(true, false)),
    _ = fun(Capture1) -> expect:equal(Capture1, false) end(max(false, false)),
    fun(Capture1) -> expect:equal(Capture1, true) end(max(false, true)).
-endif.

min(A, B) ->
    case A of
        false ->
            false;

        true ->
            B
    end.

-ifdef(TEST).
min_test() ->
    _ = fun(Capture1) -> expect:equal(Capture1, true) end(min(true, true)),
    _ = fun(Capture1) -> expect:equal(Capture1, false) end(min(true, false)),
    _ = fun(Capture1) -> expect:equal(Capture1, false) end(min(false, false)),
    fun(Capture1) -> expect:equal(Capture1, false) end(min(false, true)).
-endif.

to_int(Bool) ->
    case Bool of
        false ->
            0;

        true ->
            1
    end.

-ifdef(TEST).
to_int_test() ->
    _ = fun(Capture1) -> expect:equal(Capture1, 1) end(to_int(true)),
    fun(Capture1) -> expect:equal(Capture1, 0) end(to_int(false)).
-endif.
