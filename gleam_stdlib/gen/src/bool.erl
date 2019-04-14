-module(bool).
-compile(no_auto_import).

-export([negate/1, max/2, min/2, to_int/1]).

negate(Bool) ->
    case Bool of
        true ->
            false;

        false ->
            true
    end.

max(A, B) ->
    case A of
        true ->
            true;

        false ->
            B
    end.

min(A, B) ->
    case A of
        false ->
            false;

        true ->
            B
    end.

to_int(Bool) ->
    case Bool of
        false ->
            0;

        true ->
            1
    end.
