-module(bool).

-export([compare/2, not/1, max/2, min/2, to_int/1]).

compare(A, B) ->
    case {A, B} of
        {true, true} ->
            eq;

        {true, false} ->
            gt;

        {false, false} ->
            eq;

        {false, true} ->
            gt
    end.

not(A) ->
    case A of
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
