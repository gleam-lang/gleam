-module(gleam@float).
-compile(no_auto_import).

-export([parse/1, to_string/1, compare/2, max/2, ceiling/1, floor/1, round/1, truncate/1]).

parse(A) ->
    gleam_stdlib:parse_float(A).

to_string(F) ->
    gleam@iodata:to_string(gleam@iodata:from_float(F)).

compare(A, B) ->
    case A =:= B of
        true ->
            eq;

        false ->
            case A < B of
                true ->
                    lt;

                false ->
                    gt
            end
    end.

max(A, B) ->
    case A > B of
        true ->
            A;

        false ->
            B
    end.

ceiling(A) ->
    math:ceil(A).

floor(A) ->
    math:floor(A).

round(A) ->
    erlang:round(A).

truncate(A) ->
    erlang:trunc(A).
