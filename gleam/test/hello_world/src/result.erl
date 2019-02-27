-module(gleam_result).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, flat_map/2, unwrap/2]).

is_ok(Result) ->
    case Result of
        {error, A} ->
            false;

        {ok, A1} ->
            true
    end.

is_error(Result) ->
    case Result of
        {ok, A} ->
            false;

        {error, A1} ->
            true
    end.

map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, X1} ->
            Result
    end.

map_error(Result, Fun) ->
    case Result of
        {ok, A} ->
            Result;

        {error, Error} ->
            {error, Fun(Error)}
    end.

flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

flat_map(Result, Fun) ->
    flatten(map(Result, Fun)).

unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, A} ->
            Default
    end.
