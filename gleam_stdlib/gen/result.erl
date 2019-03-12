-module(result).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, flat_map/2, unwrap/2]).

is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-ifdef(TEST).
is_ok_test() ->
    expect:true(is_ok({ok, 1})),
    expect:false(is_ok({error, 1})).
-endif.

is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-ifdef(TEST).
is_error_test() ->
    expect:false(is_error({ok, 1})),
    expect:true(is_error({error, 1})).
-endif.

map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, _} ->
            Result
    end.

-ifdef(TEST).
map_test() ->
    (fun(Capture1) ->
        expect:equal(Capture1, {ok, 2})
    end)((fun(Capture1) -> map(Capture1, fun(X) -> X + 1 end) end)({ok, 1})),
    (fun(Capture1) ->
        expect:equal(Capture1, {error, 1})
    end)((fun(Capture1) -> map(Capture1, fun(X) -> X + 1 end) end)({error, 1})).
-endif.

map_error(Result, Fun) ->
    case Result of
        {ok, _} ->
            Result;

        {error, Error} ->
            {error, Fun(Error)}
    end.

-ifdef(TEST).
map_error_test() ->
    (fun(Capture1) ->
        expect:equal(Capture1, {ok, 1})
    end)((fun(Capture1) ->
             map_error(Capture1, fun(X) -> X + 1 end)
         end)({ok, 1})),
    (fun(Capture1) ->
        expect:equal(Capture1, {error, 2})
    end)((fun(Capture1) ->
             map_error(Capture1, fun(X) -> X + 1 end)
         end)({error, 1})).
-endif.

flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-ifdef(TEST).
flatten_test() ->
    (fun(Capture1) ->
        expect:equal(Capture1, {ok, 1})
    end)(flatten({ok, {ok, 1}})),
    (fun(Capture1) ->
        expect:equal(Capture1, {error, 1})
    end)(flatten({ok, {error, 1}})),
    (fun(Capture1) ->
        expect:equal(Capture1, {error, 1})
    end)(flatten({error, 1})),
    (fun(Capture1) ->
        expect:equal(Capture1, {error, {error, 1}})
    end)(flatten({error, {error, 1}})).
-endif.

flat_map(Result, Fun) ->
    case Result of
        {ok, X} ->
            case Fun(X) of
                {ok, Y} ->
                    {ok, Y};

                {error, Y1} ->
                    {error, Y1}
            end;

        {error, _} ->
            Result
    end.

-ifdef(TEST).
flat_map_test() ->
    (fun(Capture1) ->
        expect:equal(Capture1, {error, 1})
    end)((fun(Capture1) ->
             flat_map(Capture1, fun(X) -> {ok, X + 1} end)
         end)({error, 1})),
    (fun(Capture1) ->
        expect:equal(Capture1, {ok, 2})
    end)((fun(Capture1) ->
             flat_map(Capture1, fun(X) -> {ok, X + 1} end)
         end)({ok, 1})),
    (fun(Capture1) ->
        expect:equal(Capture1, {error, 1})
    end)((fun(Capture1) ->
             flat_map(Capture1, fun(Unused) -> {error, 1} end)
         end)({ok, 1})).
-endif.

unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-ifdef(TEST).
unwrap_test() ->
    (fun(Capture1) -> expect:equal(Capture1, 1) end)(unwrap({ok, 1}, 50)),
    (fun(Capture1) ->
        expect:equal(Capture1, 50)
    end)(unwrap({error, <<"nope">>}, 50)).
-endif.
