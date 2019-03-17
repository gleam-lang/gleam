-module(result).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, then/2, unwrap/2]).

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

        {error, E} ->
            {error, E}
    end.

-ifdef(TEST).
map_test() ->
    expect:equal(map({ok, 1}, fun(X) -> X + 1 end), {ok, 2}),
    expect:equal(map({ok, 1}, fun(_) -> <<"2">> end), {ok, <<"2">>}),
    expect:equal(map({error, 1}, fun(X) -> X + 1 end), {error, 1}).
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
    expect:equal(map_error({ok, 1}, fun(X) -> X + 1 end), {ok, 1}),
    expect:equal(map_error({error, 1}, fun(X) -> X + 1 end), {error, 2}).
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
    expect:equal(flatten({ok, {ok, 1}}), {ok, 1}),
    expect:equal(flatten({ok, {error, 1}}), {error, 1}),
    expect:equal(flatten({error, 1}), {error, 1}),
    expect:equal(flatten({error, {error, 1}}), {error, {error, 1}}).
-endif.

then(Result, Fun) ->
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
then_test() ->
    expect:equal(then({error, 1}, fun(X) -> {ok, X + 1} end), {error, 1}),
    expect:equal(then({ok, 1}, fun(X) -> {ok, X + 1} end), {ok, 2}),
    expect:equal(then({ok, 1}, fun(_) -> {error, 1} end), {error, 1}).
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
    expect:equal(unwrap({ok, 1}, 50), 1),
    expect:equal(unwrap({error, <<"nope">>}, 50), 50).
-endif.
