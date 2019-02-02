-module(gleam_hello_world).

-export([ok/1, error/1, big/0, small/0, to_int/1]).

ok(X) ->
    {'ok', X}.

error(X) ->
    {'error', X}.

big() ->
    'big'.

small() ->
    'small'.

to_int(X) ->
    case X of
        'big' ->
            2;

        'small' ->
            1
    end.
