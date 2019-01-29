-module(gleam_hello_world).

-export([ok/1, error/1, big/0, small/0]).

ok(X) ->
    {'ok', X}.

error(X) ->
    {'error', X}.

big() ->
    'big'.

small() ->
    'small'.
