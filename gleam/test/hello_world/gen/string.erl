-module(string).

-export([concat/1]).

concat(A) ->
    erlang:iolist_to_binary(A).
