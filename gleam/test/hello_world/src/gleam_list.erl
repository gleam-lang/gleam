-module(gleam_list).

-export([length/1, reverse/1, is_empty/1, of/1, new/0]).

length(A) ->
    erlang:length(A).

reverse(A) ->
    erlang:reverse(A).

is_empty(List) ->
    List =:= [].

of(X) ->
    [X].

new() ->
    [].
