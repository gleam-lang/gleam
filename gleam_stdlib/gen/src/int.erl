-module(int).
-compile(no_auto_import).

-export([parse/1, to_string/1, to_base_string/2]).

parse(A) ->
    gleam__stdlib:parse_int(A).

to_string(A) ->
    erlang:integer_to_binary(A).

to_base_string(A, B) ->
    erlang:integer_to_binary(A, B).
