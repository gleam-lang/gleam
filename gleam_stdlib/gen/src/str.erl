-module(str).
-compile(no_auto_import).

-export([length/1, lowercase/1, uppercase/1, reverse/1, split/2, replace/3, from_int/1, parse_int/1, parse_float/1, base_from_int/2, from_float/1]).

length(A) ->
    string:length(A).

lowercase(A) ->
    string:lowercase(A).

uppercase(A) ->
    string:uppercase(A).

reverse(String) ->
    iodata:to_string(iodata:reverse(iodata:new(String))).

split(String, On) ->
    list:map(iodata:split(iodata:new(String), On), fun iodata:to_string/1).

replace(String, Pattern, With) ->
    iodata:to_string(iodata:replace(iodata:new(String), Pattern, With)).

from_int(A) ->
    erlang:integer_to_binary(A).

parse_int(A) ->
    gleam__stdlib:parse_int(A).

parse_float(A) ->
    gleam__stdlib:parse_float(A).

base_from_int(A, B) ->
    erlang:integer_to_binary(A, B).

from_float(F) ->
    iodata:to_string(iodata:from_float(F)).
