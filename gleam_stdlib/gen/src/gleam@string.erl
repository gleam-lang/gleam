-module(gleam@string).
-compile(no_auto_import).

-export([length/1, lowercase/1, uppercase/1, reverse/1, split/2, replace/3, append/2]).

length(A) ->
    string:length(A).

lowercase(A) ->
    string:lowercase(A).

uppercase(A) ->
    string:uppercase(A).

reverse(String) ->
    gleam@iodata:to_string(gleam@iodata:reverse(gleam@iodata:new(String))).

split(String, On) ->
    gleam@list:map(
        gleam@iodata:split(gleam@iodata:new(String), On),
        fun gleam@iodata:to_string/1
    ).

replace(String, Pattern, With) ->
    gleam@iodata:to_string(
        gleam@iodata:replace(gleam@iodata:new(String), Pattern, With)
    ).

append(S1, S2) ->
    gleam@iodata:to_string(gleam@iodata:append(gleam@iodata:new(S1), S2)).
