-module(str).
-compile(no_auto_import).

-export([length/1, lowercase/1, uppercase/1, reverse/1, split/2, replace/3, append/2]).

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

append(S1, S2) ->
    iodata:to_string(iodata:append(iodata:new(S1), S2)).
