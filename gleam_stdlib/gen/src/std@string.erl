-module(std@string).
-compile(no_auto_import).

-export([length/1, lowercase/1, uppercase/1, reverse/1, split/2, replace/3, append/2]).

length(A) ->
    string:length(A).

lowercase(A) ->
    string:lowercase(A).

uppercase(A) ->
    string:uppercase(A).

reverse(String) ->
    std@iodata:to_string(std@iodata:reverse(std@iodata:new(String))).

split(String, On) ->
    std@list:map(std@iodata:split(std@iodata:new(String), On),
                 fun std@iodata:to_string/1).

replace(String, Pattern, With) ->
    std@iodata:to_string(std@iodata:replace(std@iodata:new(String),
                                            Pattern,
                                            With)).

append(S1, S2) ->
    std@iodata:to_string(std@iodata:append(std@iodata:new(S1), S2)).
