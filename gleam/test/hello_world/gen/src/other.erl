-module(other).
-compile(no_auto_import).

-export([reverse/1, is_empty/1, contains/2, head/1]).

reverse(A) ->
    lists:reverse(A).

is_empty(List) ->
    List =:= [].

contains(List, Elem) ->
    case List of
        [] ->
            false;

        [Head | Rest] ->
            Head =:= Elem orelse contains(Rest, Elem)
    end.

head(List) ->
    case List of
        [] ->
            {error, empty};

        [X | _] ->
            {ok, X}
    end.

run() ->
    Origin = {4, 8},
    Cursor = {7, 6},
    <<"ok!">>.
