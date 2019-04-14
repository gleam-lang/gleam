-module(tuple).
-compile(no_auto_import).

-export([new/2, first/1, second/1, swap/1, fetch/2]).

new(A, B) ->
    {A, B}.

first(Tup) ->
    {A, _} = Tup,
    A.

second(Tup) ->
    {_, A} = Tup,
    A.

swap(Tup) ->
    {A, B} = Tup,
    {B, A}.

fetch(Haystack, Needle) ->
    list:find(Haystack, fun(Tuple) -> case first(Tuple) =:= Needle of
                      true ->
                          {ok, second(Tuple)};

                      false ->
                          {error, []}
                  end end).
