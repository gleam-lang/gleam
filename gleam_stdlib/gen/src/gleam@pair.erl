-module(gleam@pair).
-compile(no_auto_import).

-export([first/1, second/1, swap/1, map_first/2, map_second/2]).

first(Tup) ->
    {A, _} = Tup,
    A.

second(Tup) ->
    {_, A} = Tup,
    A.

swap(Tup) ->
    {A, B} = Tup,
    {B, A}.

map_first(Tup, Fun) ->
    {A, B} = Tup,
    {Fun(A), B}.

map_second(Tup, Fun) ->
    {A, B} = Tup,
    {A, Fun(B)}.
