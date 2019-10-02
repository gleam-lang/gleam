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

map_first(Tup, F) ->
    {A, B} = Tup,
    {F(A), B}.

map_second(Tup, F) ->
    {A, B} = Tup,
    {A, F(B)}.
