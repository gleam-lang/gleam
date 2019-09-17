-module(gleam@pair).
-compile(no_auto_import).

-export([first/1, second/1, swap/1]).

first(Tup) ->
    {A, _} = Tup,
    A.

second(Tup) ->
    {_, A} = Tup,
    A.

swap(Tup) ->
    {A, B} = Tup,
    {B, A}.
