-module(gleam@triple).
-compile(no_auto_import).

-export([first/1, second/1, third/1]).

first(Trip) ->
    {A, _, _} = Trip,
    A.

second(Trip) ->
    {_, A, _} = Trip,
    A.

third(Trip) ->
    {_, _, A} = Trip,
    A.
