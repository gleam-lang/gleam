-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-spec first({ET, any()}) -> ET.
first(Pair) ->
    {A, _} = Pair,
    A.

-spec second({any(), EW}) -> EW.
second(Pair) ->
    {_, A} = Pair,
    A.

-spec swap({EX, EY}) -> {EY, EX}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-spec map_first({EZ, FA}, fun((EZ) -> FB)) -> {FB, FA}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-spec map_second({FC, FD}, fun((FD) -> FE)) -> {FC, FE}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-spec new(FF, FG) -> {FF, FG}.
new(First, Second) ->
    {First, Second}.
