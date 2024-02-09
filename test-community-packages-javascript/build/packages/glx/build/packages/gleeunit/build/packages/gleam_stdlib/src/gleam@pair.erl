-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-spec first({IJ, any()}) -> IJ.
first(Pair) ->
    {A, _} = Pair,
    A.

-spec second({any(), IM}) -> IM.
second(Pair) ->
    {_, A} = Pair,
    A.

-spec swap({IN, IO}) -> {IO, IN}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-spec map_first({IP, IQ}, fun((IP) -> IR)) -> {IR, IQ}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-spec map_second({IS, IT}, fun((IT) -> IU)) -> {IS, IU}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-spec new(IV, IW) -> {IV, IW}.
new(First, Second) ->
    {First, Second}.
