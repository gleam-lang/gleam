-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/pair.gleam", 10).
-spec first({YZ, any()}) -> YZ.
first(Pair) ->
    {A, _} = Pair,
    A.

-file("/Users/louis/src/gleam/stdlib/src/gleam/pair.gleam", 24).
-spec second({any(), AAC}) -> AAC.
second(Pair) ->
    {_, A} = Pair,
    A.

-file("/Users/louis/src/gleam/stdlib/src/gleam/pair.gleam", 38).
-spec swap({AAD, AAE}) -> {AAE, AAD}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/pair.gleam", 53).
-spec map_first({AAF, AAG}, fun((AAF) -> AAH)) -> {AAH, AAG}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/pair.gleam", 68).
-spec map_second({AAI, AAJ}, fun((AAJ) -> AAK)) -> {AAI, AAK}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/pair.gleam", 83).
-spec new(AAL, AAM) -> {AAL, AAM}.
new(First, Second) ->
    {First, Second}.
