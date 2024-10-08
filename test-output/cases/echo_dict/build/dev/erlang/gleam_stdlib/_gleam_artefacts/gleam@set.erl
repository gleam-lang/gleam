-module(gleam@set).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, size/1, is_empty/1, contains/2, delete/2, to_list/1, fold/3, filter/2, drop/2, take/2, intersection/2, difference/2, is_subset/2, is_disjoint/2, each/2, insert/2, from_list/1, map/2, union/2, symmetric_difference/2]).
-export_type([set/1]).

-opaque set(FCQ) :: {set, gleam@dict:dict(FCQ, list(nil))}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 32).
-spec new() -> set(any()).
new() ->
    {set, gleam@dict:new()}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 50).
-spec size(set(any())) -> integer().
size(Set) ->
    maps:size(erlang:element(2, Set)).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 68).
-spec is_empty(set(any())) -> boolean().
is_empty(Set) ->
    Set =:= new().

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 110).
-spec contains(set(FDB), FDB) -> boolean().
contains(Set, Member) ->
    _pipe = erlang:element(2, Set),
    _pipe@1 = gleam@dict:get(_pipe, Member),
    gleam@result:is_ok(_pipe@1).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 131).
-spec delete(set(FDD), FDD) -> set(FDD).
delete(Set, Member) ->
    {set, gleam@dict:delete(erlang:element(2, Set), Member)}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 149).
-spec to_list(set(FDG)) -> list(FDG).
to_list(Set) ->
    gleam@dict:keys(erlang:element(2, Set)).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 190).
-spec fold(set(FDM), FDO, fun((FDO, FDM) -> FDO)) -> FDO.
fold(Set, Initial, Reducer) ->
    gleam@dict:fold(
        erlang:element(2, Set),
        Initial,
        fun(A, K, _) -> Reducer(A, K) end
    ).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 214).
-spec filter(set(FDP), fun((FDP) -> boolean())) -> set(FDP).
filter(Set, Predicate) ->
    {set,
        gleam@dict:filter(erlang:element(2, Set), fun(M, _) -> Predicate(M) end)}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 249).
-spec drop(set(FDW), list(FDW)) -> set(FDW).
drop(Set, Disallowed) ->
    gleam@list:fold(Disallowed, Set, fun delete/2).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 267).
-spec take(set(FEA), list(FEA)) -> set(FEA).
take(Set, Desired) ->
    {set, gleam@dict:take(erlang:element(2, Set), Desired)}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 271).
-spec order(set(FEE), set(FEE)) -> {set(FEE), set(FEE)}.
order(First, Second) ->
    case maps:size(erlang:element(2, First)) > maps:size(
        erlang:element(2, Second)
    ) of
        true ->
            {First, Second};

        false ->
            {Second, First}
    end.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 305).
-spec intersection(set(FEN), set(FEN)) -> set(FEN).
intersection(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    take(Larger, to_list(Smaller)).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 323).
-spec difference(set(FER), set(FER)) -> set(FER).
difference(First, Second) ->
    drop(First, to_list(Second)).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 344).
-spec is_subset(set(FEV), set(FEV)) -> boolean().
is_subset(First, Second) ->
    intersection(First, Second) =:= First.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 362).
-spec is_disjoint(set(FEY), set(FEY)) -> boolean().
is_disjoint(First, Second) ->
    intersection(First, Second) =:= new().

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 402).
-spec each(set(FFF), fun((FFF) -> any())) -> nil.
each(Set, Fun) ->
    fold(
        Set,
        nil,
        fun(Nil, Member) ->
            Fun(Member),
            Nil
        end
    ).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 86).
-spec insert(set(FCY), FCY) -> set(FCY).
insert(Set, Member) ->
    {set, gleam@dict:insert(erlang:element(2, Set), Member, [])}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 167).
-spec from_list(list(FDJ)) -> set(FDJ).
from_list(Members) ->
    Dict = gleam@list:fold(
        Members,
        gleam@dict:new(),
        fun(M, K) -> gleam@dict:insert(M, K, []) end
    ),
    {set, Dict}.

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 232).
-spec map(set(FDS), fun((FDS) -> FDU)) -> set(FDU).
map(Set, Fun) ->
    fold(Set, new(), fun(Acc, Member) -> insert(Acc, Fun(Member)) end).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 289).
-spec union(set(FEJ), set(FEJ)) -> set(FEJ).
union(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    fold(Smaller, Larger, fun insert/2).

-file("/Users/giacomocavalieri/Documents/progetti/gleam/test-output/cases/echo_dict/build/packages/gleam_stdlib/src/gleam/set.gleam", 374).
-spec symmetric_difference(set(FFB), set(FFB)) -> set(FFB).
symmetric_difference(First, Second) ->
    difference(union(First, Second), intersection(First, Second)).
