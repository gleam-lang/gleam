-module(gleam@set).
-compile([no_auto_import, nowarn_unused_vars]).

-export([new/0, size/1, insert/2, contains/2, delete/2, to_list/1, from_list/1, fold/3, filter/2, drop/2, take/2, union/2, intersection/2]).
-export_type([set/1]).

-opaque set(EOC) :: {set, gleam@map:map_(EOC, list(nil))}.

-spec new() -> set(any()).
new() ->
    {set, gleam@map:new()}.

-spec size(set(any())) -> integer().
size(Set) ->
    gleam@map:size(erlang:element(2, Set)).

-spec insert(set(EOI), EOI) -> set(EOI).
insert(Set, Member) ->
    {set, gleam@map:insert(erlang:element(2, Set), Member, [])}.

-spec contains(set(EOL), EOL) -> boolean().
contains(Set, Member) ->
    _pipe = erlang:element(2, Set),
    _pipe@1 = gleam@map:get(_pipe, Member),
    gleam@result:is_ok(_pipe@1).

-spec delete(set(EON), EON) -> set(EON).
delete(Set, Member) ->
    {set, gleam@map:delete(erlang:element(2, Set), Member)}.

-spec to_list(set(EOQ)) -> list(EOQ).
to_list(Set) ->
    gleam@map:keys(erlang:element(2, Set)).

-spec from_list(list(EOT)) -> set(EOT).
from_list(Members) ->
    Map = gleam@list:fold(
        Members,
        gleam@map:new(),
        fun(M, K) -> gleam@map:insert(M, K, []) end
    ),
    {set, Map}.

-spec fold(set(EOW), EOY, fun((EOY, EOW) -> EOY)) -> EOY.
fold(Set, Initial, Reducer) ->
    gleam@map:fold(
        erlang:element(2, Set),
        Initial,
        fun(A, K, _) -> Reducer(A, K) end
    ).

-spec filter(set(EOZ), fun((EOZ) -> boolean())) -> set(EOZ).
filter(Set, Property) ->
    {set,
        gleam@map:filter(erlang:element(2, Set), fun(M, _) -> Property(M) end)}.

-spec drop(set(EPC), list(EPC)) -> set(EPC).
drop(Set, Disallowed) ->
    gleam@list:fold(Disallowed, Set, fun delete/2).

-spec take(set(EPG), list(EPG)) -> set(EPG).
take(Set, Desired) ->
    {set, gleam@map:take(erlang:element(2, Set), Desired)}.

-spec order(set(EPK), set(EPK)) -> {set(EPK), set(EPK)}.
order(First, Second) ->
    case gleam@map:size(erlang:element(2, First)) > gleam@map:size(
        erlang:element(2, Second)
    ) of
        true ->
            {First, Second};

        false ->
            {Second, First}
    end.

-spec union(set(EPP), set(EPP)) -> set(EPP).
union(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    fold(Smaller, Larger, fun insert/2).

-spec intersection(set(EPT), set(EPT)) -> set(EPT).
intersection(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    take(Larger, to_list(Smaller)).
