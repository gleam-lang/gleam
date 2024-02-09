-module(gleamy_structures@non_empty_list).
-compile([no_auto_import, nowarn_unused_vars]).

-export([from_list/1, fold/3, count/1, filter/2, to_list/1, reverse/1, map/2]).
-export_type([non_empty_list/1]).

-type non_empty_list(HAU) :: {'end', HAU} | {next, HAU, non_empty_list(HAU)}.

-spec from_list(list(HBK)) -> {ok, non_empty_list(HBK)} | {error, nil}.
from_list(List) ->
    case List of
        [] ->
            {error, nil};

        [X | Xs] ->
            {ok,
                gleam@list:fold(
                    Xs,
                    {'end', X},
                    fun(Acc, Item) -> {next, Item, Acc} end
                )}
    end.

-spec fold(non_empty_list(HAV), HAX, fun((HAX, HAV) -> HAX)) -> HAX.
fold(List, Initial, Fun) ->
    case List of
        {'end', Item} ->
            Fun(Initial, Item);

        {next, X, Xs} ->
            fold(Xs, Fun(Initial, X), Fun)
    end.

-spec count(non_empty_list(any())) -> integer().
count(List) ->
    fold(List, 0, fun(Acc, _) -> Acc + 1 end).

-spec filter(non_empty_list(HBE), fun((HBE) -> boolean())) -> list(HBE).
filter(List, Predicate) ->
    _pipe = fold(List, [], fun(Acc, Item) -> case Predicate(Item) of
                true ->
                    [Item | Acc];

                false ->
                    Acc
            end end),
    gleam@list:reverse(_pipe).

-spec to_list(non_empty_list(HBH)) -> list(HBH).
to_list(List) ->
    _pipe = fold(List, [], fun(Acc, Item) -> [Item | Acc] end),
    gleam@list:reverse(_pipe).

-spec reverse(non_empty_list(HBP)) -> non_empty_list(HBP).
reverse(List) ->
    case List of
        {'end', _} ->
            List;

        {next, X, Xs} ->
            fold(Xs, {'end', X}, fun(Acc, X@1) -> {next, X@1, Acc} end)
    end.

-spec map(non_empty_list(HBA), fun((HBA) -> HBC)) -> non_empty_list(HBC).
map(List, Transform) ->
    case List of
        {'end', X} ->
            {'end', Transform(X)};

        {next, X@1, Xs} ->
            _pipe = fold(
                Xs,
                {'end', Transform(X@1)},
                fun(Acc, Item) -> {next, Transform(Item), Acc} end
            ),
            reverse(_pipe)
    end.
