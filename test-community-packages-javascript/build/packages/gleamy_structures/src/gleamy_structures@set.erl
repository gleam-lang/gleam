-module(gleamy_structures@set).
-compile([no_auto_import, nowarn_unused_vars]).

-export([contains/2, delete/2, filter/2, fold/3, from_list/2, insert/2, intersection/2, new/1, count/1, to_list/1, union/2, take/2]).

-spec contains(gleamy_structures@tree@red_black_tree:tree(FTU), FTU) -> boolean().
contains(Set, Member) ->
    case gleamy_structures@tree@red_black_tree:find(Set, Member) of
        {ok, _} ->
            true;

        {error, _} ->
            false
    end.

-spec delete(gleamy_structures@tree@red_black_tree:tree(FTW), FTW) -> gleamy_structures@tree@red_black_tree:tree(FTW).
delete(Set, Member) ->
    gleamy_structures@tree@red_black_tree:delete(Set, Member).

-spec filter(
    gleamy_structures@tree@red_black_tree:tree(FTZ),
    fun((FTZ) -> boolean())
) -> gleamy_structures@tree@red_black_tree:tree(FTZ).
filter(Set, Property) ->
    gleamy_structures@tree@red_black_tree:fold(
        Set,
        Set,
        fun(Set@1, I) -> case Property(I) of
                true ->
                    Set@1;

                false ->
                    gleamy_structures@tree@red_black_tree:delete(Set@1, I)
            end end
    ).

-spec fold(
    gleamy_structures@tree@red_black_tree:tree(FUC),
    FUE,
    fun((FUE, FUC) -> FUE)
) -> FUE.
fold(Set, Initial, Reducer) ->
    gleamy_structures@tree@red_black_tree:fold(Set, Initial, Reducer).

-spec from_list(list(FUF), fun((FUF, FUF) -> gleam@order:order())) -> gleamy_structures@tree@red_black_tree:tree(FUF).
from_list(Members, Compare) ->
    gleam@list:fold(
        Members,
        gleamy_structures@tree@red_black_tree:new(Compare),
        fun gleamy_structures@tree@red_black_tree:insert/2
    ).

-spec insert(gleamy_structures@tree@red_black_tree:tree(FUI), FUI) -> gleamy_structures@tree@red_black_tree:tree(FUI).
insert(Set, Member) ->
    gleamy_structures@tree@red_black_tree:insert(Set, Member).

-spec intersection(
    gleamy_structures@tree@red_black_tree:tree(FUL),
    gleamy_structures@tree@red_black_tree:tree(FUL)
) -> gleamy_structures@tree@red_black_tree:tree(FUL).
intersection(First, Second) ->
    gleamy_structures@tree@red_black_tree:fold(
        Second,
        gleamy_structures@tree@red_black_tree:clear(First),
        fun(A, I) ->
            case gleamy_structures@tree@red_black_tree:find(First, I) of
                {ok, _} ->
                    gleamy_structures@tree@red_black_tree:insert(A, I);

                {error, _} ->
                    A
            end
        end
    ).

-spec new(fun((FUP, FUP) -> gleam@order:order())) -> gleamy_structures@tree@red_black_tree:tree(FUP).
new(Compare) ->
    gleamy_structures@tree@red_black_tree:new(Compare).

-spec count(gleamy_structures@tree@red_black_tree:tree(any())) -> integer().
count(Set) ->
    gleamy_structures@tree@red_black_tree:fold(Set, 0, fun(A, _) -> A + 1 end).

-spec to_list(gleamy_structures@tree@red_black_tree:tree(FUX)) -> list(FUX).
to_list(Set) ->
    gleamy_structures@tree@red_black_tree:foldr(
        Set,
        [],
        fun(A, I) ->
            gleam@io:println(gleam@string:inspect(I)),
            [I | A]
        end
    ).

-spec union(
    gleamy_structures@tree@red_black_tree:tree(FVA),
    gleamy_structures@tree@red_black_tree:tree(FVA)
) -> gleamy_structures@tree@red_black_tree:tree(FVA).
union(First, Second) ->
    gleamy_structures@tree@red_black_tree:fold(
        First,
        Second,
        fun(A, I) -> gleamy_structures@tree@red_black_tree:insert(A, I) end
    ).

-spec take(gleamy_structures@tree@red_black_tree:tree(FUT), list(FUT)) -> gleamy_structures@tree@red_black_tree:tree(FUT).
take(Set, Desired) ->
    case Desired of
        [X | Xs] ->
            case gleamy_structures@tree@red_black_tree:find(Set, X) of
                {ok, X@1} ->
                    gleamy_structures@tree@red_black_tree:insert(
                        take(Set, Xs),
                        X@1
                    );

                {error, _} ->
                    take(Set, Xs)
            end;

        [] ->
            gleamy_structures@tree@red_black_tree:clear(Set)
    end.
