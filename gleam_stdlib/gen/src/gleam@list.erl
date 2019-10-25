-module(gleam@list).
-compile(no_auto_import).

-export([length/1, reverse/1, is_empty/1, contains/2, head/1, tail/1, filter/2, map/2, index_map/2, traverse/2, drop/2, take/2, new/0, append/2, flatten/1, fold/3, fold_right/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, intersperse/2, at/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2]).

length(A) ->
    erlang:length(A).

reverse(A) ->
    lists:reverse(A).

is_empty(List) ->
    List =:= [].

contains(List, Elem) ->
    case List of
        [] ->
            false;

        [Head | Rest] ->
            Head =:= Elem orelse contains(Rest, Elem)
    end.

head(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _] ->
            {ok, X}
    end.

tail(List) ->
    case List of
        [] ->
            {error, nil};

        [_ | Xs] ->
            {ok, Xs}
    end.

do_filter(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            NewAcc = case Fun(X) of
                true ->
                    [X | Acc];

                false ->
                    Acc
            end,
            do_filter(Xs, Fun, NewAcc)
    end.

filter(List, Predicate) ->
    do_filter(List, Predicate, []).

do_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            do_map(Xs, Fun, [Fun(X) | Acc])
    end.

map(List, Fun) ->
    do_map(List, Fun, []).

do_index_map(List, Fun, Index, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            do_index_map(Xs, Fun, Index + 1, [Fun(Index, X) | Acc])
    end.

index_map(List, Fun) ->
    do_index_map(List, Fun, 0, []).

do_traverse(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, reverse(Acc)};

        [X | Xs] ->
            case Fun(X) of
                {ok, Y} ->
                    do_traverse(Xs, Fun, [Y | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

traverse(List, Fun) ->
    do_traverse(List, Fun, []).

drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_ | Xs] ->
                    drop(Xs, N - 1)
            end
    end.

do_take(List, N, Acc) ->
    case N =< 0 of
        true ->
            reverse(Acc);

        false ->
            case List of
                [] ->
                    reverse(Acc);

                [X | Xs] ->
                    do_take(Xs, N - 1, [X | Acc])
            end
    end.

take(List, N) ->
    do_take(List, N, []).

new() ->
    [].

append(A, B) ->
    lists:append(A, B).

do_flatten(Lists, Acc) ->
    case Lists of
        [] ->
            Acc;

        [L | Rest] ->
            do_flatten(Rest, append(Acc, L))
    end.

flatten(Lists) ->
    do_flatten(Lists, []).

fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(X, Initial), Fun)
    end.

fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(X, fold_right(Rest, Initial, Fun))
    end.

find(Haystack, IsDesired) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case IsDesired(X) of
                true ->
                    {ok, X};

                _ ->
                    find(Rest, IsDesired)
            end
    end.

find_map(Haystack, Fun) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X1} ->
                    {ok, X1};

                _ ->
                    find_map(Rest, Fun)
            end
    end.

all(List, Predicate) ->
    case List of
        [] ->
            true;

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    all(Rest, Predicate);

                _ ->
                    false
            end
    end.

any(List, Predicate) ->
    case List of
        [] ->
            false;

        [X | Rest] ->
            case Predicate(X) of
                false ->
                    any(Rest, Predicate);

                _ ->
                    true
            end
    end.

zip(Xs, Ys) ->
    case {Xs, Ys} of
        {[], _} ->
            [];

        {_, []} ->
            [];

        {[X | Xs1], [Y | Ys1]} ->
            [{X, Y} | zip(Xs1, Ys1)]
    end.

strict_zip(L1, L2) ->
    case length(L1) =:= length(L2) of
        true ->
            {ok, zip(L1, L2)};

        false ->
            {error, length_mismatch}
    end.

intersperse(List, Elem) ->
    case List of
        [] ->
            [];

        [X] ->
            [X];

        [X1 | Rest] ->
            [X1, Elem | intersperse(Rest, Elem)]
    end.

at(List, Index) ->
    case Index < 0 of
        true ->
            {error, nil};

        false ->
            case List of
                [] ->
                    {error, nil};

                [X | Rest] ->
                    case Index =:= 0 of
                        true ->
                            {ok, X};

                        false ->
                            at(Rest, Index - 1)
                    end
            end
    end.

unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

merge_sort(A, B, Compare) ->
    case {A, B} of
        {[], _} ->
            B;

        {_, []} ->
            A;

        {[Ax | Ar], [Bx | Br]} ->
            case Compare(Ax, Bx) of
                lt ->
                    [Ax | merge_sort(Ar, B, Compare)];

                _ ->
                    [Bx | merge_sort(A, Br, Compare)]
            end
    end.

do_sort(List, Compare, ListLength) ->
    case ListLength < 2 of
        true ->
            List;

        false ->
            SplitLength = ListLength div 2,
            AList = take(List, SplitLength),
            BList = drop(List, SplitLength),
            merge_sort(
                do_sort(AList, Compare, SplitLength),
                do_sort(BList, Compare, ListLength - SplitLength),
                Compare
            )
    end.

sort(List, Compare) ->
    do_sort(List, Compare, length(List)).

range(Start, Stop) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            [];

        gt ->
            [Start | range(Start - 1, Stop)];

        lt ->
            [Start | range(Start + 1, Stop)]
    end.

do_repeat(A, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            do_repeat(A, Times - 1, [A | Acc])
    end.

repeat(A, Times) ->
    do_repeat(A, Times, []).

do_split(List, N, Taken) ->
    case N =< 0 of
        true ->
            {reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {reverse(Taken), []};

                [X | Xs] ->
                    do_split(Xs, N - 1, [X | Taken])
            end
    end.

split(List, Target) ->
    do_split(List, Target, []).

do_split_while(List, F, Acc) ->
    case List of
        [] ->
            {reverse(Acc), []};

        [X | Xs] ->
            case F(X) of
                false ->
                    {reverse(Acc), List};

                _ ->
                    do_split_while(Xs, F, [X | Acc])
            end
    end.

split_while(List, Predicate) ->
    do_split_while(List, Predicate, []).

key_find(Haystack, Needle) ->
    find_map(Haystack, fun(P) -> case gleam@pair:first(P) =:= Needle of
                true ->
                    {ok, gleam@pair:second(P)};

                false ->
                    {error, nil}
            end end).
