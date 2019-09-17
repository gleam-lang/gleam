-module(gleam@list).
-compile(no_auto_import).

-export([length/1, reverse/1, is_empty/1, contains/2, head/1, tail/1, filter/2, map/2, index_map/2, traverse/2, drop/2, take/2, new/0, append/2, flatten/1, fold/3, fold_right/3, find/2, all/2, any/2, zip/2, strict_zip/2, intersperse/2, at/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2]).

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
            {error, {}};

        [X | _] ->
            {ok, X}
    end.

tail(List) ->
    case List of
        [] ->
            {error, {}};

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

filter(List, Fun) ->
    do_filter(List, Fun, []).

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

fold(List, Acc, Fun) ->
    case List of
        [] ->
            Acc;

        [X | Rest] ->
            fold(Rest, Fun(X, Acc), Fun)
    end.

fold_right(List, Acc, Fun) ->
    case List of
        [] ->
            Acc;

        [X | Rest] ->
            Fun(X, fold_right(Rest, Acc, Fun))
    end.

find(Haystack, F) ->
    case Haystack of
        [] ->
            {error, {}};

        [X | Rest] ->
            case F(X) of
                {ok, X1} ->
                    {ok, X1};

                _ ->
                    find(Rest, F)
            end
    end.

all(List, F) ->
    case List of
        [] ->
            true;

        [X | Rest] ->
            case F(X) of
                true ->
                    all(Rest, F);

                _ ->
                    false
            end
    end.

any(List, F) ->
    case List of
        [] ->
            false;

        [X | Rest] ->
            case F(X) of
                false ->
                    any(Rest, F);

                _ ->
                    true
            end
    end.

zip(L1, L2) ->
    case {L1, L2} of
        {[], _} ->
            [];

        {_, []} ->
            [];

        {[X1 | Rest1], [X2 | Rest2]} ->
            [{X1, X2} | zip(Rest1, Rest2)]
    end.

strict_zip(L1, L2) ->
    case length(L1) =:= length(L2) of
        true ->
            {ok, zip(L1, L2)};

        false ->
            {error, {}}
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

at(List, I) ->
    case I < 0 of
        true ->
            {error, {}};

        false ->
            case List of
                [] ->
                    {error, {}};

                [X | Rest] ->
                    case I =:= 0 of
                        true ->
                            {ok, X};

                        false ->
                            at(Rest, I - 1)
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

split(List, N) ->
    do_split(List, N, []).

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

split_while(List, F) ->
    do_split_while(List, F, []).

key_find(Haystack, Needle) ->
    find(Haystack, fun(P) -> case gleam@pair:first(P) =:= Needle of
                true ->
                    {ok, gleam@pair:second(P)};

                false ->
                    {error, nil}
            end end).
