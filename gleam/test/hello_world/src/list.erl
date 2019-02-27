-module(gleam_list).

-export([length/1, reverse/1, is_empty/1, new/0, member/2, head/1, tail/1, filter/2, map/2, do_traverse/3, traverse/2, drop/2, take/2, foldl/3, foldr/3]).

length(A) ->
    erlang:length(A).

reverse(A) ->
    erlang:reverse(A).

is_empty(List) ->
    List =:= [].

new() ->
    [].

member(List, Elem) ->
    case List of
        [] ->
            false;

        [Head | Rest] ->
            Head =:= Elem orelse member(Rest, Elem)
    end.

head(List) ->
    case List of
        [] ->
            {error, empty};

        [X | Xs] ->
            {ok, X}
    end.

tail(List) ->
    case List of
        [] ->
            {error, empty};

        [X | Xs] ->
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
    case N =:= 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [X | Xs] ->
                    drop(Xs, N - 1)
            end
    end.

do_take(List, N, Acc) ->
    case N =:= 0 of
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

foldl(List, Acc, Fun) ->
    case List of
        [] ->
            Acc;

        [X | Rest] ->
            foldl(Rest, Fun(X, Acc), Fun)
    end.

foldr(List, Acc, Fun) ->
    case List of
        [] ->
            Acc;

        [X | Rest] ->
            Fun(X, foldr(Rest, Acc, Fun))
    end.
