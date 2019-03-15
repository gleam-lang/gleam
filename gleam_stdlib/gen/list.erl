-module(list).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([length/1, reverse/1, is_empty/1, has_member/2, head/1, tail/1, map/2, do_traverse/3, traverse/2, new/0, append/2, flatten/1, foldl/3, foldr/3]).

length(A) ->
    erlang:length(A).

-ifdef(TEST).
length_test() ->
    expect:equal(length([]), 0),
    expect:equal(length([1]), 1),
    expect:equal(length([1, 1]), 2),
    expect:equal(length([1, 1, 1]), 3).
-endif.

reverse(A) ->
    lists:reverse(A).

-ifdef(TEST).
reverse_test() ->
    expect:equal(length([]), 0),
    expect:equal(length([1, 2, 3, 4, 5]), 5).
-endif.

is_empty(List) ->
    List =:= [].

-ifdef(TEST).
is_empty_test() ->
    expect:true(is_empty([])),
    expect:false(is_empty([1])).
-endif.

has_member(List, Elem) ->
    case List of
        [] ->
            false;

        [Head | Rest] ->
            Head =:= Elem orelse has_member(Rest, Elem)
    end.

-ifdef(TEST).
has_member_test() ->
    expect:true(has_member([0, 4, 5, 1], 1)),
    expect:false(has_member([0, 4, 5, 7], 1)),
    expect:false(has_member([], 1)).
-endif.

head(List) ->
    case List of
        [] ->
            {error, empty};

        [X | _] ->
            {ok, X}
    end.

-ifdef(TEST).
head_test() ->
    expect:equal(head([0, 4, 5, 7]), {ok, 0}),
    expect:equal(head([]), {error, empty}).
-endif.

tail(List) ->
    case List of
        [] ->
            {error, empty};

        [_ | Xs] ->
            {ok, Xs}
    end.

-ifdef(TEST).
tail_test() ->
    expect:equal(tail([0, 4, 5, 7]), {ok, [4, 5, 7]}),
    expect:equal(tail([0]), {ok, []}),
    expect:equal(tail([]), {error, empty}).
-endif.

do_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            do_map(Xs, Fun, [Fun(X) | Acc])
    end.

map(List, Fun) ->
    do_map(List, Fun, []).

-ifdef(TEST).
map_test() ->
    expect:equal(map([], fun(X) -> X * 2 end), []),
    expect:equal(map([0, 4, 5, 7, 3], fun(X) -> X * 2 end), [0, 8, 10, 14, 6]).
-endif.

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

-ifdef(TEST).
traverse_test() ->
    Fun = fun(X) -> case X =:= 6 orelse X =:= 5 orelse X =:= 4 of
            true ->
                {ok, X * 2};

            false ->
                {error, X}
        end end,
    expect:equal(traverse([5, 6, 5, 6], Fun), {ok, [10, 12, 10, 12]}),
    expect:equal(traverse([4, 6, 5, 7, 3], Fun), {error, 7}).
-endif.

new() ->
    [].

-ifdef(TEST).
new_test() ->
    expect:equal(new(), []).
-endif.

append(A, B) ->
    lists:append(A, B).

-ifdef(TEST).
append_test() ->
    expect:equal(append([1], [2, 3]), [1, 2, 3]).
-endif.

do_flatten(Lists, Acc) ->
    case Lists of
        [] ->
            Acc;

        [L | Rest] ->
            do_flatten(Rest, append(Acc, L))
    end.

flatten(Lists) ->
    do_flatten(Lists, []).

-ifdef(TEST).
flatten_test() ->
    expect:equal(flatten([]), []),
    expect:equal(flatten([[]]), []),
    expect:equal(flatten([[], [], []]), []),
    expect:equal(flatten([[1, 2], [], [3, 4]]), [1, 2, 3, 4]).
-endif.

foldl(List, Acc, Fun) ->
    case List of
        [] ->
            Acc;

        [X | Rest] ->
            foldl(Rest, Fun(X, Acc), Fun)
    end.

-ifdef(TEST).
foldl_test() ->
    expect:equal(foldl([1, 2, 3], [], fun(X, Acc) -> [X | Acc] end), [3, 2, 1]).
-endif.

foldr(List, Acc, Fun) ->
    case List of
        [] ->
            Acc;

        [X | Rest] ->
            Fun(X, foldr(Rest, Acc, Fun))
    end.

-ifdef(TEST).
foldr_test() ->
    expect:equal(foldr([1, 2, 3], [], fun(X, Acc) -> [X | Acc] end), [1, 2, 3]).
-endif.
