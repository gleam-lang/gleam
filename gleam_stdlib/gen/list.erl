-module(list).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([length/1, reverse/1, is_empty/1, has_member/2, head/1, tail/1, map/2, do_traverse/3, traverse/2, new/0, foldl/3, foldr/3]).

length(A) ->
    erlang:length(A).

-ifdef(TEST).
length_test() ->
    _ = (fun(Capture1) -> expect:equal(Capture1, 0) end)(length([])),
    _ = (fun(Capture1) -> expect:equal(Capture1, 1) end)(length([1])),
    _ = (fun(Capture1) -> expect:equal(Capture1, 2) end)(length([1, 1])),
    (fun(Capture1) -> expect:equal(Capture1, 3) end)(length([1, 1, 1])).
-endif.

reverse(A) ->
    lists:reverse(A).

-ifdef(TEST).
reverse_test() ->
    _ = (fun(Capture1) -> expect:equal(Capture1, 0) end)(length([])),
    (fun(Capture1) -> expect:equal(Capture1, 5) end)(length([1, 2, 3, 4, 5])).
-endif.

is_empty(List) ->
    List =:= [].

-ifdef(TEST).
is_empty_test() ->
    _ = expect:true(is_empty([])),
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
    _ = expect:true(has_member([0, 4, 5, 1], 1)),
    _ = expect:false(has_member([0, 4, 5, 7], 1)),
    expect:false(has_member([], 1)).
-endif.

head(List) ->
    case List of
        [] ->
            {error, empty};

        [X | Xs] ->
            {ok, X}
    end.

-ifdef(TEST).
head_test() ->
    _ = (fun(Capture1) ->
        expect:equal(Capture1, {ok, 0})
    end)(head([0, 4, 5, 7])),
    (fun(Capture1) -> expect:equal(Capture1, {error, empty}) end)(head([])).
-endif.

tail(List) ->
    case List of
        [] ->
            {error, empty};

        [X | Xs] ->
            {ok, Xs}
    end.

-ifdef(TEST).
tail_test() ->
    _ = (fun(Capture1) ->
        expect:equal(Capture1, {ok, [4, 5, 7]})
    end)(tail([0, 4, 5, 7])),
    _ = (fun(Capture1) -> expect:equal(Capture1, {ok, []}) end)(tail([0])),
    (fun(Capture1) -> expect:equal(Capture1, {error, empty}) end)(tail([])).
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
    _ = (fun(Capture1) ->
        expect:equal(Capture1, [])
    end)((fun(Capture1) -> map(Capture1, fun(X) -> X * 2 end) end)([])),
    (fun(Capture1) ->
        expect:equal(Capture1, [0, 8, 10, 14, 6])
    end)((fun(Capture1) ->
             map(Capture1, fun(X) -> X * 2 end)
         end)([0, 4, 5, 7, 3])).
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
    _ = (fun(Capture1) ->
        expect:equal(Capture1, {ok, [10, 12, 10, 12]})
    end)((fun(Capture1) -> traverse(Capture1, Fun) end)([5, 6, 5, 6])),
    (fun(Capture1) ->
        expect:equal(Capture1, {error, 7})
    end)((fun(Capture1) -> traverse(Capture1, Fun) end)([4, 6, 5, 7, 3])).
-endif.

new() ->
    [].

-ifdef(TEST).
new_test() ->
    (fun(Capture1) -> expect:equal(Capture1, []) end)(new()).
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
    (fun(Capture1) ->
        expect:equal(Capture1, [3, 2, 1])
    end)((fun(Capture1) ->
             foldl(Capture1, [], fun(X, Acc) -> [X | Acc] end)
         end)([1, 2, 3])).
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
    (fun(Capture1) ->
        expect:equal(Capture1, [1, 2, 3])
    end)((fun(Capture1) ->
             foldr(Capture1, [], fun(X, Acc) -> [X | Acc] end)
         end)([1, 2, 3])).
-endif.
