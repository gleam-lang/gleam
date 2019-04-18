-module(list_test).
-compile(no_auto_import).

-export([length_test/0, reverse_test/0, is_empty_test/0, contains_test/0, head_test/0, tail_test/0, filter_test/0, map_test/0, traverse_test/0, drop_test/0, take_test/0, new_test/0, append_test/0, flatten_test/0, fold_test/0, fold_right_test/0, find_test/0, all_test/0, any_test/0, zip_test/0, intersperse_test/0, at_test/0, unique_test/0, sort_test/0, index_map_test/0]).

length_test() ->
    expect:equal(list:length([]), 0),
    expect:equal(list:length([1]), 1),
    expect:equal(list:length([1, 1]), 2),
    expect:equal(list:length([1, 1, 1]), 3).

reverse_test() ->
    expect:equal(list:reverse([]), []),
    expect:equal(list:reverse([1, 2, 3, 4, 5]), [5, 4, 3, 2, 1]).

is_empty_test() ->
    expect:true(list:is_empty([])),
    expect:false(list:is_empty([1])).

contains_test() ->
    expect:true(list:contains([0, 4, 5, 1], 1)),
    expect:false(list:contains([0, 4, 5, 7], 1)),
    expect:false(list:contains([], 1)).

head_test() ->
    expect:equal(list:head([0, 4, 5, 7]), {ok, 0}),
    expect:is_error(list:head([])).

tail_test() ->
    expect:equal(list:tail([0, 4, 5, 7]), {ok, [4, 5, 7]}),
    expect:equal(list:tail([0]), {ok, []}),
    expect:is_error(list:tail([])).

filter_test() ->
    expect:equal(list:filter([], fun(_) -> true end), []),
    expect:equal(list:filter([0, 4, 5, 7, 3], fun(_) -> true end),
                 [0, 4, 5, 7, 3]),
    expect:equal(list:filter([0, 4, 5, 7, 3], fun(X) -> X > 4 end), [5, 7]),
    expect:equal(list:filter([0, 4, 5, 7, 3], fun(X) -> X < 4 end), [0, 3]).

map_test() ->
    expect:equal(list:map([], fun(X) -> X * 2 end), []),
    expect:equal(list:map([0, 4, 5, 7, 3], fun(X) -> X * 2 end),
                 [0, 8, 10, 14, 6]).

traverse_test() ->
    Fun = fun(X) -> case X =:= 6 orelse X =:= 5 orelse X =:= 4 of
            true ->
                {ok, X * 2};

            false ->
                {error, X}
        end end,
    expect:equal(list:traverse([5, 6, 5, 6], Fun), {ok, [10, 12, 10, 12]}),
    expect:equal(list:traverse([4, 6, 5, 7, 3], Fun), {error, 7}).

drop_test() ->
    expect:equal(list:drop([], 5), []),
    expect:equal(list:drop([1, 2, 3, 4, 5, 6, 7, 8], 5), [6, 7, 8]).

take_test() ->
    expect:equal(list:take([], 5), []),
    expect:equal(list:take([1, 2, 3, 4, 5, 6, 7, 8], 5), [1, 2, 3, 4, 5]).

new_test() ->
    expect:equal(list:new(), []).

append_test() ->
    expect:equal(list:append([1], [2, 3]), [1, 2, 3]).

flatten_test() ->
    expect:equal(list:flatten([]), []),
    expect:equal(list:flatten([[]]), []),
    expect:equal(list:flatten([[], [], []]), []),
    expect:equal(list:flatten([[1, 2], [], [3, 4]]), [1, 2, 3, 4]).

fold_test() ->
    expect:equal(list:fold([1, 2, 3], [], fun(X, Acc) -> [X | Acc] end),
                 [3, 2, 1]).

fold_right_test() ->
    expect:equal(list:fold_right([1, 2, 3], [], fun(X, Acc) -> [X | Acc] end),
                 [1, 2, 3]).

find_test() ->
    F = fun(X) -> case X of
            2 ->
                {ok, 4};

            _ ->
                {error, 0}
        end end,
    expect:equal(list:find([1, 2, 3], F), {ok, 4}),
    expect:equal(list:find([1, 3, 2], F), {ok, 4}),
    expect:is_error(list:find([1, 3], F)).

all_test() ->
    expect:equal(list:all([1, 2, 3, 4, 5], fun(X) -> X > 0 end), true),
    expect:equal(list:all([1, 2, 3, 4, 5], fun(X) -> X < 0 end), false),
    expect:equal(list:all([], fun(_) -> false end), true).

any_test() ->
    expect:equal(list:any([1, 2, 3, 4, 5], fun(X) -> X =:= 2 end), true),
    expect:equal(list:any([1, 2, 3, 4, 5], fun(X) -> X < 0 end), false),
    expect:equal(list:any([], fun(_) -> false end), false).

zip_test() ->
    expect:equal(list:zip([], [1, 2, 3]), []),
    expect:equal(list:zip([1, 2], []), []),
    expect:equal(list:zip([1, 2, 3], [4, 5, 6]), [{1, 4}, {2, 5}, {3, 6}]),
    expect:equal(list:zip([5, 6], [1, 2, 3]), [{5, 1}, {6, 2}]),
    expect:equal(list:zip([5, 6, 7], [1, 2]), [{5, 1}, {6, 2}]).

intersperse_test() ->
    expect:equal(list:intersperse([1, 2, 3], 4), [1, 4, 2, 4, 3]),
    expect:equal(list:intersperse([], 2), []).

at_test() ->
    expect:equal(list:at([1, 2, 3], 2), {ok, 3}),
    expect:is_error(list:at([1, 2, 3], 5)),
    expect:is_error(list:at([], 0)),
    expect:is_error(list:at([1, 2, 3, 4, 5, 6], -1)).

unique_test() ->
    expect:equal(list:unique([1, 1, 2, 3, 4, 4, 4, 5, 6]), [1, 2, 3, 4, 5, 6]),
    expect:equal(list:unique([7, 1, 45, 6, 2, 47, 2, 7, 5]),
                 [7, 1, 45, 6, 2, 47, 5]),
    expect:equal(list:unique([3, 4, 5]), [3, 4, 5]),
    expect:equal(list:unique([]), []).

sort_test() ->
    expect:equal(list:sort([4, 3, 6, 5, 4]), [3, 4, 4, 5, 6]),
    expect:equal(list:sort([]), []),
    expect:equal(list:sort([{1, 2}, {4, 5}, {3, 2}]), [{1, 2}, {3, 2}, {4, 5}]).

index_map_test() ->
    expect:equal(list:index_map([3, 4, 5], fun(I, X) -> {I, X} end),
                 [{0, 3}, {1, 4}, {2, 5}]),
    F = fun(I, X) ->
        iodata:to_string(iodata:append(iodata:new(X), str:from_int(I)))
    end,
    expect:equal(list:index_map([<<"a">>, <<"b">>, <<"c">>], F),
                 [<<"a0">>, <<"b1">>, <<"c2">>]).
