-module(map).
-include_lib("eunit/include/eunit.hrl").

-export([new/0, size/1, to_list/1, from_list/1, fetch/2, map_values/2, keys/1, values/1, filter/2]).

new() ->
    maps:new().

size(A) ->
    maps:size(A).

to_list(A) ->
    maps:to_list(A).

from_list(A) ->
    maps:from_list(A).

-ifdef(TEST).
from_list_test() ->
    Proplist = [{4, 0}, {1, 0}],
    Map = from_list(Proplist),
    _ = (fun(Capture1) -> expect:equal(Capture1, 2) end)(size(Map)),
    (fun(Capture1) -> expect:equal(Capture1, Proplist) end)(to_list(Map)).
-endif.

fetch(A, B) ->
    gleam__stdlib:map_fetch(A, B).

-ifdef(TEST).
fetch_test() ->
    Proplist = [{4, 0}, {1, 1}],
    Map = from_list(Proplist),
    _ = (fun(Capture1) ->
        expect:equal(Capture1, {ok, 0})
    end)((fun(Capture1) -> fetch(Capture1, 4) end)(Map)),
    (fun(Capture1) ->
        expect:equal(Capture1, {ok, 1})
    end)((fun(Capture1) -> fetch(Capture1, 1) end)(Map)).
-endif.

erl_map_values(A, B) ->
    maps:map(A, B).

map_values(Map, Fun) ->
    erl_map_values(Fun, Map).

-ifdef(TEST).
map_values_test() ->
    (fun(Capture1) ->
        expect:equal(Capture1, from_list([{1, 0}, {2, 3}, {3, 5}]))
    end)((fun(Capture1) ->
             map_values(Capture1, fun(K, V) -> K + V end)
         end)(from_list([{1, 0}, {2, 1}, {3, 2}]))).
-endif.

keys(A) ->
    maps:keys(A).

-ifdef(TEST).
keys_test() ->
    (fun(Capture1) ->
        expect:equal(Capture1, [<<"a">>, <<"b">>, <<"c">>])
    end)(keys(from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}]))).
-endif.

values(A) ->
    maps:values(A).

-ifdef(TEST).
values_test() ->
    (fun(Capture1) ->
        expect:equal(Capture1, [0, 1, 2])
    end)(values(from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}]))).
-endif.

erl_filter(A, B) ->
    maps:filter(A, B).

filter(Map, Fun) ->
    filter(Fun, Map).
