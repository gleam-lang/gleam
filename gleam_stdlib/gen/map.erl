-module(map).
-compile(no_auto_import).
-include_lib("eunit/include/eunit.hrl").

-export([size/1, to_list/1, from_list/1, has_key/2, new/0, fetch/2, put/3, map_values/2, keys/1, values/1, filter/2]).

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
    (fun(Capture1) -> expect:equal(Capture1, 2) end)(size(Map)).
-endif.

is_key(A, B) ->
    maps:is_key(A, B).

has_key(Map, Key) ->
    is_key(Key, Map).

-ifdef(TEST).
has_key_test() ->
    expect:false((fun(Capture1) -> has_key(Capture1, 1) end)(from_list([]))),
    expect:true((fun(Capture1) ->
                    has_key(Capture1, 1)
                end)(from_list([{1, 0}]))),
    expect:true((fun(Capture1) ->
                    has_key(Capture1, 1)
                end)(from_list([{4, 0}, {1, 0}]))),
    expect:false((fun(Capture1) ->
                     has_key(Capture1, 0)
                 end)(from_list([{4, 0}, {1, 0}]))).
-endif.

new() ->
    maps:new().

-ifdef(TEST).
new_test() ->
    (fun(Capture1) -> expect:equal(Capture1, 0) end)(size(new())),
    (fun(Capture1) -> expect:equal(Capture1, []) end)(to_list(new())).
-endif.

fetch(A, B) ->
    gleam__stdlib:map_fetch(A, B).

-ifdef(TEST).
fetch_test() ->
    Proplist = [{4, 0}, {1, 1}],
    Map = from_list(Proplist),
    (fun(Capture1) ->
        expect:equal(Capture1, {ok, 0})
    end)((fun(Capture1) -> fetch(Capture1, 4) end)(Map)),
    (fun(Capture1) ->
        expect:equal(Capture1, {ok, 1})
    end)((fun(Capture1) -> fetch(Capture1, 1) end)(Map)).
-endif.

erl_put(A, B, C) ->
    maps:put(A, B, C).

put(Map, Key, Value) ->
    erl_put(Key, Value, Map).

-ifdef(TEST).
put_test() ->
    (fun(Capture1) ->
        expect:equal(Capture1,
                     from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}]))
    end)((fun(Capture1) ->
             put(Capture1, <<"c">>, 2)
         end)((fun(Capture1) ->
                  put(Capture1, <<"b">>, 1)
              end)((fun(Capture1) -> put(Capture1, <<"a">>, 0) end)(new())))).
-endif.

erl_map_values(A, B) ->
    maps:map(A, B).

map_values(Map, Fun) ->
    erl_map_values(Fun, Map).

-ifdef(TEST).
map_values_test() ->
    (fun(Capture1) ->
        expect:equal(Capture1, from_list([{1, 1}, {2, 3}, {3, 5}]))
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
    erl_filter(Fun, Map).
