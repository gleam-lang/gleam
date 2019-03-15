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
    expect:equal(size(Map), 2).
-endif.

is_key(A, B) ->
    maps:is_key(A, B).

has_key(Map, Key) ->
    is_key(Key, Map).

-ifdef(TEST).
has_key_test() ->
    expect:false(has_key(from_list([]), 1)),
    expect:true(has_key(from_list([{1, 0}]), 1)),
    expect:true(has_key(from_list([{4, 0}, {1, 0}]), 1)),
    expect:false(has_key(from_list([{4, 0}, {1, 0}]), 0)).
-endif.

new() ->
    maps:new().

-ifdef(TEST).
new_test() ->
    expect:equal(size(new()), 0),
    expect:equal(to_list(new()), []).
-endif.

fetch(A, B) ->
    gleam__stdlib:map_fetch(A, B).

-ifdef(TEST).
fetch_test() ->
    Proplist = [{4, 0}, {1, 1}],
    Map = from_list(Proplist),
    expect:equal(fetch(Map, 4), {ok, 0}),
    expect:equal(fetch(Map, 1), {ok, 1}).
-endif.

erl_put(A, B, C) ->
    maps:put(A, B, C).

put(Map, Key, Value) ->
    erl_put(Key, Value, Map).

-ifdef(TEST).
put_test() ->
    expect:equal(put(put(put(new(), <<"a">>, 0), <<"b">>, 1), <<"c">>, 2),
                 from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}])).
-endif.

erl_map_values(A, B) ->
    maps:map(A, B).

map_values(Map, Fun) ->
    erl_map_values(Fun, Map).

-ifdef(TEST).
map_values_test() ->
    expect:equal(map_values(from_list([{1, 0}, {2, 1}, {3, 2}]),
                            fun(K, V) -> K + V end),
                 from_list([{1, 1}, {2, 3}, {3, 5}])).
-endif.

keys(A) ->
    maps:keys(A).

-ifdef(TEST).
keys_test() ->
    expect:equal(keys(from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}])),
                 [<<"a">>, <<"b">>, <<"c">>]).
-endif.

values(A) ->
    maps:values(A).

-ifdef(TEST).
values_test() ->
    expect:equal(values(from_list([{<<"a">>, 0}, {<<"b">>, 1}, {<<"c">>, 2}])),
                 [0, 1, 2]).
-endif.

erl_filter(A, B) ->
    maps:filter(A, B).

filter(Map, Fun) ->
    erl_filter(Fun, Map).
