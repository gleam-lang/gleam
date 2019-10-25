-module(gleam@map).
-compile(no_auto_import).

-export([size/1, to_list/1, from_list/1, has_key/2, new/0, get/2, insert/3, map_values/2, keys/1, values/1, filter/2, take/2, merge/2, delete/2, drop/2, update/3, fold/3]).

size(A) ->
    maps:size(A).

to_list(A) ->
    maps:to_list(A).

from_list(A) ->
    maps:from_list(A).

is_key(A, B) ->
    maps:is_key(A, B).

has_key(Map, Key) ->
    is_key(Key, Map).

new() ->
    maps:new().

get(A, B) ->
    gleam_stdlib:map_get(A, B).

erl_insert(A, B, C) ->
    maps:put(A, B, C).

insert(Map, Key, Value) ->
    erl_insert(Key, Value, Map).

erl_map_values(A, B) ->
    maps:map(A, B).

map_values(Map, Fun) ->
    erl_map_values(Fun, Map).

keys(A) ->
    maps:keys(A).

values(A) ->
    maps:values(A).

erl_filter(A, B) ->
    maps:filter(A, B).

filter(Map, Predicate) ->
    erl_filter(Predicate, Map).

erl_take(A, B) ->
    maps:with(A, B).

take(Map, DesiredKeys) ->
    erl_take(DesiredKeys, Map).

merge(A, B) ->
    maps:merge(A, B).

erl_delete(A, B) ->
    maps:remove(A, B).

delete(Map, Key) ->
    erl_delete(Key, Map).

drop(Map, DisallowedKeys) ->
    gleam@list:fold(DisallowedKeys, Map, fun(Key, Acc) -> delete(Acc, Key) end).

update(Map, Key, Fun) ->
    case get(Map, Key) of
        {ok, Value} ->
            insert(Map, Key, Fun({ok, Value}));

        {error, _} ->
            insert(Map, Key, Fun({error, nil}))
    end.

do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Tail] ->
            do_fold(Tail, Fun(K, V, Initial), Fun)
    end.

fold(Map, Initial, Fun) ->
    Kvs = to_list(Map),
    do_fold(Kvs, Initial, Fun).
