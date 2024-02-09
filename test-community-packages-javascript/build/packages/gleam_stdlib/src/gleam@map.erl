-module(gleam@map).
-compile([no_auto_import, nowarn_unused_vars]).

-export([size/1, to_list/1, from_list/1, has_key/2, new/0, get/2, insert/3, update/3, map_values/2, keys/1, values/1, filter/2, take/2, merge/2, delete/2, drop/2, fold/3]).
-export_type([map_/2]).

-type map_(Key, Value) :: any() | {gleam_phantom, Key, Value}.

-spec size(map_(any(), any())) -> integer().
size(Map) ->
    maps:size(Map).

-spec to_list(map_(KF, KG)) -> list({KF, KG}).
to_list(Map) ->
    maps:to_list(Map).

-spec from_list(list({KK, KL})) -> map_(KK, KL).
from_list(List) ->
    maps:from_list(List).

-spec has_key(map_(KP, any()), KP) -> boolean().
has_key(Map, Key) ->
    maps:is_key(Key, Map).

-spec new() -> map_(any(), any()).
new() ->
    maps:new().

-spec get(map_(KX, KY), KX) -> {ok, KY} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec insert(map_(LD, LE), LD, LE) -> map_(LD, LE).
insert(Map, Key, Value) ->
    maps:put(Key, Value, Map).

-spec update(map_(NI, NJ), NI, fun((gleam@option:option(NJ)) -> NJ)) -> map_(NI, NJ).
update(Map, Key, Fun) ->
    _pipe = Map,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Map, Key, _pipe@3).

-spec map_values(map_(LJ, LK), fun((LJ, LK) -> LN)) -> map_(LJ, LN).
map_values(Map, Fun) ->
    maps:map(Fun, Map).

-spec keys(map_(LQ, any())) -> list(LQ).
keys(Map) ->
    maps:keys(Map).

-spec values(map_(any(), LW)) -> list(LW).
values(Map) ->
    maps:values(Map).

-spec filter(map_(MA, MB), fun((MA, MB) -> boolean())) -> map_(MA, MB).
filter(Map, Property) ->
    maps:filter(Property, Map).

-spec take(map_(MG, MH), list(MG)) -> map_(MG, MH).
take(Map, Desired_keys) ->
    maps:with(Desired_keys, Map).

-spec merge(map_(MN, MO), map_(MN, MO)) -> map_(MN, MO).
merge(Map, New_entries) ->
    maps:merge(Map, New_entries).

-spec delete(map_(MV, MW), MV) -> map_(MV, MW).
delete(Map, Key) ->
    maps:remove(Key, Map).

-spec drop(map_(NB, NC), list(NB)) -> map_(NB, NC).
drop(Map, Disallowed_keys) ->
    case Disallowed_keys of
        [] ->
            Map;

        [X | Xs] ->
            drop(delete(Map, X), Xs)
    end.

-spec do_fold(list({NP, NQ}), NS, fun((NS, NP, NQ) -> NS)) -> NS.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Rest] ->
            do_fold(Rest, Fun(Initial, K, V), Fun)
    end.

-spec fold(map_(NT, NU), NX, fun((NX, NT, NU) -> NX)) -> NX.
fold(Map, Initial, Fun) ->
    _pipe = Map,
    _pipe@1 = to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).
