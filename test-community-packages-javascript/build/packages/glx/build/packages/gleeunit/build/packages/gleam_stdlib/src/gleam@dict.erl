-module(gleam@dict).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([size/1, to_list/1, from_list/1, has_key/2, new/0, get/2, insert/3, map_values/2, keys/1, values/1, filter/2, take/2, merge/2, delete/2, drop/2, update/3, fold/3]).
-export_type([dict/2]).

-type dict(NP, NQ) :: any() | {gleam_phantom, NP, NQ}.

-spec size(dict(any(), any())) -> integer().
size(Dict) ->
    maps:size(Dict).

-spec to_list(dict(NZ, OA)) -> list({NZ, OA}).
to_list(Dict) ->
    maps:to_list(Dict).

-spec from_list(list({OJ, OK})) -> dict(OJ, OK).
from_list(List) ->
    maps:from_list(List).

-spec has_key(dict(OT, any()), OT) -> boolean().
has_key(Dict, Key) ->
    maps:is_key(Key, Dict).

-spec new() -> dict(any(), any()).
new() ->
    maps:new().

-spec get(dict(PJ, PK), PJ) -> {ok, PK} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec insert(dict(PV, PW), PV, PW) -> dict(PV, PW).
insert(Dict, Key, Value) ->
    maps:put(Key, Value, Dict).

-spec map_values(dict(QH, QI), fun((QH, QI) -> QL)) -> dict(QH, QL).
map_values(Dict, Fun) ->
    maps:map(Fun, Dict).

-spec keys(dict(QV, any())) -> list(QV).
keys(Dict) ->
    maps:keys(Dict).

-spec values(dict(any(), RG)) -> list(RG).
values(Dict) ->
    maps:values(Dict).

-spec filter(dict(RP, RQ), fun((RP, RQ) -> boolean())) -> dict(RP, RQ).
filter(Dict, Predicate) ->
    maps:filter(Predicate, Dict).

-spec take(dict(SB, SC), list(SB)) -> dict(SB, SC).
take(Dict, Desired_keys) ->
    maps:with(Desired_keys, Dict).

-spec merge(dict(SP, SQ), dict(SP, SQ)) -> dict(SP, SQ).
merge(Dict, New_entries) ->
    maps:merge(Dict, New_entries).

-spec delete(dict(TF, TG), TF) -> dict(TF, TG).
delete(Dict, Key) ->
    maps:remove(Key, Dict).

-spec drop(dict(TR, TS), list(TR)) -> dict(TR, TS).
drop(Dict, Disallowed_keys) ->
    case Disallowed_keys of
        [] ->
            Dict;

        [X | Xs] ->
            drop(delete(Dict, X), Xs)
    end.

-spec update(dict(TY, TZ), TY, fun((gleam@option:option(TZ)) -> TZ)) -> dict(TY, TZ).
update(Dict, Key, Fun) ->
    _pipe = Dict,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Dict, Key, _pipe@3).

-spec do_fold(list({UF, UG}), UI, fun((UI, UF, UG) -> UI)) -> UI.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Rest] ->
            do_fold(Rest, Fun(Initial, K, V), Fun)
    end.

-spec fold(dict(UJ, UK), UN, fun((UN, UJ, UK) -> UN)) -> UN.
fold(Dict, Initial, Fun) ->
    _pipe = Dict,
    _pipe@1 = to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).
