-module(gleam@dict).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([size/1, to_list/1, new/0, is_empty/1, get/2, has_key/2, insert/3, from_list/1, keys/1, values/1, take/2, merge/2, delete/2, drop/2, upsert/3, fold/3, map_values/2, filter/2, each/2, combine/3]).
-export_type([dict/2]).

-type dict(KM, KN) :: any() | {gleam_phantom, KM, KN}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 36).
-spec size(dict(any(), any())) -> integer().
size(Dict) ->
    maps:size(Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 80).
-spec to_list(dict(KW, KX)) -> list({KW, KX}).
to_list(Dict) ->
    maps:to_list(Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 127).
-spec new() -> dict(any(), any()).
new() ->
    maps:new().

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 52).
-spec is_empty(dict(any(), any())) -> boolean().
is_empty(Dict) ->
    Dict =:= new().

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 152).
-spec get(dict(MD, ME), MD) -> {ok, ME} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 116).
-spec has_key(dict(LN, any()), LN) -> boolean().
has_key(Dict, Key) ->
    maps:is_key(Key, Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 177).
-spec insert(dict(MP, MQ), MP, MQ) -> dict(MP, MQ).
insert(Dict, Key, Value) ->
    maps:put(Key, Value, Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 92).
-spec fold_list_of_pair(list({LG, LH}), dict(LG, LH)) -> dict(LG, LH).
fold_list_of_pair(List, Initial) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold_list_of_pair(
                Rest,
                insert(Initial, erlang:element(1, X), erlang:element(2, X))
            )
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 88).
-spec from_list(list({LB, LC})) -> dict(LB, LC).
from_list(List) ->
    maps:from_list(List).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 229).
-spec reverse_and_concat(list(NZ), list(NZ)) -> list(NZ).
reverse_and_concat(Remaining, Accumulator) ->
    case Remaining of
        [] ->
            Accumulator;

        [Item | Rest] ->
            reverse_and_concat(Rest, [Item | Accumulator])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 236).
-spec do_keys_acc(list({OD, any()}), list(OD)) -> list(OD).
do_keys_acc(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [First | Rest] ->
            do_keys_acc(Rest, [erlang:element(1, First) | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 219).
-spec keys(dict(NP, any())) -> list(NP).
keys(Dict) ->
    maps:keys(Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 266).
-spec do_values_acc(list({any(), OT}), list(OT)) -> list(OT).
do_values_acc(List, Acc) ->
    case List of
        [] ->
            reverse_and_concat(Acc, []);

        [First | Rest] ->
            do_values_acc(Rest, [erlang:element(2, First) | Acc])
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 256).
-spec values(dict(any(), OJ)) -> list(OJ).
values(Dict) ->
    maps:values(Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 335).
-spec insert_taken(dict(PX, PY), list(PX), dict(PX, PY)) -> dict(PX, PY).
insert_taken(Dict, Desired_keys, Acc) ->
    Insert = fun(Taken, Key) -> case get(Dict, Key) of
            {ok, Value} ->
                insert(Taken, Key, Value);

            _ ->
                Taken
        end end,
    case Desired_keys of
        [] ->
            Acc;

        [First | Rest] ->
            insert_taken(Dict, Rest, Insert(Acc, First))
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 326).
-spec take(dict(PJ, PK), list(PJ)) -> dict(PJ, PK).
take(Dict, Desired_keys) ->
    maps:with(Desired_keys, Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 377).
-spec insert_pair(dict(QW, QX), {QW, QX}) -> dict(QW, QX).
insert_pair(Dict, Pair) ->
    insert(Dict, erlang:element(1, Pair), erlang:element(2, Pair)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 381).
-spec fold_inserts(list({RC, RD}), dict(RC, RD)) -> dict(RC, RD).
fold_inserts(New_entries, Dict) ->
    case New_entries of
        [] ->
            Dict;

        [First | Rest] ->
            fold_inserts(Rest, insert_pair(Dict, First))
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 366).
-spec merge(dict(QG, QH), dict(QG, QH)) -> dict(QG, QH).
merge(Dict, New_entries) ->
    maps:merge(Dict, New_entries).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 403).
-spec delete(dict(RJ, RK), RJ) -> dict(RJ, RK).
delete(Dict, Key) ->
    maps:remove(Key, Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 431).
-spec drop(dict(RV, RW), list(RV)) -> dict(RV, RW).
drop(Dict, Disallowed_keys) ->
    case Disallowed_keys of
        [] ->
            Dict;

        [First | Rest] ->
            drop(delete(Dict, First), Rest)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 461).
-spec upsert(dict(SC, SD), SC, fun((gleam@option:option(SD)) -> SD)) -> dict(SC, SD).
upsert(Dict, Key, Fun) ->
    _pipe = Dict,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Dict, Key, _pipe@3).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 473).
-spec do_fold(list({SJ, SK}), SM, fun((SM, SJ, SK) -> SM)) -> SM.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Rest] ->
            do_fold(Rest, Fun(Initial, K, V), Fun)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 505).
-spec fold(dict(SN, SO), SR, fun((SR, SN, SO) -> SR)) -> SR.
fold(Dict, Initial, Fun) ->
    _pipe = Dict,
    _pipe@1 = maps:to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 196).
-spec map_values(dict(NB, NC), fun((NB, NC) -> NF)) -> dict(NB, NF).
map_values(Dict, Fun) ->
    maps:map(Fun, Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 290).
-spec filter(dict(OX, OY), fun((OX, OY) -> boolean())) -> dict(OX, OY).
filter(Dict, Predicate) ->
    maps:filter(Predicate, Dict).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 537).
-spec each(dict(SS, ST), fun((SS, ST) -> any())) -> nil.
each(Dict, Fun) ->
    fold(
        Dict,
        nil,
        fun(Nil, K, V) ->
            Fun(K, V),
            Nil
        end
    ).

-file("/Users/louis/src/gleam/stdlib/src/gleam/dict.gleam", 558).
-spec combine(dict(SX, SY), dict(SX, SY), fun((SY, SY) -> SY)) -> dict(SX, SY).
combine(Dict, Other, Fun) ->
    fold(Dict, Other, fun(Acc, Key, Value) -> case get(Acc, Key) of
                {ok, Other_value} ->
                    insert(Acc, Key, Fun(Value, Other_value));

                {error, _} ->
                    insert(Acc, Key, Value)
            end end).
