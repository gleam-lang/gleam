-module(gleam@map).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([size/1, to_list/1, from_list/1, has_key/2, new/0, get/2, insert/3, map_values/2, keys/1, values/1, filter/2, take/2, merge/2, delete/2, drop/2, update/3, fold/3]).

-spec size(gleam@dict:dict(any(), any())) -> integer().
size(Map) ->
    gleam@dict:size(Map).

-spec to_list(gleam@dict:dict(FDE, FDF)) -> list({FDE, FDF}).
to_list(Map) ->
    gleam@dict:to_list(Map).

-spec from_list(list({FDH, FDI})) -> gleam@dict:dict(FDH, FDI).
from_list(List) ->
    gleam@dict:from_list(List).

-spec has_key(gleam@dict:dict(FDM, any()), FDM) -> boolean().
has_key(Map, Key) ->
    gleam@dict:has_key(Map, Key).

-spec new() -> gleam@dict:dict(any(), any()).
new() ->
    gleam@dict:new().

-spec get(gleam@dict:dict(FDP, FDQ), FDP) -> {ok, FDQ} | {error, nil}.
get(From, Get) ->
    gleam@dict:get(From, Get).

-spec insert(gleam@dict:dict(FDU, FDV), FDU, FDV) -> gleam@dict:dict(FDU, FDV).
insert(Map, Key, Value) ->
    gleam@dict:insert(Map, Key, Value).

-spec map_values(gleam@dict:dict(FDY, FDZ), fun((FDY, FDZ) -> FEA)) -> gleam@dict:dict(FDY, FEA).
map_values(Map, Fun) ->
    gleam@dict:map_values(Map, Fun).

-spec keys(gleam@dict:dict(FED, any())) -> list(FED).
keys(Map) ->
    gleam@dict:keys(Map).

-spec values(gleam@dict:dict(any(), FEG)) -> list(FEG).
values(Map) ->
    gleam@dict:values(Map).

-spec filter(gleam@dict:dict(FEJ, FEK), fun((FEJ, FEK) -> boolean())) -> gleam@dict:dict(FEJ, FEK).
filter(Map, Predicate) ->
    gleam@dict:filter(Map, Predicate).

-spec take(gleam@dict:dict(FEN, FGH), list(FEN)) -> gleam@dict:dict(FEN, FGH).
take(Map, Desired_keys) ->
    gleam@dict:take(Map, Desired_keys).

-spec merge(gleam@dict:dict(FGI, FGJ), gleam@dict:dict(FGI, FGJ)) -> gleam@dict:dict(FGI, FGJ).
merge(Map, New_entries) ->
    gleam@dict:merge(Map, New_entries).

-spec delete(gleam@dict:dict(FEU, FGL), FEU) -> gleam@dict:dict(FEU, FGL).
delete(Map, Key) ->
    gleam@dict:delete(Map, Key).

-spec drop(gleam@dict:dict(FEX, FGN), list(FEX)) -> gleam@dict:dict(FEX, FGN).
drop(Map, Disallowed_keys) ->
    gleam@dict:drop(Map, Disallowed_keys).

-spec update(
    gleam@dict:dict(FFB, FFC),
    FFB,
    fun((gleam@option:option(FFC)) -> FFC)
) -> gleam@dict:dict(FFB, FFC).
update(Map, Key, Fun) ->
    gleam@dict:update(Map, Key, Fun).

-spec fold(gleam@dict:dict(FFH, FFI), FFG, fun((FFG, FFH, FFI) -> FFG)) -> FFG.
fold(Map, Initial, Fun) ->
    gleam@dict:fold(Map, Initial, Fun).
