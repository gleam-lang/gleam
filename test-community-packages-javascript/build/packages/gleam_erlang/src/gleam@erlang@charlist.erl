-module(gleam@erlang@charlist).
-compile([no_auto_import, nowarn_unused_vars]).

-export([to_string/1, from_string/1]).
-export_type([charlist/0]).

-type charlist() :: any().

-spec to_string(charlist()) -> binary().
to_string(Field@0) ->
    unicode:characters_to_binary(Field@0).

-spec from_string(binary()) -> charlist().
from_string(Field@0) ->
    unicode:characters_to_list(Field@0).
