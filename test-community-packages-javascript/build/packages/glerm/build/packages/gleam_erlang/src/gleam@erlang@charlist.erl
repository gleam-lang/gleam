-module(gleam@erlang@charlist).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([to_string/1, from_string/1]).
-export_type([charlist/0]).

-type charlist() :: any().

-spec to_string(charlist()) -> binary().
to_string(A) ->
    unicode:characters_to_binary(A).

-spec from_string(binary()) -> charlist().
from_string(A) ->
    unicode:characters_to_list(A).
