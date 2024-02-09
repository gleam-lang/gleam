-module(gleam@erlang@atom).
-compile([no_auto_import, nowarn_unused_vars]).

-export([from_string/1, create_from_string/1, to_string/1, from_dynamic/1]).
-export_type([from_string_error/0, atom_/0]).

-type from_string_error() :: atom_not_loaded.

-type atom_() :: any().

-spec from_string(binary()) -> {ok, atom_()} | {error, from_string_error()}.
from_string(Field@0) ->
    gleam_erlang_ffi:atom_from_string(Field@0).

-spec create_from_string(binary()) -> atom_().
create_from_string(Field@0) ->
    erlang:binary_to_atom(Field@0).

-spec to_string(atom_()) -> binary().
to_string(Field@0) ->
    erlang:atom_to_binary(Field@0).

-spec from_dynamic(gleam@dynamic:dynamic()) -> {ok, atom_()} |
    {error, list(gleam@dynamic:decode_error())}.
from_dynamic(Field@0) ->
    gleam_erlang_ffi:atom_from_dynamic(Field@0).
