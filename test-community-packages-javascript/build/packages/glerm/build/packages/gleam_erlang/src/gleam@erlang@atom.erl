-module(gleam@erlang@atom).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([from_string/1, create_from_string/1, to_string/1, from_dynamic/1]).
-export_type([atom_/0, from_string_error/0]).

-type atom_() :: any().

-type from_string_error() :: atom_not_loaded.

-spec from_string(binary()) -> {ok, atom_()} | {error, from_string_error()}.
from_string(A) ->
    gleam_erlang_ffi:atom_from_string(A).

-spec create_from_string(binary()) -> atom_().
create_from_string(A) ->
    erlang:binary_to_atom(A).

-spec to_string(atom_()) -> binary().
to_string(A) ->
    erlang:atom_to_binary(A).

-spec from_dynamic(gleam@dynamic:dynamic_()) -> {ok, atom_()} |
    {error, list(gleam@dynamic:decode_error())}.
from_dynamic(From) ->
    gleam_erlang_ffi:atom_from_dynamic(From).
