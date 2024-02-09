-module(gleam@bit_builder).
-compile([no_auto_import, nowarn_unused_vars]).

-export([append_builder/2, prepend_builder/2, new/0, concat/1, concat_bit_strings/1, from_string/1, prepend_string/2, append_string/2, from_string_builder/1, from_bit_string/1, prepend/2, append/2, to_bit_string/1, byte_size/1]).
-export_type([bit_builder/0]).

-type bit_builder() :: any().

-spec append_builder(bit_builder(), bit_builder()) -> bit_builder().
append_builder(First, Second) ->
    gleam_stdlib:iodata_append(First, Second).

-spec prepend_builder(bit_builder(), bit_builder()) -> bit_builder().
prepend_builder(To, Prefix) ->
    append_builder(Prefix, To).

-spec new() -> bit_builder().
new() ->
    gleam_stdlib:identity([]).

-spec concat(list(bit_builder())) -> bit_builder().
concat(Builders) ->
    gleam_stdlib:identity(Builders).

-spec concat_bit_strings(list(bitstring())) -> bit_builder().
concat_bit_strings(Bits) ->
    gleam_stdlib:identity(Bits).

-spec from_string(binary()) -> bit_builder().
from_string(String) ->
    gleam_stdlib:wrap_list(String).

-spec prepend_string(bit_builder(), binary()) -> bit_builder().
prepend_string(To, Prefix) ->
    append_builder(from_string(Prefix), To).

-spec append_string(bit_builder(), binary()) -> bit_builder().
append_string(To, Suffix) ->
    append_builder(To, from_string(Suffix)).

-spec from_string_builder(gleam@string_builder:string_builder()) -> bit_builder().
from_string_builder(Builder) ->
    gleam_stdlib:wrap_list(Builder).

-spec from_bit_string(bitstring()) -> bit_builder().
from_bit_string(Bits) ->
    gleam_stdlib:wrap_list(Bits).

-spec prepend(bit_builder(), bitstring()) -> bit_builder().
prepend(To, Prefix) ->
    append_builder(from_bit_string(Prefix), To).

-spec append(bit_builder(), bitstring()) -> bit_builder().
append(To, Suffix) ->
    append_builder(To, from_bit_string(Suffix)).

-spec to_bit_string(bit_builder()) -> bitstring().
to_bit_string(Builder) ->
    erlang:list_to_bitstring(Builder).

-spec byte_size(bit_builder()) -> integer().
byte_size(Builder) ->
    erlang:iolist_size(Builder).
