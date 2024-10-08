-module(gleam@bytes_builder).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([append_builder/2, prepend_builder/2, concat/1, new/0, from_string/1, prepend_string/2, append_string/2, from_string_builder/1, from_bit_array/1, prepend/2, append/2, concat_bit_arrays/1, to_bit_array/1, byte_size/1]).
-export_type([bytes_builder/0]).

-opaque bytes_builder() :: {bytes, bitstring()} |
    {text, gleam@string_builder:string_builder()} |
    {many, list(bytes_builder())}.

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_builder.gleam", 72).
-spec append_builder(bytes_builder(), bytes_builder()) -> bytes_builder().
append_builder(First, Second) ->
    gleam_stdlib:iodata_append(First, Second).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_builder.gleam", 60).
-spec prepend_builder(bytes_builder(), bytes_builder()) -> bytes_builder().
prepend_builder(Second, First) ->
    gleam_stdlib:iodata_append(First, Second).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_builder.gleam", 111).
-spec concat(list(bytes_builder())) -> bytes_builder().
concat(Builders) ->
    gleam_stdlib:identity(Builders).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_builder.gleam", 36).
-spec new() -> bytes_builder().
new() ->
    gleam_stdlib:identity([]).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_builder.gleam", 132).
-spec from_string(binary()) -> bytes_builder().
from_string(String) ->
    gleam_stdlib:wrap_list(String).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_builder.gleam", 87).
-spec prepend_string(bytes_builder(), binary()) -> bytes_builder().
prepend_string(Second, First) ->
    gleam_stdlib:iodata_append(gleam_stdlib:wrap_list(First), Second).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_builder.gleam", 99).
-spec append_string(bytes_builder(), binary()) -> bytes_builder().
append_string(First, Second) ->
    gleam_stdlib:iodata_append(First, gleam_stdlib:wrap_list(Second)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_builder.gleam", 142).
-spec from_string_builder(gleam@string_builder:string_builder()) -> bytes_builder().
from_string_builder(Builder) ->
    gleam_stdlib:wrap_list(Builder).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_builder.gleam", 151).
-spec from_bit_array(bitstring()) -> bytes_builder().
from_bit_array(Bits) ->
    gleam_stdlib:wrap_list(Bits).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_builder.gleam", 44).
-spec prepend(bytes_builder(), bitstring()) -> bytes_builder().
prepend(Second, First) ->
    gleam_stdlib:iodata_append(gleam_stdlib:wrap_list(First), Second).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_builder.gleam", 52).
-spec append(bytes_builder(), bitstring()) -> bytes_builder().
append(First, Second) ->
    gleam_stdlib:iodata_append(First, gleam_stdlib:wrap_list(Second)).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_builder.gleam", 120).
-spec concat_bit_arrays(list(bitstring())) -> bytes_builder().
concat_bit_arrays(Bits) ->
    gleam_stdlib:identity(Bits).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_builder.gleam", 170).
-spec to_list(list(list(bytes_builder())), list(bitstring())) -> list(bitstring()).
to_list(Stack, Acc) ->
    case Stack of
        [] ->
            Acc;

        [[] | Remaining_stack] ->
            to_list(Remaining_stack, Acc);

        [[{bytes, Bits} | Rest] | Remaining_stack@1] ->
            to_list([Rest | Remaining_stack@1], [Bits | Acc]);

        [[{text, Builder} | Rest@1] | Remaining_stack@2] ->
            Bits@1 = gleam_stdlib:identity(
                gleam@string_builder:to_string(Builder)
            ),
            to_list([Rest@1 | Remaining_stack@2], [Bits@1 | Acc]);

        [[{many, Builders} | Rest@2] | Remaining_stack@3] ->
            to_list([Builders, Rest@2 | Remaining_stack@3], Acc)
    end.

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_builder.gleam", 163).
-spec to_bit_array(bytes_builder()) -> bitstring().
to_bit_array(Builder) ->
    erlang:list_to_bitstring(Builder).

-file("/Users/louis/src/gleam/stdlib/src/gleam/bytes_builder.gleam", 197).
-spec byte_size(bytes_builder()) -> integer().
byte_size(Builder) ->
    erlang:iolist_size(Builder).
