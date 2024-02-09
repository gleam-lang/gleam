-module(gleam@bit_array).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([from_string/1, byte_size/1, slice/3, is_utf8/1, to_string/1, concat/1, append/2, base64_encode/2, base64_decode/1, base64_url_encode/2, base64_url_decode/1, base16_encode/1, base16_decode/1]).

-spec from_string(binary()) -> bitstring().
from_string(X) ->
    gleam_stdlib:identity(X).

-spec byte_size(bitstring()) -> integer().
byte_size(X) ->
    erlang:byte_size(X).

-spec slice(bitstring(), integer(), integer()) -> {ok, bitstring()} |
    {error, nil}.
slice(String, Position, Length) ->
    gleam_stdlib:bit_array_slice(String, Position, Length).

-spec do_is_utf8(bitstring()) -> boolean().
do_is_utf8(Bits) ->
    case Bits of
        <<>> ->
            true;

        <<_/utf8, Rest/binary>> ->
            do_is_utf8(Rest);

        _ ->
            false
    end.

-spec is_utf8(bitstring()) -> boolean().
is_utf8(Bits) ->
    do_is_utf8(Bits).

-spec do_to_string(bitstring()) -> {ok, binary()} | {error, nil}.
do_to_string(Bits) ->
    case is_utf8(Bits) of
        true ->
            {ok, gleam_stdlib:identity(Bits)};

        false ->
            {error, nil}
    end.

-spec to_string(bitstring()) -> {ok, binary()} | {error, nil}.
to_string(Bits) ->
    do_to_string(Bits).

-spec concat(list(bitstring())) -> bitstring().
concat(Bit_arrays) ->
    gleam_stdlib:bit_array_concat(Bit_arrays).

-spec append(bitstring(), bitstring()) -> bitstring().
append(First, Second) ->
    gleam_stdlib:bit_array_concat([First, Second]).

-spec base64_encode(bitstring(), boolean()) -> binary().
base64_encode(Input, Padding) ->
    Encoded = base64:encode(Input),
    case Padding of
        true ->
            Encoded;

        false ->
            gleam@string:replace(Encoded, <<"="/utf8>>, <<""/utf8>>)
    end.

-spec base64_decode(binary()) -> {ok, bitstring()} | {error, nil}.
base64_decode(Encoded) ->
    Padded = case erlang:byte_size(gleam_stdlib:identity(Encoded)) rem 4 of
        0 ->
            Encoded;

        N ->
            gleam@string:append(
                Encoded,
                gleam@string:repeat(<<"="/utf8>>, 4 - N)
            )
    end,
    gleam_stdlib:base_decode64(Padded).

-spec base64_url_encode(bitstring(), boolean()) -> binary().
base64_url_encode(Input, Padding) ->
    _pipe = base64_encode(Input, Padding),
    _pipe@1 = gleam@string:replace(_pipe, <<"+"/utf8>>, <<"-"/utf8>>),
    gleam@string:replace(_pipe@1, <<"/"/utf8>>, <<"_"/utf8>>).

-spec base64_url_decode(binary()) -> {ok, bitstring()} | {error, nil}.
base64_url_decode(Encoded) ->
    _pipe = Encoded,
    _pipe@1 = gleam@string:replace(_pipe, <<"-"/utf8>>, <<"+"/utf8>>),
    _pipe@2 = gleam@string:replace(_pipe@1, <<"_"/utf8>>, <<"/"/utf8>>),
    base64_decode(_pipe@2).

-spec base16_encode(bitstring()) -> binary().
base16_encode(Input) ->
    binary:encode_hex(Input).

-spec base16_decode(binary()) -> {ok, bitstring()} | {error, nil}.
base16_decode(Input) ->
    gleam_stdlib:base16_decode(Input).
