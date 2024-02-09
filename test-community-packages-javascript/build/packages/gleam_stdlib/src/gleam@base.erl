-module(gleam@base).
-compile([no_auto_import, nowarn_unused_vars]).

-export([encode64/2, url_encode64/2, decode64/1, url_decode64/1]).

-spec encode64(bitstring(), boolean()) -> binary().
encode64(Input, Padding) ->
    Encoded = base64:encode(Input),
    case Padding of
        true ->
            Encoded;

        false ->
            gleam@string:replace(Encoded, <<"="/utf8>>, <<""/utf8>>)
    end.

-spec url_encode64(bitstring(), boolean()) -> binary().
url_encode64(Input, Padding) ->
    _pipe = encode64(Input, Padding),
    _pipe@1 = gleam@string:replace(_pipe, <<"+"/utf8>>, <<"-"/utf8>>),
    gleam@string:replace(_pipe@1, <<"/"/utf8>>, <<"_"/utf8>>).

-spec decode64(binary()) -> {ok, bitstring()} | {error, nil}.
decode64(Encoded) ->
    Padded = case gleam@bit_string:byte_size(
        gleam@bit_string:from_string(Encoded)
    )
    rem 4 of
        0 ->
            Encoded;

        N ->
            gleam@string:append(
                Encoded,
                gleam@string:repeat(<<"="/utf8>>, 4 - N)
            )
    end,
    gleam_stdlib:base_decode64(Padded).

-spec url_decode64(binary()) -> {ok, bitstring()} | {error, nil}.
url_decode64(Encoded) ->
    _pipe = Encoded,
    _pipe@1 = gleam@string:replace(_pipe, <<"-"/utf8>>, <<"+"/utf8>>),
    _pipe@2 = gleam@string:replace(_pipe@1, <<"_"/utf8>>, <<"/"/utf8>>),
    decode64(_pipe@2).
