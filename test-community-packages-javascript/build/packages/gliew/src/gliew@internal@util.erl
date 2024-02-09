-module(gliew@internal@util).
-compile([no_auto_import, nowarn_unused_vars]).

-export([random_hex_string/1]).

-spec to_hex_string(bitstring()) -> binary().
to_hex_string(Bstr) ->
    case Bstr of
        <<>> ->
            <<""/utf8>>;

        <<A:8, Rest/bitstring>> ->
            <<(begin
                    _pipe = gleam@int:to_base16(A),
                    gleam@string:lowercase(_pipe)
                end)/binary,
                (to_hex_string(Rest))/binary>>
    end.

-spec random_hex_string(integer()) -> binary().
random_hex_string(Len) ->
    _pipe = crypto:strong_rand_bytes(Len),
    to_hex_string(_pipe).
