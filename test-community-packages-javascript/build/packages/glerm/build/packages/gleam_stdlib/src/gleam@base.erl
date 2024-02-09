-module(gleam@base).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([encode64/2, decode64/1, url_encode64/2, url_decode64/1]).

-spec encode64(bitstring(), boolean()) -> binary().
encode64(Input, Padding) ->
    gleam@bit_array:base64_encode(Input, Padding).

-spec decode64(binary()) -> {ok, bitstring()} | {error, nil}.
decode64(Encoded) ->
    gleam@bit_array:base64_decode(Encoded).

-spec url_encode64(bitstring(), boolean()) -> binary().
url_encode64(Input, Padding) ->
    gleam@bit_array:base64_url_encode(Input, Padding).

-spec url_decode64(binary()) -> {ok, bitstring()} | {error, nil}.
url_decode64(Encoded) ->
    gleam@bit_array:base64_url_decode(Encoded).
