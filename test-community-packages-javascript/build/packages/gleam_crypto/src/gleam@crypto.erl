-module(gleam@crypto).
-compile([no_auto_import, nowarn_unused_vars]).

-export([strong_random_bytes/1, hash/2, hmac/3, sign_message/3, secure_compare/2, verify_signed_message/2]).
-export_type([hash_algorithm/0, hmac/0]).

-type hash_algorithm() :: sha224 | sha256 | sha384 | sha512.

-type hmac() :: hmac.

-spec signing_input(hash_algorithm(), bitstring()) -> binary().
signing_input(Digest_type, Message) ->
    Protected = case Digest_type of
        sha224 ->
            <<"HS224"/utf8>>;

        sha256 ->
            <<"HS256"/utf8>>;

        sha384 ->
            <<"HS384"/utf8>>;

        sha512 ->
            <<"HS512"/utf8>>
    end,
    gleam@string:concat(
        [gleam@base:url_encode64(<<Protected/binary>>, false),
            <<"."/utf8>>,
            gleam@base:url_encode64(Message, false)]
    ).

-spec strong_random_bytes(integer()) -> bitstring().
strong_random_bytes(Field@0) ->
    crypto:strong_rand_bytes(Field@0).

-spec hash(hash_algorithm(), bitstring()) -> bitstring().
hash(Field@0, Field@1) ->
    crypto:hash(Field@0, Field@1).

-spec hmac(bitstring(), hash_algorithm(), bitstring()) -> bitstring().
hmac(Data, Algorithm, Key) ->
    crypto:mac(hmac, Algorithm, Key, Data).

-spec sign_message(bitstring(), bitstring(), hash_algorithm()) -> binary().
sign_message(Message, Secret, Digest_type) ->
    Input = signing_input(Digest_type, Message),
    Signature = hmac(<<Input/binary>>, Digest_type, Secret),
    gleam@string:concat(
        [Input, <<"."/utf8>>, gleam@base:url_encode64(Signature, false)]
    ).

-spec do_secure_compare(bitstring(), bitstring(), integer()) -> boolean().
do_secure_compare(Left, Right, Accumulator) ->
    case {Left, Right} of
        {<<X, Left@1/bitstring>>, <<Y, Right@1/bitstring>>} ->
            Accumulator@1 = gleam@bitwise:'or'(
                Accumulator,
                gleam@bitwise:exclusive_or(X, Y)
            ),
            do_secure_compare(Left@1, Right@1, Accumulator@1);

        {<<>>, <<>>} ->
            Accumulator =:= 0
    end.

-spec secure_compare(bitstring(), bitstring()) -> boolean().
secure_compare(Left, Right) ->
    case gleam@bit_string:byte_size(Left) =:= gleam@bit_string:byte_size(Right) of
        true ->
            do_secure_compare(Left, Right, 0);

        false ->
            false
    end.

-spec verify_signed_message(binary(), bitstring()) -> {ok, bitstring()} |
    {error, nil}.
verify_signed_message(Message, Secret) ->
    gleam@result:then(case gleam@string:split(Message, <<"."/utf8>>) of
            [A, B, C] ->
                {ok, {A, B, C}};

            _ ->
                {error, nil}
        end, fun(_use0) ->
            {Protected, Payload, Signature} = _use0,
            Text = gleam@string:concat([Protected, <<"."/utf8>>, Payload]),
            gleam@result:then(
                gleam@base:url_decode64(Payload),
                fun(Payload@1) ->
                    gleam@result:then(
                        gleam@base:url_decode64(Signature),
                        fun(Signature@1) ->
                            gleam@result:then(
                                gleam@base:url_decode64(Protected),
                                fun(Protected@1) ->
                                    gleam@result:then(case Protected@1 of
                                            <<"HS224"/utf8>> ->
                                                {ok, sha224};

                                            <<"HS256"/utf8>> ->
                                                {ok, sha256};

                                            <<"HS384"/utf8>> ->
                                                {ok, sha384};

                                            <<"HS512"/utf8>> ->
                                                {ok, sha512};

                                            _ ->
                                                {error, nil}
                                        end, fun(Digest_type) ->
                                            Challenge = hmac(
                                                <<Text/binary>>,
                                                Digest_type,
                                                Secret
                                            ),
                                            case secure_compare(
                                                Challenge,
                                                Signature@1
                                            ) of
                                                true ->
                                                    {ok, Payload@1};

                                                false ->
                                                    {error, nil}
                                            end
                                        end)
                                end
                            )
                        end
                    )
                end
            )
        end).
