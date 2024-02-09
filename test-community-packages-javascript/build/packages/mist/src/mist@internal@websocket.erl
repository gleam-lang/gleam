-module(mist@internal@websocket).
-compile([no_auto_import, nowarn_unused_vars]).

-export([frame_to_bit_builder/1, to_text_frame/1, to_binary_frame/1, crypto_hash/2, base64_encode/1, parse_key/1, frame_from_message/3]).
-export_type([message/0, frame/0, sha_hash/0, websocket_handler/0]).

-type message() :: {binary_message, bitstring()} | {text_message, binary()}.

-type frame() :: {close_frame, integer(), bitstring()} |
    {text_frame, integer(), bitstring()} |
    {binary_frame, integer(), bitstring()} |
    {ping_frame, integer(), bitstring()} |
    {pong_frame, integer(), bitstring()}.

-type sha_hash() :: sha.

-type websocket_handler() :: {websocket_handler,
        gleam@option:option(fun((gleam@erlang@process:subject(glisten@handler:handler_message())) -> nil)),
        gleam@option:option(fun((gleam@erlang@process:subject(glisten@handler:handler_message())) -> nil)),
        fun((message(), gleam@erlang@process:subject(glisten@handler:handler_message())) -> {ok,
                nil} |
            {error, nil})}.

-spec make_frame(integer(), integer(), bitstring()) -> gleam@bit_builder:bit_builder().
make_frame(Opcode, Length, Payload) ->
    Length_section = case Length of
        Length@1 when Length@1 > 65535 ->
            <<127:7, Length@1:64/integer>>;

        Length@2 when Length@2 >= 126 ->
            <<126:7, Length@2:16/integer>>;

        _ ->
            <<Length:7>>
    end,
    _pipe = <<1:1,
        0:3,
        Opcode:4,
        0:1,
        Length_section/bitstring,
        Payload/bitstring>>,
    gleam@bit_builder:from_bit_string(_pipe).

-spec frame_to_bit_builder(frame()) -> gleam@bit_builder:bit_builder().
frame_to_bit_builder(Frame) ->
    case Frame of
        {text_frame, Payload_length, Payload} ->
            make_frame(1, Payload_length, Payload);

        {close_frame, Payload_length@1, Payload@1} ->
            make_frame(8, Payload_length@1, Payload@1);

        {binary_frame, Payload_length@2, Payload@2} ->
            make_frame(2, Payload_length@2, Payload@2);

        {pong_frame, Payload_length@3, Payload@3} ->
            make_frame(10, Payload_length@3, Payload@3);

        {ping_frame, _, _} ->
            gleam@bit_builder:from_bit_string(<<>>)
    end.

-spec to_text_frame(bitstring()) -> gleam@bit_builder:bit_builder().
to_text_frame(Data) ->
    Size = gleam@bit_string:byte_size(Data),
    frame_to_bit_builder({text_frame, Size, Data}).

-spec to_binary_frame(bitstring()) -> gleam@bit_builder:bit_builder().
to_binary_frame(Data) ->
    Size = gleam@bit_string:byte_size(Data),
    frame_to_bit_builder({binary_frame, Size, Data}).

-spec crypto_hash(sha_hash(), binary()) -> binary().
crypto_hash(Field@0, Field@1) ->
    crypto:hash(Field@0, Field@1).

-spec base64_encode(binary()) -> binary().
base64_encode(Field@0) ->
    base64:encode(Field@0).

-spec parse_key(binary()) -> binary().
parse_key(Key) ->
    _pipe = Key,
    _pipe@1 = gleam@string:append(
        _pipe,
        <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11"/utf8>>
    ),
    _pipe@2 = crypto:hash(sha, _pipe@1),
    base64:encode(_pipe@2).

-spec unmask_data(bitstring(), list(bitstring()), integer(), bitstring()) -> bitstring().
unmask_data(Data, Masks, Index, Resp) ->
    case Data of
        <<>> ->
            Resp;

        <<Masked:8/bitstring, Rest/bitstring>> ->
            _assert_subject = gleam@list:at(Masks, Index rem 4),
            {ok, Mask_value} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"mist/internal/websocket"/utf8>>,
                                function => <<"unmask_data"/utf8>>,
                                line => 43})
            end,
            Unmasked = crypto:exor(Mask_value, Masked),
            unmask_data(
                Rest,
                Masks,
                Index + 1,
                <<Resp/bitstring, Unmasked/bitstring>>
            )
    end.

-spec frame_from_message(
    glisten@socket:socket(),
    glisten@socket@transport:transport(),
    bitstring()
) -> {ok, frame()} | {error, nil}.
frame_from_message(Socket, Transport, Message) ->
    <<_:1, Rest/bitstring>> = case Message of
        <<_:1, _/bitstring>> -> Message;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"mist/internal/websocket"/utf8>>,
                        function => <<"frame_from_message"/utf8>>,
                        line => 60})
    end,
    <<_:3, Rest@1/bitstring>> = case Rest of
        <<_:3, _/bitstring>> -> Rest;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"mist/internal/websocket"/utf8>>,
                        function => <<"frame_from_message"/utf8>>,
                        line => 61})
    end,
    <<Opcode:4/integer, Rest@2/bitstring>> = case Rest@1 of
        <<_:4/integer, _/bitstring>> -> Rest@1;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"mist/internal/websocket"/utf8>>,
                        function => <<"frame_from_message"/utf8>>,
                        line => 62})
    end,
    case Opcode of
        1 ->
            <<1:1, Rest@3/bitstring>> = case Rest@2 of
                <<1:1, _/bitstring>> -> Rest@2;
                _assert_fail@3 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@3,
                                module => <<"mist/internal/websocket"/utf8>>,
                                function => <<"frame_from_message"/utf8>>,
                                line => 66})
            end,
            <<Payload_length:7/integer, Rest@4/bitstring>> = case Rest@3 of
                <<_:7/integer, _/bitstring>> -> Rest@3;
                _assert_fail@4 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@4,
                                module => <<"mist/internal/websocket"/utf8>>,
                                function => <<"frame_from_message"/utf8>>,
                                line => 67})
            end,
            {Payload_length@1, Rest@7} = case Payload_length of
                126 ->
                    <<Length:16/integer, Rest@5/bitstring>> = case Rest@4 of
                        <<_:16/integer, _/bitstring>> -> Rest@4;
                        _assert_fail@5 ->
                            erlang:error(#{gleam_error => assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail@5,
                                        module => <<"mist/internal/websocket"/utf8>>,
                                        function => <<"frame_from_message"/utf8>>,
                                        line => 70})
                    end,
                    {Length, Rest@5};

                127 ->
                    <<Length@1:64/integer, Rest@6/bitstring>> = case Rest@4 of
                        <<_:64/integer, _/bitstring>> -> Rest@4;
                        _assert_fail@6 ->
                            erlang:error(#{gleam_error => assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail@6,
                                        module => <<"mist/internal/websocket"/utf8>>,
                                        function => <<"frame_from_message"/utf8>>,
                                        line => 74})
                    end,
                    {Length@1, Rest@6};

                _ ->
                    {Payload_length, Rest@4}
            end,
            <<Mask1:8/bitstring,
                Mask2:8/bitstring,
                Mask3:8/bitstring,
                Mask4:8/bitstring,
                Rest@8/bitstring>> = case Rest@7 of
                <<_:8/bitstring,
                    _:8/bitstring,
                    _:8/bitstring,
                    _:8/bitstring,
                    _/bitstring>> -> Rest@7;
                _assert_fail@7 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@7,
                                module => <<"mist/internal/websocket"/utf8>>,
                                function => <<"frame_from_message"/utf8>>,
                                line => 79})
            end,
            Data = case Payload_length@1 - gleam@bit_string:byte_size(Rest@8) of
                0 ->
                    unmask_data(Rest@8, [Mask1, Mask2, Mask3, Mask4], 0, <<>>);

                Need ->
                    _assert_subject = (erlang:element(9, Transport))(
                        Socket,
                        Need
                    ),
                    {ok, Needed} = case _assert_subject of
                        {ok, _} -> _assert_subject;
                        _assert_fail@8 ->
                            erlang:error(#{gleam_error => assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail@8,
                                        module => <<"mist/internal/websocket"/utf8>>,
                                        function => <<"frame_from_message"/utf8>>,
                                        line => 89})
                    end,
                    _pipe = Rest@8,
                    _pipe@1 = gleam@bit_string:append(_pipe, Needed),
                    unmask_data(_pipe@1, [Mask1, Mask2, Mask3, Mask4], 0, <<>>)
            end,
            _pipe@2 = case Opcode of
                1 ->
                    {text_frame, Payload_length@1, Data};

                2 ->
                    {binary_frame, Payload_length@1, Data}
            end,
            {ok, _pipe@2};

        2 ->
            <<1:1, Rest@3/bitstring>> = case Rest@2 of
                <<1:1, _/bitstring>> -> Rest@2;
                _assert_fail@3 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@3,
                                module => <<"mist/internal/websocket"/utf8>>,
                                function => <<"frame_from_message"/utf8>>,
                                line => 66})
            end,
            <<Payload_length:7/integer, Rest@4/bitstring>> = case Rest@3 of
                <<_:7/integer, _/bitstring>> -> Rest@3;
                _assert_fail@4 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@4,
                                module => <<"mist/internal/websocket"/utf8>>,
                                function => <<"frame_from_message"/utf8>>,
                                line => 67})
            end,
            {Payload_length@1, Rest@7} = case Payload_length of
                126 ->
                    <<Length:16/integer, Rest@5/bitstring>> = case Rest@4 of
                        <<_:16/integer, _/bitstring>> -> Rest@4;
                        _assert_fail@5 ->
                            erlang:error(#{gleam_error => assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail@5,
                                        module => <<"mist/internal/websocket"/utf8>>,
                                        function => <<"frame_from_message"/utf8>>,
                                        line => 70})
                    end,
                    {Length, Rest@5};

                127 ->
                    <<Length@1:64/integer, Rest@6/bitstring>> = case Rest@4 of
                        <<_:64/integer, _/bitstring>> -> Rest@4;
                        _assert_fail@6 ->
                            erlang:error(#{gleam_error => assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail@6,
                                        module => <<"mist/internal/websocket"/utf8>>,
                                        function => <<"frame_from_message"/utf8>>,
                                        line => 74})
                    end,
                    {Length@1, Rest@6};

                _ ->
                    {Payload_length, Rest@4}
            end,
            <<Mask1:8/bitstring,
                Mask2:8/bitstring,
                Mask3:8/bitstring,
                Mask4:8/bitstring,
                Rest@8/bitstring>> = case Rest@7 of
                <<_:8/bitstring,
                    _:8/bitstring,
                    _:8/bitstring,
                    _:8/bitstring,
                    _/bitstring>> -> Rest@7;
                _assert_fail@7 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@7,
                                module => <<"mist/internal/websocket"/utf8>>,
                                function => <<"frame_from_message"/utf8>>,
                                line => 79})
            end,
            Data = case Payload_length@1 - gleam@bit_string:byte_size(Rest@8) of
                0 ->
                    unmask_data(Rest@8, [Mask1, Mask2, Mask3, Mask4], 0, <<>>);

                Need ->
                    _assert_subject = (erlang:element(9, Transport))(
                        Socket,
                        Need
                    ),
                    {ok, Needed} = case _assert_subject of
                        {ok, _} -> _assert_subject;
                        _assert_fail@8 ->
                            erlang:error(#{gleam_error => assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail@8,
                                        module => <<"mist/internal/websocket"/utf8>>,
                                        function => <<"frame_from_message"/utf8>>,
                                        line => 89})
                    end,
                    _pipe = Rest@8,
                    _pipe@1 = gleam@bit_string:append(_pipe, Needed),
                    unmask_data(_pipe@1, [Mask1, Mask2, Mask3, Mask4], 0, <<>>)
            end,
            _pipe@2 = case Opcode of
                1 ->
                    {text_frame, Payload_length@1, Data};

                2 ->
                    {binary_frame, Payload_length@1, Data}
            end,
            {ok, _pipe@2};

        8 ->
            {ok, {close_frame, 0, <<>>}}
    end.
