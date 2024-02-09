-module(mist@internal@handler).
-compile([no_auto_import, nowarn_unused_vars]).

-export([new_state/0, with_func/1, with/2]).
-export_type([handler_response/0, handler_error/0, state/0]).

-type handler_response() :: {response,
        gleam@http@response:response(mist@internal@http:http_response_body())} |
    {upgrade, mist@internal@websocket:websocket_handler()}.

-type handler_error() :: {invalid_request, mist@internal@http:decode_error()} |
    not_found.

-type state() :: {state,
        gleam@option:option(gleam@erlang@process:timer()),
        gleam@option:option(mist@internal@websocket:websocket_handler())}.

-spec new_state() -> state().
new_state() ->
    {state, none, none}.

-spec handle_websocket_message(
    glisten@handler:loop_state(state()),
    mist@internal@websocket:websocket_handler(),
    bitstring()
) -> gleam@otp@actor:next(glisten@handler:loop_state(state())).
handle_websocket_message(State, Handler, Msg) ->
    case mist@internal@websocket:frame_from_message(
        erlang:element(2, State),
        erlang:element(4, State),
        Msg
    ) of
        {ok, {ping_frame, _, _}} ->
            _assert_subject = (erlang:element(11, erlang:element(4, State)))(
                erlang:element(2, State),
                mist@internal@websocket:frame_to_bit_builder(
                    {pong_frame, 0, <<>>}
                )
            ),
            {ok, _} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"mist/internal/handler"/utf8>>,
                                function => <<"handle_websocket_message"/utf8>>,
                                line => 119})
            end,
            {continue, State};

        {ok, {close_frame, _, _} = Frame} ->
            _assert_subject@1 = (erlang:element(11, erlang:element(4, State)))(
                erlang:element(2, State),
                mist@internal@websocket:frame_to_bit_builder(Frame)
            ),
            {ok, _} = case _assert_subject@1 of
                {ok, _} -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@1,
                                module => <<"mist/internal/handler"/utf8>>,
                                function => <<"handle_websocket_message"/utf8>>,
                                line => 127})
            end,
            _ = case erlang:element(2, Handler) of
                {some, Func} ->
                    Func(erlang:element(3, State));

                _ ->
                    nil
            end,
            {stop, normal};

        {ok, {pong_frame, _, _}} ->
            {stop, normal};

        {ok, Frame@1} ->
            _pipe = case Frame@1 of
                {text_frame, _, Payload} ->
                    _assert_subject@2 = gleam@bit_string:to_string(Payload),
                    {ok, Msg@1} = case _assert_subject@2 of
                        {ok, _} -> _assert_subject@2;
                        _assert_fail@2 ->
                            erlang:error(#{gleam_error => assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail@2,
                                        module => <<"mist/internal/handler"/utf8>>,
                                        function => <<"handle_websocket_message"/utf8>>,
                                        line => 142})
                    end,
                    {text_message, Msg@1};

                _ ->
                    {binary_message, erlang:element(3, Frame@1)}
            end,
            _pipe@1 = (fun(Ws_msg) ->
                gleam_erlang_ffi:rescue(
                    fun() ->
                        (erlang:element(4, Handler))(
                            Ws_msg,
                            erlang:element(3, State)
                        )
                    end
                )
            end)(_pipe),
            _pipe@2 = gleam@result:replace(_pipe@1, {continue, State}),
            _pipe@3 = gleam@result:map_error(
                _pipe@2,
                fun(Err) ->
                    mist@internal@logger:error(Err),
                    _ = case erlang:element(2, Handler) of
                        {some, Func@1} ->
                            Func@1(erlang:element(3, State));

                        _ ->
                            nil
                    end,
                    Err
                end
            ),
            _pipe@4 = gleam@result:replace_error(_pipe@3, {stop, normal}),
            gleam@result:unwrap_both(_pipe@4);

        {error, _} ->
            _ = case erlang:element(2, Handler) of
                {some, Func@2} ->
                    Func@2(erlang:element(3, State));

                _ ->
                    nil
            end,
            {stop, normal}
    end.

-spec log_and_error(
    gleam@erlang:crash(),
    glisten@socket:socket(),
    glisten@socket@transport:transport()
) -> gleam@otp@actor:next(glisten@handler:loop_state(state())).
log_and_error(Error, Socket, Transport) ->
    case Error of
        {exited, Msg} ->
            mist@internal@logger:error(Error),
            _pipe = gleam@http@response:new(500),
            _pipe@1 = gleam@http@response:set_body(
                _pipe,
                gleam@bit_builder:from_bit_string(
                    <<"Internal Server Error"/utf8>>
                )
            ),
            _pipe@2 = gleam@http@response:prepend_header(
                _pipe@1,
                <<"content-length"/utf8>>,
                <<"21"/utf8>>
            ),
            _pipe@3 = mist@internal@http:add_default_headers(_pipe@2),
            _pipe@4 = mist@internal@encoder:to_bit_builder(_pipe@3),
            (erlang:element(11, Transport))(Socket, _pipe@4),
            _ = (erlang:element(4, Transport))(Socket),
            {stop, {abnormal, gleam@dynamic:unsafe_coerce(Msg)}};

        {thrown, Msg} ->
            mist@internal@logger:error(Error),
            _pipe = gleam@http@response:new(500),
            _pipe@1 = gleam@http@response:set_body(
                _pipe,
                gleam@bit_builder:from_bit_string(
                    <<"Internal Server Error"/utf8>>
                )
            ),
            _pipe@2 = gleam@http@response:prepend_header(
                _pipe@1,
                <<"content-length"/utf8>>,
                <<"21"/utf8>>
            ),
            _pipe@3 = mist@internal@http:add_default_headers(_pipe@2),
            _pipe@4 = mist@internal@encoder:to_bit_builder(_pipe@3),
            (erlang:element(11, Transport))(Socket, _pipe@4),
            _ = (erlang:element(4, Transport))(Socket),
            {stop, {abnormal, gleam@dynamic:unsafe_coerce(Msg)}};

        {errored, Msg} ->
            mist@internal@logger:error(Error),
            _pipe = gleam@http@response:new(500),
            _pipe@1 = gleam@http@response:set_body(
                _pipe,
                gleam@bit_builder:from_bit_string(
                    <<"Internal Server Error"/utf8>>
                )
            ),
            _pipe@2 = gleam@http@response:prepend_header(
                _pipe@1,
                <<"content-length"/utf8>>,
                <<"21"/utf8>>
            ),
            _pipe@3 = mist@internal@http:add_default_headers(_pipe@2),
            _pipe@4 = mist@internal@encoder:to_bit_builder(_pipe@3),
            (erlang:element(11, Transport))(Socket, _pipe@4),
            _ = (erlang:element(4, Transport))(Socket),
            {stop, {abnormal, gleam@dynamic:unsafe_coerce(Msg)}}
    end.

-spec handle_bit_builder_body(
    gleam@http@response:response(mist@internal@http:http_response_body()),
    gleam@bit_builder:bit_builder(),
    glisten@handler:loop_state(state())
) -> gleam@otp@actor:next(glisten@handler:loop_state(state())).
handle_bit_builder_body(Resp, Body, State) ->
    _pipe = Resp,
    _pipe@1 = gleam@http@response:set_body(_pipe, Body),
    _pipe@2 = mist@internal@http:add_default_headers(_pipe@1),
    _pipe@3 = mist@internal@encoder:to_bit_builder(_pipe@2),
    _pipe@4 = (erlang:element(11, erlang:element(4, State)))(
        erlang:element(2, State),
        _pipe@3
    ),
    _pipe@5 = gleam@result:map(
        _pipe@4,
        fun(_) ->
            case gleam@http@response:get_header(Resp, <<"connection"/utf8>>) of
                {ok, <<"close"/utf8>>} ->
                    _ = (erlang:element(4, erlang:element(4, State)))(
                        erlang:element(2, State)
                    ),
                    {stop, normal};

                _ ->
                    Timer = gleam@erlang@process:send_after(
                        erlang:element(3, State),
                        10000,
                        close
                    ),
                    {continue,
                        erlang:setelement(
                            5,
                            State,
                            erlang:setelement(
                                2,
                                erlang:element(5, State),
                                {some, Timer}
                            )
                        )}
            end
        end
    ),
    _pipe@6 = gleam@result:replace_error(_pipe@5, {stop, normal}),
    gleam@result:unwrap_both(_pipe@6).

-spec handle_file_body(
    gleam@http@response:response(mist@internal@http:http_response_body()),
    glisten@handler:loop_state(state())
) -> gleam@otp@actor:next(glisten@handler:loop_state(state())).
handle_file_body(Resp, State) ->
    _assert_subject = erlang:element(4, Resp),
    {file_body, File_descriptor, Content_type, Offset, Length} = case _assert_subject of
        {file_body, _, _, _, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"mist/internal/handler"/utf8>>,
                        function => <<"handle_file_body"/utf8>>,
                        line => 268})
    end,
    _pipe = Resp,
    _pipe@1 = gleam@http@response:prepend_header(
        _pipe,
        <<"content-length"/utf8>>,
        gleam@int:to_string(Length - Offset)
    ),
    _pipe@2 = gleam@http@response:prepend_header(
        _pipe@1,
        <<"content-type"/utf8>>,
        Content_type
    ),
    _pipe@3 = gleam@http@response:set_body(_pipe@2, gleam@bit_builder:new()),
    _pipe@4 = (fun(R) ->
        mist@internal@encoder:response_builder(
            erlang:element(2, Resp),
            erlang:element(3, R)
        )
    end)(_pipe@3),
    _pipe@5 = (erlang:element(11, erlang:element(4, State)))(
        erlang:element(2, State),
        _pipe@4
    ),
    _pipe@6 = gleam@result:map(
        _pipe@5,
        fun(_) ->
            file:sendfile(
                File_descriptor,
                erlang:element(2, State),
                Offset,
                Length,
                []
            )
        end
    ),
    _pipe@7 = gleam@result:replace(_pipe@6, {continue, State}),
    _pipe@8 = gleam@result:replace_error(_pipe@7, {stop, normal}),
    gleam@result:unwrap_both(_pipe@8).

-spec handle_upgrade(
    gleam@http@request:request(mist@internal@http:body()),
    mist@internal@websocket:websocket_handler(),
    glisten@handler:loop_state(state())
) -> gleam@otp@actor:next(glisten@handler:loop_state(state())).
handle_upgrade(Req, Handler, State) ->
    _pipe = Req,
    _pipe@1 = mist@internal@http:upgrade(
        erlang:element(2, State),
        erlang:element(4, State),
        _pipe
    ),
    _pipe@2 = gleam@result:map(
        _pipe@1,
        fun(_) -> _ = case erlang:element(3, Handler) of
                {some, Func} ->
                    Func(erlang:element(3, State));

                _ ->
                    nil
            end end
    ),
    _pipe@3 = gleam@result:replace(
        _pipe@2,
        {continue,
            erlang:setelement(
                5,
                State,
                erlang:setelement(3, erlang:element(5, State), {some, Handler})
            )}
    ),
    _pipe@4 = gleam@result:replace_error(_pipe@3, {stop, normal}),
    gleam@result:unwrap_both(_pipe@4).

-spec int_to_hex(integer()) -> binary().
int_to_hex(Int) ->
    erlang:integer_to_list(Int, 16).

-spec handle_chunked_body(
    gleam@http@response:response(mist@internal@http:http_response_body()),
    gleam@iterator:iterator(gleam@bit_builder:bit_builder()),
    glisten@handler:loop_state(state())
) -> gleam@otp@actor:next(glisten@handler:loop_state(state())).
handle_chunked_body(Resp, Body, State) ->
    Headers = [{<<"transfer-encoding"/utf8>>, <<"chunked"/utf8>>} |
        erlang:element(3, Resp)],
    Initial_payload = mist@internal@encoder:response_builder(
        erlang:element(2, Resp),
        Headers
    ),
    _pipe = (erlang:element(11, erlang:element(4, State)))(
        erlang:element(2, State),
        Initial_payload
    ),
    _pipe@8 = gleam@result:then(_pipe, fun(_) -> _pipe@1 = Body,
            _pipe@2 = gleam@iterator:append(
                _pipe@1,
                gleam@iterator:from_list([gleam@bit_builder:new()])
            ),
            gleam@iterator:try_fold(
                _pipe@2,
                nil,
                fun(_, Chunk) ->
                    Size = gleam@bit_builder:byte_size(Chunk),
                    Encoded = begin
                        _pipe@3 = Size,
                        _pipe@4 = int_to_hex(_pipe@3),
                        _pipe@5 = gleam@bit_builder:from_string(_pipe@4),
                        _pipe@6 = gleam@bit_builder:append_string(
                            _pipe@5,
                            <<"\r\n"/utf8>>
                        ),
                        _pipe@7 = gleam@bit_builder:append_builder(
                            _pipe@6,
                            Chunk
                        ),
                        gleam@bit_builder:append_string(
                            _pipe@7,
                            <<"\r\n"/utf8>>
                        )
                    end,
                    (erlang:element(11, erlang:element(4, State)))(
                        erlang:element(2, State),
                        Encoded
                    )
                end
            ) end),
    _pipe@9 = gleam@result:replace(_pipe@8, {continue, State}),
    gleam@result:unwrap(_pipe@9, {stop, normal}).

-spec with_func(
    fun((gleam@http@request:request(mist@internal@http:body())) -> handler_response())
) -> fun((glisten@handler:handler_message(), glisten@handler:loop_state(state())) -> gleam@otp@actor:next(glisten@handler:loop_state(state()))).
with_func(Handler) ->
    glisten@handler:func(
        fun(Msg, Socket_state) ->
            {loop_state, Socket, _, Transport, State} = Socket_state,
            case erlang:element(3, State) of
                {some, Ws_handler} ->
                    handle_websocket_message(Socket_state, Ws_handler, Msg);

                none ->
                    _pipe@7 = begin
                        _ = case erlang:element(2, State) of
                            {some, T} ->
                                gleam@erlang@process:cancel_timer(T);

                            _ ->
                                timer_not_found
                        end,
                        _pipe = Msg,
                        _pipe@1 = mist@internal@http:parse_request(
                            _pipe,
                            Socket,
                            Transport
                        ),
                        _pipe@2 = gleam@result:map_error(
                            _pipe@1,
                            fun(Err) -> case Err of
                                    discard_packet ->
                                        nil;

                                    _ ->
                                        mist@internal@logger:error(Err),
                                        _ = (erlang:element(4, Transport))(
                                            Socket
                                        ),
                                        nil
                                end end
                        ),
                        _pipe@3 = gleam@result:replace_error(
                            _pipe@2,
                            {stop, normal}
                        ),
                        _pipe@6 = gleam@result:then(
                            _pipe@3,
                            fun(Req) ->
                                _pipe@4 = gleam_erlang_ffi:rescue(
                                    fun() -> Handler(Req) end
                                ),
                                _pipe@5 = gleam@result:map(
                                    _pipe@4,
                                    fun(Resp) -> {Req, Resp} end
                                ),
                                gleam@result:map_error(
                                    _pipe@5,
                                    fun(_capture) ->
                                        log_and_error(
                                            _capture,
                                            erlang:element(2, Socket_state),
                                            erlang:element(4, Socket_state)
                                        )
                                    end
                                )
                            end
                        ),
                        gleam@result:map(
                            _pipe@6,
                            fun(Req_resp) ->
                                {Req@1, Response} = Req_resp,
                                case Response of
                                    {response,
                                        {response,
                                            _,
                                            _,
                                            {bit_builder_body, Body}} = Resp@1} ->
                                        handle_bit_builder_body(
                                            Resp@1,
                                            Body,
                                            Socket_state
                                        );

                                    {response,
                                        {response, _, _, {chunked, Body@1}} = Resp@2} ->
                                        handle_chunked_body(
                                            Resp@2,
                                            Body@1,
                                            Socket_state
                                        );

                                    {response,
                                        {response,
                                            _,
                                            _,
                                            {file_body, _, _, _, _}} = Resp@3} ->
                                        handle_file_body(Resp@3, Socket_state);

                                    {upgrade, With_handler} ->
                                        handle_upgrade(
                                            Req@1,
                                            With_handler,
                                            Socket_state
                                        )
                                end
                            end
                        )
                    end,
                    gleam@result:unwrap_both(_pipe@7)
            end
        end
    ).

-spec with(
    fun((gleam@http@request:request(bitstring())) -> gleam@http@response:response(gleam@bit_builder:bit_builder())),
    integer()
) -> fun((glisten@handler:handler_message(), glisten@handler:loop_state(state())) -> gleam@otp@actor:next(glisten@handler:loop_state(state()))).
with(Handler, Max_body_limit) ->
    Bad_request = begin
        _pipe = gleam@http@response:new(400),
        gleam@http@response:set_body(_pipe, gleam@bit_builder:new())
    end,
    with_func(
        fun(Req) ->
            _pipe@14 = case {gleam@http@request:get_header(
                    Req,
                    <<"content-length"/utf8>>
                ),
                gleam@http@request:get_header(Req, <<"transfer-encoding"/utf8>>)} of
                {{ok, <<"0"/utf8>>}, _} ->
                    _pipe@1 = Req,
                    _pipe@2 = gleam@http@request:set_body(_pipe@1, <<>>),
                    Handler(_pipe@2);

                {{error, nil}, {error, nil}} ->
                    _pipe@1 = Req,
                    _pipe@2 = gleam@http@request:set_body(_pipe@1, <<>>),
                    Handler(_pipe@2);

                {_, {ok, <<"chunked"/utf8>>}} ->
                    _pipe@3 = Req,
                    _pipe@4 = mist@internal@http:read_body(_pipe@3),
                    _pipe@5 = gleam@result:map(_pipe@4, Handler),
                    gleam@result:unwrap(_pipe@5, Bad_request);

                {{ok, Size}, _} ->
                    _pipe@6 = Size,
                    _pipe@7 = gleam@int:parse(_pipe@6),
                    _pipe@13 = gleam@result:map(
                        _pipe@7,
                        fun(Size@1) -> case Size@1 > Max_body_limit of
                                true ->
                                    _pipe@8 = gleam@http@response:new(413),
                                    _pipe@9 = gleam@http@response:set_body(
                                        _pipe@8,
                                        gleam@bit_builder:new()
                                    ),
                                    gleam@http@response:prepend_header(
                                        _pipe@9,
                                        <<"connection"/utf8>>,
                                        <<"close"/utf8>>
                                    );

                                false ->
                                    _pipe@10 = Req,
                                    _pipe@11 = mist@internal@http:read_body(
                                        _pipe@10
                                    ),
                                    _pipe@12 = gleam@result:map(
                                        _pipe@11,
                                        Handler
                                    ),
                                    gleam@result:unwrap(_pipe@12, Bad_request)
                            end end
                    ),
                    gleam@result:unwrap(_pipe@13, Bad_request)
            end,
            _pipe@15 = gleam@http@response:map(
                _pipe@14,
                fun(Field@0) -> {bit_builder_body, Field@0} end
            ),
            {response, _pipe@15}
        end
    ).
