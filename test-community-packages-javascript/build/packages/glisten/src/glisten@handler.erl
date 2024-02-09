-module(glisten@handler).
-compile([no_auto_import, nowarn_unused_vars]).

-export([start/1, func/1]).
-export_type([handler_message/0, loop_state/1, handler/1]).

-type handler_message() :: close |
    ready |
    {receive_message, bitstring()} |
    {send_message, gleam@bit_builder:bit_builder()} |
    {ssl, gleam@otp@port:port_(), bitstring()} |
    {ssl_closed, nil} |
    {tcp, gleam@otp@port:port_(), bitstring()} |
    {tcp_closed, nil}.

-type loop_state(GMF) :: {loop_state,
        glisten@socket:socket(),
        gleam@erlang@process:subject(handler_message()),
        glisten@socket@transport:transport(),
        GMF}.

-type handler(GMG) :: {handler,
        glisten@socket:socket(),
        GMG,
        fun((handler_message(), loop_state(GMG)) -> gleam@otp@actor:next(loop_state(GMG))),
        gleam@option:option(fun((gleam@erlang@process:subject(handler_message())) -> nil)),
        gleam@option:option(fun((gleam@erlang@process:subject(handler_message())) -> nil)),
        glisten@socket@transport:transport()}.

-spec start(handler(any())) -> {ok,
        gleam@erlang@process:subject(handler_message())} |
    {error, gleam@otp@actor:start_error()}.
start(Handler) ->
    gleam@otp@actor:start_spec(
        {spec,
            fun() ->
                Subject = gleam@erlang@process:new_subject(),
                Selector = begin
                    _pipe = gleam_erlang_ffi:new_selector(),
                    _pipe@1 = gleam@erlang@process:selecting(
                        _pipe,
                        Subject,
                        fun gleam@function:identity/1
                    ),
                    gleam@erlang@process:selecting_anything(
                        _pipe@1,
                        fun(Msg) -> case gleam@dynamic:unsafe_coerce(Msg) of
                                {tcp, _, Data} ->
                                    {receive_message, Data};

                                {ssl, _, Data} ->
                                    {receive_message, Data};

                                Msg@1 ->
                                    Msg@1
                            end end
                    )
                end,
                {ready,
                    {loop_state,
                        erlang:element(2, Handler),
                        Subject,
                        erlang:element(7, Handler),
                        erlang:element(3, Handler)},
                    Selector}
            end,
            1000,
            fun(Msg@2, State) -> case Msg@2 of
                    {tcp_closed, _} ->
                        case (erlang:element(4, erlang:element(4, State)))(
                            erlang:element(2, State)
                        ) of
                            {ok, nil} ->
                                _ = case erlang:element(6, Handler) of
                                    {some, Func} ->
                                        Func(erlang:element(3, State));

                                    _ ->
                                        nil
                                end,
                                {stop, normal};

                            {error, Err} ->
                                {stop, {abnormal, gleam@string:inspect(Err)}}
                        end;

                    {ssl_closed, _} ->
                        case (erlang:element(4, erlang:element(4, State)))(
                            erlang:element(2, State)
                        ) of
                            {ok, nil} ->
                                _ = case erlang:element(6, Handler) of
                                    {some, Func} ->
                                        Func(erlang:element(3, State));

                                    _ ->
                                        nil
                                end,
                                {stop, normal};

                            {error, Err} ->
                                {stop, {abnormal, gleam@string:inspect(Err)}}
                        end;

                    close ->
                        case (erlang:element(4, erlang:element(4, State)))(
                            erlang:element(2, State)
                        ) of
                            {ok, nil} ->
                                _ = case erlang:element(6, Handler) of
                                    {some, Func} ->
                                        Func(erlang:element(3, State));

                                    _ ->
                                        nil
                                end,
                                {stop, normal};

                            {error, Err} ->
                                {stop, {abnormal, gleam@string:inspect(Err)}}
                        end;

                    ready ->
                        _pipe@2 = erlang:element(2, State),
                        _pipe@3 = (erlang:element(6, erlang:element(4, State)))(
                            _pipe@2
                        ),
                        _pipe@4 = gleam@result:replace_error(
                            _pipe@3,
                            <<"Failed to handshake socket"/utf8>>
                        ),
                        _pipe@5 = gleam@result:map(
                            _pipe@4,
                            fun(_) -> _ = case erlang:element(5, Handler) of
                                    {some, Func@1} ->
                                        Func@1(erlang:element(3, State));

                                    _ ->
                                        nil
                                end end
                        ),
                        _pipe@7 = gleam@result:then(
                            _pipe@5,
                            fun(_) ->
                                _pipe@6 = (erlang:element(
                                    12,
                                    erlang:element(4, State)
                                ))(
                                    erlang:element(2, State),
                                    [{active_mode, once}]
                                ),
                                gleam@result:replace_error(
                                    _pipe@6,
                                    <<"Failed to set socket active"/utf8>>
                                )
                            end
                        ),
                        _pipe@8 = gleam@result:replace(
                            _pipe@7,
                            {continue, State}
                        ),
                        _pipe@9 = gleam@result:map_error(
                            _pipe@8,
                            fun(Reason) -> {stop, {abnormal, Reason}} end
                        ),
                        gleam@result:unwrap_both(_pipe@9);

                    Msg@3 ->
                        case (erlang:element(4, Handler))(Msg@3, State) of
                            {continue, Next_state} ->
                                _assert_subject = (erlang:element(
                                    12,
                                    erlang:element(4, State)
                                ))(
                                    erlang:element(2, State),
                                    [{active_mode, once}]
                                ),
                                {ok, nil} = case _assert_subject of
                                    {ok, nil} -> _assert_subject;
                                    _assert_fail ->
                                        erlang:error(#{gleam_error => assert,
                                                    message => <<"Assertion pattern match failed"/utf8>>,
                                                    value => _assert_fail,
                                                    module => <<"glisten/handler"/utf8>>,
                                                    function => <<"start"/utf8>>,
                                                    line => 116})
                                end,
                                {continue, Next_state};

                            Msg@4 ->
                                Msg@4
                        end
                end end}
    ).

-spec func(
    fun((bitstring(), loop_state(GMU)) -> gleam@otp@actor:next(loop_state(GMU)))
) -> fun((handler_message(), loop_state(GMU)) -> gleam@otp@actor:next(loop_state(GMU))).
func(Func) ->
    fun(Msg, State) -> case Msg of
            {tcp, _, _} ->
                glisten@logger:error(
                    {<<"Received an unexpected TCP message"/utf8>>, Msg}
                ),
                {continue, State};

            ready ->
                glisten@logger:error(
                    {<<"Received an unexpected TCP message"/utf8>>, Msg}
                ),
                {continue, State};

            {receive_message, Data} ->
                Func(Data, State);

            {send_message, Data@1} ->
                case (erlang:element(11, erlang:element(4, State)))(
                    erlang:element(2, State),
                    Data@1
                ) of
                    {ok, _} ->
                        {continue, State};

                    {error, Reason} ->
                        glisten@logger:error(
                            {<<"Failed to send data"/utf8>>, Reason}
                        ),
                        {stop, {abnormal, <<"Failed to send data"/utf8>>}}
                end;

            Msg@1 ->
                glisten@logger:error({<<"Unhandled TCP message"/utf8>>, Msg@1}),
                {stop, {abnormal, <<"Unhandled TCP message"/utf8>>}}
        end end.
