-module(glisten@acceptor).
-compile([no_auto_import, nowarn_unused_vars]).

-export([start/1, new_pool/1, new_pool_with_data/2, with_init/2, with_close/2, with_pool_size/2, over_ssl/1, start_pool/1]).
-export_type([acceptor_message/0, acceptor_error/0, acceptor_state/0, pool/1]).

-type acceptor_message() :: {accept_connection, glisten@socket:listen_socket()}.

-type acceptor_error() :: accept_error | handler_error | control_error.

-type acceptor_state() :: {acceptor_state,
        gleam@erlang@process:subject(acceptor_message()),
        gleam@option:option(glisten@socket:socket()),
        glisten@socket@transport:transport()}.

-type pool(GRD) :: {pool,
        glisten@socket:listen_socket(),
        fun((glisten@handler:handler_message(), glisten@handler:loop_state(GRD)) -> gleam@otp@actor:next(glisten@handler:loop_state(GRD))),
        GRD,
        integer(),
        gleam@option:option(fun((gleam@erlang@process:subject(glisten@handler:handler_message())) -> nil)),
        gleam@option:option(fun((gleam@erlang@process:subject(glisten@handler:handler_message())) -> nil)),
        glisten@socket@transport:transport()}.

-spec start(pool(any())) -> {ok,
        gleam@erlang@process:subject(acceptor_message())} |
    {error, gleam@otp@actor:start_error()}.
start(Pool) ->
    gleam@otp@actor:start_spec(
        {spec,
            fun() ->
                Subject = gleam@erlang@process:new_subject(),
                Selector = begin
                    _pipe = gleam_erlang_ffi:new_selector(),
                    gleam@erlang@process:selecting(
                        _pipe,
                        Subject,
                        fun gleam@function:identity/1
                    )
                end,
                gleam@erlang@process:send(
                    Subject,
                    {accept_connection, erlang:element(2, Pool)}
                ),
                {ready,
                    {acceptor_state, Subject, none, erlang:element(8, Pool)},
                    Selector}
            end,
            1000,
            fun(Msg, State) ->
                {acceptor_state, Sender, _, _} = State,
                case Msg of
                    {accept_connection, Listener} ->
                        Res = begin
                            gleam@result:then(
                                begin
                                    _pipe@1 = (erlang:element(
                                        2,
                                        erlang:element(4, State)
                                    ))(Listener),
                                    gleam@result:replace_error(
                                        _pipe@1,
                                        accept_error
                                    )
                                end,
                                fun(Sock) ->
                                    gleam@result:then(
                                        begin
                                            _pipe@2 = {handler,
                                                Sock,
                                                erlang:element(4, Pool),
                                                erlang:element(3, Pool),
                                                erlang:element(6, Pool),
                                                erlang:element(7, Pool),
                                                erlang:element(8, Pool)},
                                            _pipe@3 = glisten@handler:start(
                                                _pipe@2
                                            ),
                                            gleam@result:replace_error(
                                                _pipe@3,
                                                handler_error
                                            )
                                        end,
                                        fun(Start) ->
                                            _pipe@4 = Sock,
                                            _pipe@5 = (erlang:element(
                                                5,
                                                erlang:element(4, State)
                                            ))(
                                                _pipe@4,
                                                gleam@erlang@process:subject_owner(
                                                    Start
                                                )
                                            ),
                                            _pipe@6 = gleam@result:replace_error(
                                                _pipe@5,
                                                control_error
                                            ),
                                            gleam@result:map(
                                                _pipe@6,
                                                fun(_) ->
                                                    gleam@erlang@process:send(
                                                        Start,
                                                        ready
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end,
                        case Res of
                            {error, Reason} ->
                                glisten@logger:error(
                                    {<<"Failed to accept/start handler"/utf8>>,
                                        Reason}
                                ),
                                {stop,
                                    {abnormal,
                                        <<"Failed to accept/start handler"/utf8>>}};

                            _ ->
                                gleam@otp@actor:send(
                                    Sender,
                                    {accept_connection, Listener}
                                ),
                                {continue, State}
                        end;

                    Msg@1 ->
                        glisten@logger:error(
                            {<<"Unknown message type"/utf8>>, Msg@1}
                        ),
                        {stop, {abnormal, <<"Unknown message type"/utf8>>}}
                end
            end}
    ).

-spec new_pool(
    fun((glisten@handler:handler_message(), glisten@handler:loop_state(nil)) -> gleam@otp@actor:next(glisten@handler:loop_state(nil)))
) -> fun((glisten@socket:listen_socket()) -> pool(nil)).
new_pool(Handler) ->
    fun(Listener_socket) ->
        {pool,
            Listener_socket,
            Handler,
            nil,
            10,
            none,
            none,
            glisten@socket@transport:tcp()}
    end.

-spec new_pool_with_data(
    fun((glisten@handler:handler_message(), glisten@handler:loop_state(GRL)) -> gleam@otp@actor:next(glisten@handler:loop_state(GRL))),
    GRL
) -> fun((glisten@socket:listen_socket()) -> pool(GRL)).
new_pool_with_data(Handler, Initial_data) ->
    fun(Listener_socket) ->
        {pool,
            Listener_socket,
            Handler,
            Initial_data,
            10,
            none,
            none,
            glisten@socket@transport:tcp()}
    end.

-spec with_init(
    fun((glisten@socket:listen_socket()) -> pool(GRO)),
    fun((gleam@erlang@process:subject(glisten@handler:handler_message())) -> nil)
) -> fun((glisten@socket:listen_socket()) -> pool(GRO)).
with_init(Make_pool, Func) ->
    fun(Socket) ->
        Pool = Make_pool(Socket),
        erlang:setelement(6, Pool, {some, Func})
    end.

-spec with_close(
    fun((glisten@socket:listen_socket()) -> pool(GRS)),
    fun((gleam@erlang@process:subject(glisten@handler:handler_message())) -> nil)
) -> fun((glisten@socket:listen_socket()) -> pool(GRS)).
with_close(Make_pool, Func) ->
    fun(Socket) ->
        Pool = Make_pool(Socket),
        erlang:setelement(7, Pool, {some, Func})
    end.

-spec with_pool_size(
    fun((glisten@socket:listen_socket()) -> pool(GRW)),
    integer()
) -> fun((glisten@socket:listen_socket()) -> pool(GRW)).
with_pool_size(Make_pool, Pool_count) ->
    fun(Socket) ->
        Pool = Make_pool(Socket),
        erlang:setelement(5, Pool, Pool_count)
    end.

-spec over_ssl(fun((glisten@socket:listen_socket()) -> pool(GRZ))) -> fun((glisten@socket:listen_socket()) -> pool(GRZ)).
over_ssl(Make_pool) ->
    fun(Socket) ->
        Pool = Make_pool(Socket),
        erlang:setelement(8, Pool, glisten@socket@transport:ssl())
    end.

-spec start_pool(pool(any())) -> {ok,
        gleam@erlang@process:subject(gleam@otp@supervisor:message())} |
    {error, gleam@otp@actor:start_error()}.
start_pool(Pool) ->
    gleam@otp@supervisor:start_spec(
        {spec,
            nil,
            100,
            1,
            fun(Children) ->
                _pipe = gleam@iterator:range(0, erlang:element(5, Pool)),
                gleam@iterator:fold(
                    _pipe,
                    Children,
                    fun(Children@1, _) ->
                        gleam@otp@supervisor:add(
                            Children@1,
                            gleam@otp@supervisor:worker(
                                fun(_) -> start(Pool) end
                            )
                        )
                    end
                )
            end}
    ).
