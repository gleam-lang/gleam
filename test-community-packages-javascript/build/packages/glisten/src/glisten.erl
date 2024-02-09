-module(glisten).
-compile([no_auto_import, nowarn_unused_vars]).

-export([serve/2, serve_ssl/4]).
-export_type([start_error/0]).

-type start_error() :: listener_closed |
    listener_timeout |
    acceptor_timeout |
    {acceptor_failed, gleam@erlang@process:exit_reason()} |
    {acceptor_crashed, gleam@dynamic:dynamic()} |
    {system_error, glisten@socket:socket_reason()}.

-spec serve(
    integer(),
    fun((glisten@socket:listen_socket()) -> glisten@acceptor:pool(any()))
) -> {ok, nil} | {error, start_error()}.
serve(Port, With_pool) ->
    _pipe = Port,
    _pipe@1 = glisten@tcp:listen(_pipe, []),
    _pipe@2 = gleam@result:map_error(_pipe@1, fun(Err) -> case Err of
                closed ->
                    listener_closed;

                timeout ->
                    listener_timeout;

                Err@1 ->
                    {system_error, Err@1}
            end end),
    _pipe@6 = gleam@result:then(
        _pipe@2,
        fun(Socket) ->
            _pipe@3 = Socket,
            _pipe@4 = With_pool(_pipe@3),
            _pipe@5 = glisten@acceptor:start_pool(_pipe@4),
            gleam@result:map_error(_pipe@5, fun(Err@2) -> case Err@2 of
                        init_timeout ->
                            acceptor_timeout;

                        {init_failed, Reason} ->
                            {acceptor_failed, Reason};

                        {init_crashed, Reason@1} ->
                            {acceptor_crashed, Reason@1}
                    end end)
        end
    ),
    gleam@result:replace(_pipe@6, nil).

-spec serve_ssl(
    integer(),
    binary(),
    binary(),
    fun((glisten@socket:listen_socket()) -> glisten@acceptor:pool(any()))
) -> {ok, nil} | {error, start_error()}.
serve_ssl(Port, Certfile, Keyfile, With_pool) ->
    _assert_subject = ssl_ffi:start_ssl(),
    {ok, _} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"glisten"/utf8>>,
                        function => <<"serve_ssl"/utf8>>,
                        line => 62})
    end,
    _pipe = Port,
    _pipe@1 = glisten@ssl:listen(
        _pipe,
        [{certfile, Certfile}, {keyfile, Keyfile}]
    ),
    _pipe@2 = gleam@result:map_error(_pipe@1, fun(Err) -> case Err of
                closed ->
                    listener_closed;

                timeout ->
                    listener_timeout;

                Err@1 ->
                    {system_error, Err@1}
            end end),
    _pipe@6 = gleam@result:then(
        _pipe@2,
        fun(Socket) ->
            _pipe@3 = Socket,
            _pipe@4 = (glisten@acceptor:over_ssl(With_pool))(_pipe@3),
            _pipe@5 = glisten@acceptor:start_pool(_pipe@4),
            gleam@result:map_error(_pipe@5, fun(Err@2) -> case Err@2 of
                        init_timeout ->
                            acceptor_timeout;

                        {init_failed, Reason} ->
                            {acceptor_failed, Reason};

                        {init_crashed, Reason@1} ->
                            {acceptor_crashed, Reason@1}
                    end end)
        end
    ),
    gleam@result:replace(_pipe@6, nil).
