-module(glisten@tcp).
-compile([no_auto_import, nowarn_unused_vars]).

-export([handshake/1, controlling_process/2, listen/2, accept_timeout/2, accept/1, receive_timeout/3, 'receive'/2, send/2, socket_info/1, close/1, do_shutdown/2, shutdown/1, set_opts/2, negotiated_protocol/1]).

-spec handshake(glisten@socket:socket()) -> {ok, glisten@socket:socket()} |
    {error, nil}.
handshake(Socket) ->
    {ok, Socket}.

-spec controlling_process(glisten@socket:socket(), gleam@erlang@process:pid_()) -> {ok,
        nil} |
    {error, gleam@erlang@atom:atom_()}.
controlling_process(Field@0, Field@1) ->
    tcp_ffi:controlling_process(Field@0, Field@1).

-spec listen(integer(), list(glisten@socket@options:tcp_option())) -> {ok,
        glisten@socket:listen_socket()} |
    {error, glisten@socket:socket_reason()}.
listen(Port, Options) ->
    _pipe = Options,
    _pipe@1 = glisten@socket@options:merge_with_defaults(_pipe),
    gen_tcp:listen(Port, _pipe@1).

-spec accept_timeout(glisten@socket:listen_socket(), integer()) -> {ok,
        glisten@socket:socket()} |
    {error, glisten@socket:socket_reason()}.
accept_timeout(Field@0, Field@1) ->
    gen_tcp:accept(Field@0, Field@1).

-spec accept(glisten@socket:listen_socket()) -> {ok, glisten@socket:socket()} |
    {error, glisten@socket:socket_reason()}.
accept(Field@0) ->
    gen_tcp:accept(Field@0).

-spec receive_timeout(glisten@socket:socket(), integer(), integer()) -> {ok,
        bitstring()} |
    {error, glisten@socket:socket_reason()}.
receive_timeout(Field@0, Field@1, Field@2) ->
    gen_tcp:recv(Field@0, Field@1, Field@2).

-spec 'receive'(glisten@socket:socket(), integer()) -> {ok, bitstring()} |
    {error, glisten@socket:socket_reason()}.
'receive'(Field@0, Field@1) ->
    gen_tcp:recv(Field@0, Field@1).

-spec send(glisten@socket:socket(), gleam@bit_builder:bit_builder()) -> {ok,
        nil} |
    {error, glisten@socket:socket_reason()}.
send(Field@0, Field@1) ->
    tcp_ffi:send(Field@0, Field@1).

-spec socket_info(glisten@socket:socket()) -> gleam@map:map_(any(), any()).
socket_info(Field@0) ->
    socket:info(Field@0).

-spec close(any()) -> {ok, nil} | {error, glisten@socket:socket_reason()}.
close(Field@0) ->
    tcp_ffi:close(Field@0).

-spec do_shutdown(glisten@socket:socket(), gleam@erlang@atom:atom_()) -> {ok,
        nil} |
    {error, glisten@socket:socket_reason()}.
do_shutdown(Field@0, Field@1) ->
    tcp_ffi:shutdown(Field@0, Field@1).

-spec shutdown(glisten@socket:socket()) -> {ok, nil} |
    {error, glisten@socket:socket_reason()}.
shutdown(Socket) ->
    _assert_subject = gleam_erlang_ffi:atom_from_string(<<"write"/utf8>>),
    {ok, Write} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"glisten/tcp"/utf8>>,
                        function => <<"shutdown"/utf8>>,
                        line => 63})
    end,
    tcp_ffi:shutdown(Socket, Write).

-spec set_opts(
    glisten@socket:socket(),
    list(glisten@socket@options:tcp_option())
) -> {ok, nil} | {error, nil}.
set_opts(Socket, Opts) ->
    _pipe = Opts,
    _pipe@1 = glisten@socket@options:to_map(_pipe),
    _pipe@2 = gleam@map:to_list(_pipe@1),
    _pipe@3 = gleam@list:map(_pipe@2, fun gleam@dynamic:from/1),
    tcp_ffi:set_opts(Socket, _pipe@3).

-spec negotiated_protocol(glisten@socket:socket()) -> any().
negotiated_protocol(Field@0) ->
    tcp:negotiated_protocol(Field@0).
