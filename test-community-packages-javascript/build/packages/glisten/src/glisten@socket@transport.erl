-module(glisten@socket@transport).
-compile([no_auto_import, nowarn_unused_vars]).

-export([socket_info/1, tcp/0, ssl/0]).
-export_type([transport/0]).

-type transport() :: {ssl,
        fun((glisten@socket:listen_socket()) -> {ok, glisten@socket:socket()} |
            {error, glisten@socket:socket_reason()}),
        fun((glisten@socket:listen_socket(), integer()) -> {ok,
                glisten@socket:socket()} |
            {error, glisten@socket:socket_reason()}),
        fun((glisten@socket:socket()) -> {ok, nil} |
            {error, glisten@socket:socket_reason()}),
        fun((glisten@socket:socket(), gleam@erlang@process:pid_()) -> {ok, nil} |
            {error, gleam@erlang@atom:atom_()}),
        fun((glisten@socket:socket()) -> {ok, glisten@socket:socket()} |
            {error, nil}),
        fun((integer(), list(glisten@socket@options:tcp_option())) -> {ok,
                glisten@socket:listen_socket()} |
            {error, glisten@socket:socket_reason()}),
        fun((glisten@socket:socket()) -> {ok, binary()} | {error, binary()}),
        fun((glisten@socket:socket(), integer()) -> {ok, bitstring()} |
            {error, glisten@socket:socket_reason()}),
        fun((glisten@socket:socket(), integer(), integer()) -> {ok, bitstring()} |
            {error, glisten@socket:socket_reason()}),
        fun((glisten@socket:socket(), gleam@bit_builder:bit_builder()) -> {ok,
                nil} |
            {error, glisten@socket:socket_reason()}),
        fun((glisten@socket:socket(), list(glisten@socket@options:tcp_option())) -> {ok,
                nil} |
            {error, nil}),
        fun((glisten@socket:socket()) -> {ok, nil} |
            {error, glisten@socket:socket_reason()}),
        fun((glisten@socket:socket()) -> gleam@map:map_(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic()))} |
    {tcp,
        fun((glisten@socket:listen_socket()) -> {ok, glisten@socket:socket()} |
            {error, glisten@socket:socket_reason()}),
        fun((glisten@socket:listen_socket(), integer()) -> {ok,
                glisten@socket:socket()} |
            {error, glisten@socket:socket_reason()}),
        fun((glisten@socket:socket()) -> {ok, nil} |
            {error, glisten@socket:socket_reason()}),
        fun((glisten@socket:socket(), gleam@erlang@process:pid_()) -> {ok, nil} |
            {error, gleam@erlang@atom:atom_()}),
        fun((glisten@socket:socket()) -> {ok, glisten@socket:socket()} |
            {error, nil}),
        fun((integer(), list(glisten@socket@options:tcp_option())) -> {ok,
                glisten@socket:listen_socket()} |
            {error, glisten@socket:socket_reason()}),
        fun((glisten@socket:socket()) -> {ok, binary()} | {error, binary()}),
        fun((glisten@socket:socket(), integer()) -> {ok, bitstring()} |
            {error, glisten@socket:socket_reason()}),
        fun((glisten@socket:socket(), integer(), integer()) -> {ok, bitstring()} |
            {error, glisten@socket:socket_reason()}),
        fun((glisten@socket:socket(), gleam@bit_builder:bit_builder()) -> {ok,
                nil} |
            {error, glisten@socket:socket_reason()}),
        fun((glisten@socket:socket(), list(glisten@socket@options:tcp_option())) -> {ok,
                nil} |
            {error, nil}),
        fun((glisten@socket:socket()) -> {ok, nil} |
            {error, glisten@socket:socket_reason()}),
        fun((glisten@socket:socket()) -> gleam@map:map_(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic()))}.

-spec socket_info(glisten@socket:socket()) -> gleam@map:map_(any(), any()).
socket_info(Field@0) ->
    socket:info(Field@0).

-spec tcp() -> transport().
tcp() ->
    {tcp,
        fun glisten@tcp:accept/1,
        fun glisten@tcp:accept_timeout/2,
        fun glisten@tcp:close/1,
        fun glisten@tcp:controlling_process/2,
        fun glisten@tcp:handshake/1,
        fun glisten@tcp:listen/2,
        fun(_) -> {error, <<"Can't negotiate protocol on tcp"/utf8>>} end,
        fun glisten@tcp:'receive'/2,
        fun glisten@tcp:receive_timeout/3,
        fun glisten@tcp:send/2,
        fun glisten@tcp:set_opts/2,
        fun glisten@tcp:shutdown/1,
        fun socket:info/1}.

-spec ssl() -> transport().
ssl() ->
    {ssl,
        fun glisten@ssl:accept/1,
        fun glisten@ssl:accept_timeout/2,
        fun glisten@ssl:close/1,
        fun glisten@ssl:controlling_process/2,
        fun glisten@ssl:handshake/1,
        fun glisten@ssl:listen/2,
        fun glisten@ssl:negotiated_protocol/1,
        fun glisten@ssl:'receive'/2,
        fun glisten@ssl:receive_timeout/3,
        fun glisten@ssl:send/2,
        fun glisten@ssl:set_opts/2,
        fun glisten@ssl:shutdown/1,
        fun socket:info/1}.
