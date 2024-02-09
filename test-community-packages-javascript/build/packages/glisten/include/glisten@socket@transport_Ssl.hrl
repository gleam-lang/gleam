-record(ssl, {
    accept :: fun((glisten@socket:listen_socket()) -> {ok,
            glisten@socket:socket()} |
        {error, glisten@socket:socket_reason()}),
    accept_timeout :: fun((glisten@socket:listen_socket(), integer()) -> {ok,
            glisten@socket:socket()} |
        {error, glisten@socket:socket_reason()}),
    close :: fun((glisten@socket:socket()) -> {ok, nil} |
        {error, glisten@socket:socket_reason()}),
    controlling_process :: fun((glisten@socket:socket(), gleam@erlang@process:pid_()) -> {ok,
            nil} |
        {error, gleam@erlang@atom:atom_()}),
    handshake :: fun((glisten@socket:socket()) -> {ok, glisten@socket:socket()} |
        {error, nil}),
    listen :: fun((integer(), list(glisten@socket@options:tcp_option())) -> {ok,
            glisten@socket:listen_socket()} |
        {error, glisten@socket:socket_reason()}),
    negotiated_protocol :: fun((glisten@socket:socket()) -> {ok, binary()} |
        {error, binary()}),
    'receive' :: fun((glisten@socket:socket(), integer()) -> {ok, bitstring()} |
        {error, glisten@socket:socket_reason()}),
    receive_timeout :: fun((glisten@socket:socket(), integer(), integer()) -> {ok,
            bitstring()} |
        {error, glisten@socket:socket_reason()}),
    send :: fun((glisten@socket:socket(), gleam@bit_builder:bit_builder()) -> {ok,
            nil} |
        {error, glisten@socket:socket_reason()}),
    set_opts :: fun((glisten@socket:socket(), list(glisten@socket@options:tcp_option())) -> {ok,
            nil} |
        {error, nil}),
    shutdown :: fun((glisten@socket:socket()) -> {ok, nil} |
        {error, glisten@socket:socket_reason()}),
    socket_info :: fun((glisten@socket:socket()) -> gleam@map:map_(gleam@erlang@atom:atom_(), gleam@dynamic:dynamic()))
}).
