-record(pool, {
    listener_socket :: glisten@socket:listen_socket(),
    handler :: fun((glisten@handler:handler_message(), glisten@handler:loop_state(any())) -> gleam@otp@actor:next(glisten@handler:loop_state(any()))),
    initial_data :: any(),
    pool_count :: integer(),
    on_init :: gleam@option:option(fun((gleam@erlang@process:subject(glisten@handler:handler_message())) -> nil)),
    on_close :: gleam@option:option(fun((gleam@erlang@process:subject(glisten@handler:handler_message())) -> nil)),
    transport :: glisten@socket@transport:transport()
}).
