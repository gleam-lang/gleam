-record(handler, {
    socket :: glisten@socket:socket(),
    initial_data :: any(),
    loop :: fun((glisten@handler:handler_message(), glisten@handler:loop_state(any())) -> gleam@otp@actor:next(glisten@handler:loop_state(any()))),
    on_init :: gleam@option:option(fun((gleam@erlang@process:subject(glisten@handler:handler_message())) -> nil)),
    on_close :: gleam@option:option(fun((gleam@erlang@process:subject(glisten@handler:handler_message())) -> nil)),
    transport :: glisten@socket@transport:transport()
}).
