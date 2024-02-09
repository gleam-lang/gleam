-record(loop_state, {
    socket :: glisten@socket:socket(),
    sender :: gleam@erlang@process:subject(glisten@handler:handler_message()),
    transport :: glisten@socket@transport:transport(),
    data :: any()
}).
