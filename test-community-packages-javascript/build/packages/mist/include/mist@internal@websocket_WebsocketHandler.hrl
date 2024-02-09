-record(websocket_handler, {
    on_close :: gleam@option:option(fun((gleam@erlang@process:subject(glisten@handler:handler_message())) -> nil)),
    on_init :: gleam@option:option(fun((gleam@erlang@process:subject(glisten@handler:handler_message())) -> nil)),
    handler :: fun((mist@internal@websocket:message(), gleam@erlang@process:subject(glisten@handler:handler_message())) -> {ok,
            nil} |
        {error, nil})
}).
