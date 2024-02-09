-record(state, {
    idle_timer :: gleam@option:option(gleam@erlang@process:timer()),
    upgraded_handler :: gleam@option:option(mist@internal@websocket:websocket_handler())
}).
