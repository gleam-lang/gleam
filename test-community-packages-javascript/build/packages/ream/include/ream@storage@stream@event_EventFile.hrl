-record(event_file, {
    id :: integer(),
    handler :: gleam@erlang@process:pid_(),
    size :: integer(),
    file_path :: binary()
}).
