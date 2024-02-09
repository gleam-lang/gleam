-record(value_file, {
    id :: integer(),
    handler :: gleam@erlang@process:pid_(),
    size :: integer(),
    max_size :: integer(),
    file_path :: binary()
}).
