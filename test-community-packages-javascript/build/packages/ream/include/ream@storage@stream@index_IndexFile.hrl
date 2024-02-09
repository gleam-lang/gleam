-record(index_file, {
    handler :: gleam@erlang@process:pid_(),
    size :: integer(),
    file_path :: binary()
}).
