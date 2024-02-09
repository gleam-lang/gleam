-record(get_worker, {
    from :: gleam@erlang@process:subject({ok,
            gleam@erlang@process:subject(gliew@internal@worker:worker_message())} |
        {error, nil}),
    id :: binary(),
    csrf :: binary()
}).
