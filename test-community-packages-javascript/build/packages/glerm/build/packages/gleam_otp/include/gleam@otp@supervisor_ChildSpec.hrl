-record(child_spec, {
    start :: fun((any()) -> {ok, gleam@erlang@process:subject(any())} |
        {error, gleam@otp@actor:start_error()}),
    returning :: fun((any(), gleam@erlang@process:subject(any())) -> any())
}).
