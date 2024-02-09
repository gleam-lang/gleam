-record(dead_end, {
    row :: integer(),
    col :: integer(),
    problem :: nibble:error(),
    context :: list(nibble:located(any()))
}).
