-record(listener_spec, {
    init :: fun(() -> {any(),
        gleam@option:option(gleam@erlang@process:selector(any()))}),
    loop :: fun((glerm:listener_message(any()), any()) -> gleam@otp@actor:next(any()))
}).
