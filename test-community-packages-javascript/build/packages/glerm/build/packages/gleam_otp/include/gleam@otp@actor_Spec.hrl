-record(spec, {
    init :: fun(() -> gleam@otp@actor:init_result(any(), any())),
    init_timeout :: integer(),
    loop :: fun((any(), any()) -> gleam@otp@actor:next(any(), any()))
}).
