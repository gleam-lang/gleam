-record(spec, {
    argument :: any(),
    max_frequency :: integer(),
    frequency_period :: integer(),
    init :: fun((gleam@otp@supervisor:children(any())) -> gleam@otp@supervisor:children(any()))
}).
