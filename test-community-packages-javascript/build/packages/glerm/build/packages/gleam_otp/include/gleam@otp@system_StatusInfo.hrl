-record(status_info, {
    module :: gleam@erlang@atom:atom_(),
    parent :: gleam@erlang@process:pid_(),
    mode :: gleam@otp@system:mode(),
    debug_state :: gleam@otp@system:debug_state(),
    state :: gleam@dynamic:dynamic_()
}).
