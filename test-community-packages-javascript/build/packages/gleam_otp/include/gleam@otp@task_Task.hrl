-record(task, {
    owner :: gleam@erlang@process:pid_(),
    pid :: gleam@erlang@process:pid_(),
    monitor :: gleam@erlang@process:process_monitor(),
    selector :: gleam@erlang@process:selector(gleam@otp@task:message(any()))
}).
