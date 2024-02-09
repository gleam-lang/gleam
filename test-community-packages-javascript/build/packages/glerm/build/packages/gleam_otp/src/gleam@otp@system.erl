-module(gleam@otp@system).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([debug_state/1, get_state/1, suspend/1, resume/1]).
-export_type([mode/0, debug_option/0, debug_state/0, status_info/0, system_message/0, do_not_leak/0]).

-type mode() :: running | suspended.

-type debug_option() :: no_debug.

-type debug_state() :: any().

-type status_info() :: {status_info,
        gleam@erlang@atom:atom_(),
        gleam@erlang@process:pid_(),
        mode(),
        debug_state(),
        gleam@dynamic:dynamic_()}.

-type system_message() :: {resume, fun(() -> nil)} |
    {suspend, fun(() -> nil)} |
    {get_state, fun((gleam@dynamic:dynamic_()) -> nil)} |
    {get_status, fun((status_info()) -> nil)}.

-type do_not_leak() :: any().

-spec debug_state(list(debug_option())) -> debug_state().
debug_state(A) ->
    sys:debug_options(A).

-spec get_state(gleam@erlang@process:pid_()) -> gleam@dynamic:dynamic_().
get_state(From) ->
    sys:get_state(From).

-spec suspend(gleam@erlang@process:pid_()) -> nil.
suspend(Pid) ->
    sys:suspend(Pid),
    nil.

-spec resume(gleam@erlang@process:pid_()) -> nil.
resume(Pid) ->
    sys:resume(Pid),
    nil.
