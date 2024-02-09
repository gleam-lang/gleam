-module(gliew@internal@event).
-compile([no_auto_import, nowarn_unused_vars]).

-export_type([event/0]).

-type event() :: {live_mount,
        fun((gleam@erlang@process:selector(gliew@internal@worker:worker_message())) -> gleam@erlang@process:selector(gliew@internal@worker:worker_message()))} |
    morph |
    append.


