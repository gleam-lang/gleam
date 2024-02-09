-module(globe).
-compile([no_auto_import, nowarn_unused_vars]).

-export([main/0]).

-spec main() -> nil.
main() ->
    gleam@io:println(<<"Hello from globe!"/utf8>>).
