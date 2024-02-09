-module(glenvy@internal@file).
-compile([no_auto_import, nowarn_unused_vars]).

-export([read/1]).

-spec read(binary()) -> {ok, binary()} | {error, binary()}.
read(Path) ->
    _pipe = gleam@erlang@file:read(Path),
    gleam@result:map_error(_pipe, fun gleam@string:inspect/1).
