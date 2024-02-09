-module(gleam@io).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function]).

-export([print/1, print_error/1, println/1, println_error/1, debug/1]).

-spec print(binary()) -> nil.
print(String) ->
    gleam_stdlib:print(String).

-spec print_error(binary()) -> nil.
print_error(String) ->
    gleam_stdlib:print_error(String).

-spec println(binary()) -> nil.
println(String) ->
    gleam_stdlib:println(String).

-spec println_error(binary()) -> nil.
println_error(String) ->
    gleam_stdlib:println_error(String).

-spec debug(ENH) -> ENH.
debug(Term) ->
    _pipe = Term,
    _pipe@1 = gleam@string:inspect(_pipe),
    gleam_stdlib:println_error(_pipe@1),
    Term.
