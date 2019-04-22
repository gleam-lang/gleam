-module(float).
-compile(no_auto_import).

-export([parse/1, to_string/1]).

parse(A) ->
    gleam__stdlib:parse_float(A).

to_string(F) ->
    iodata:to_string(iodata:from_float(F)).
