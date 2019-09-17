-module(gleam@atom).
-compile(no_auto_import).

-export([from_string/1, create_from_string/1, to_string/1]).

from_string(A) ->
    gleam_stdlib:atom_from_string(A).

create_from_string(A) ->
    gleam_stdlib:atom_create_from_string(A).

to_string(A) ->
    gleam_stdlib:atom_to_string(A).
