-module(mist@internal@file).
-compile([no_auto_import, nowarn_unused_vars]).

-export([size/1, sendfile/5, open/1]).
-export_type([file_error/0, file_descriptor/0]).

-type file_error() :: is_dir | no_access | no_entry | unknown_file_error.

-type file_descriptor() :: any().

-spec size(bitstring()) -> integer().
size(Field@0) ->
    filelib:file_size(Field@0).

-spec sendfile(
    file_descriptor(),
    glisten@socket:socket(),
    integer(),
    integer(),
    list(any())
) -> {ok, integer()} | {error, gleam@erlang@atom:atom_()}.
sendfile(Field@0, Field@1, Field@2, Field@3, Field@4) ->
    file:sendfile(Field@0, Field@1, Field@2, Field@3, Field@4).

-spec open(bitstring()) -> {ok, file_descriptor()} | {error, file_error()}.
open(Field@0) ->
    mist_ffi:file_open(Field@0).
