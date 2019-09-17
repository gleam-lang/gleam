-module(gleam@iodata).
-compile(no_auto_import).

-export([prepend/2, append/2, prepend_iodata/2, append_iodata/2, from_strings/1, concat/1, new/1, to_string/1, byte_size/1, from_float/1, lowercase/1, uppercase/1, reverse/1, split/2, replace/3, is_equal/2, is_empty/1]).

prepend(A, B) ->
    gleam_stdlib:iodata_prepend(A, B).

append(A, B) ->
    gleam_stdlib:iodata_append(A, B).

prepend_iodata(A, B) ->
    gleam_stdlib:iodata_prepend(A, B).

append_iodata(A, B) ->
    gleam_stdlib:iodata_append(A, B).

from_strings(A) ->
    gleam_stdlib:identity(A).

concat(A) ->
    gleam_stdlib:identity(A).

new(A) ->
    gleam_stdlib:identity(A).

to_string(A) ->
    erlang:iolist_to_binary(A).

byte_size(A) ->
    erlang:iolist_size(A).

from_float(A) ->
    io_lib_format:fwrite_g(A).

lowercase(A) ->
    string:lowercase(A).

uppercase(A) ->
    string:uppercase(A).

reverse(A) ->
    string:reverse(A).

erl_split(A, B, C) ->
    string:split(A, B, C).

split(Iodata, On) ->
    erl_split(Iodata, On, all).

erl_replace(A, B, C, D) ->
    string:replace(A, B, C, D).

replace(Iodata, Pattern, Replacement) ->
    erl_replace(Iodata, Pattern, Replacement, all).

is_equal(A, B) ->
    string:equal(A, B).

is_empty(A) ->
    string:is_empty(A).
