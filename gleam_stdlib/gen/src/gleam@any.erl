-module(gleam@any).
-compile(no_auto_import).

-export([from/1, unsafe_coerce/1, string/1, int/1, float/1, atom/1, bool/1, thunk/1, list/2, pair/1, field/2]).

from(A) ->
    gleam_stdlib:identity(A).

unsafe_coerce(A) ->
    gleam_stdlib:identity(A).

string(A) ->
    gleam_stdlib:decode_string(A).

int(A) ->
    gleam_stdlib:decode_int(A).

float(A) ->
    gleam_stdlib:decode_float(A).

atom(A) ->
    gleam_stdlib:decode_atom(A).

bool(A) ->
    gleam_stdlib:decode_bool(A).

thunk(A) ->
    gleam_stdlib:decode_thunk(A).

list_any(A) ->
    gleam_stdlib:decode_list(A).

list(Any, DecoderType) ->
    gleam@result:then(
        list_any(Any),
        fun(Capture1) -> gleam@list:traverse(Capture1, DecoderType) end
    ).

pair(A) ->
    gleam_stdlib:decode_pair(A).

field(A, B) ->
    gleam_stdlib:decode_field(A, B).
