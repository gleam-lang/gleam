-module(gleam@any).
-compile(no_auto_import).

-export([from/1, unsafe_coerce/1, string/1, int/1, float/1, atom/1, bool/1, thunk/1, list/2, pair/1, field/2]).

from(A) ->
    gleam__stdlib:identity(A).

unsafe_coerce(A) ->
    gleam__stdlib:identity(A).

string(A) ->
    gleam__stdlib:decode_string(A).

int(A) ->
    gleam__stdlib:decode_int(A).

float(A) ->
    gleam__stdlib:decode_float(A).

atom(A) ->
    gleam__stdlib:decode_atom(A).

bool(A) ->
    gleam__stdlib:decode_bool(A).

thunk(A) ->
    gleam__stdlib:decode_thunk(A).

list_any(A) ->
    gleam__stdlib:decode_list(A).

list(Any, Decode) ->
    gleam@result:then(
        list_any(Any),
        fun(Capture1) -> gleam@list:traverse(Capture1, Decode) end
    ).

pair(A) ->
    gleam__stdlib:decode_pair(A).

field(A, B) ->
    gleam__stdlib:decode_field(A, B).
