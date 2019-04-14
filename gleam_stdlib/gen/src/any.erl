-module(any).
-compile(no_auto_import).

-export([from/1, unsafeCoerce/1, string/1, int/1, float/1, atom/1, bool/1, thunk/1, list/2, tuple/1, field/2]).

list_module() ->
    list.

from(A) ->
    gleam__stdlib:identity(A).

unsafeCoerce(A) ->
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
    result:then(list_any(Any),
                fun(Capture1) ->
                    (list_module()):traverse(Capture1, Decode)
                end).

tuple(A) ->
    gleam__stdlib:decode_tuple(A).

field(A, B) ->
    gleam__stdlib:decode_field(A, B).
