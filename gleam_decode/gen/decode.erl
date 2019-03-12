-module(decode).
-compile(no_auto_import).

-export([string/1, int/1, float/1, atom/1, bool/1, thunk/1]).

string(A) ->
    gleam__decode_erl:string(A).

int(A) ->
    gleam__decode_erl:int(A).

float(A) ->
    gleam__decode_erl:float(A).

atom(A) ->
    gleam__decode_erl:atom(A).

bool(A) ->
    gleam__decode_erl:bool(A).

thunk(A) ->
    gleam__decode_erl:thunk(A).
