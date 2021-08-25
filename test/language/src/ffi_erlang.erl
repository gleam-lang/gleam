-module(ffi_erlang).

-export([to_string/1, append/2, print/1, ansi_green/1]).

append(A, B) ->
    <<A/binary, B/binary>>.

print(S) ->
    io:format("~s", [S]), 
    nil.

to_string(Term) ->
    List = io_lib:format("~p", [Term]),
    iolist_to_binary(List).

ansi_green(String) ->
    <<"\e[32m", String/binary, "\e[0m">>.
