%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2021 The Gleam contributors

-module(ffi_erlang).

-export([
    to_string/1, append/2, print/1, file_exists/1, halt/1, to_dynamic/1, to_codepoint/1,
    read_uint16_from_bit_array/1, module_source/1
]).

append(A, B) ->
    <<A/binary, B/binary>>.

print(S) ->
    io:format("~s", [S]),
    nil.

to_string(Term) ->
    List = io_lib:format("~p", [Term]),
    iolist_to_binary(List).

file_exists(Path) ->
    filelib:is_regular(Path).

halt(Code) ->
    erlang:halt(Code).

to_dynamic(X) ->
    X.

to_codepoint(X) -> X.

read_uint16_from_bit_array(BitArray) ->
    <<Value:16/big-unsigned-integer, _/binary>> = BitArray,
    Value.

module_source(Name) ->
    Module = binary_to_existing_atom(Name),
    CompileInfo = Module:module_info(compile),
    Source = proplists:get_value(source, CompileInfo),
    unicode:characters_to_binary(Source).
