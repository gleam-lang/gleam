%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2024 The Gleam contributors

-module(external_only_erlang_ffi).
-export([main/0]).

main() ->
    io:format("Hello!\n", []).
