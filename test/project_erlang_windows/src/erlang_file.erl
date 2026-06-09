%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2021 The Gleam contributors

-module(erlang_file).

-export([main/0]).

-include("erlang_header.hrl").

main() ->
  String = header_function(),
  <<"Hello, from the Erlang module!\n"/utf8, String/binary>>.
