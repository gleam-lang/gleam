%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2021 The Gleam contributors

-module(erlang_test_file).

-export([main/0, project_source/0]).

-include("erlang_test_header.hrl").

main() ->
  String = header_function(),
  <<"Hello, from the Erlang test module!\n"/utf8, String/binary>>.

project_source() ->
  Source = proplists:get_value(source, project:module_info(compile)),
  unicode:characters_to_binary(Source).
