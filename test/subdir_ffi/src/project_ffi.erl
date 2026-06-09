%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2024 The Gleam contributors

-module(project_ffi).

-export([log/1]).

log(Message) ->
  erlang:display(Message).
