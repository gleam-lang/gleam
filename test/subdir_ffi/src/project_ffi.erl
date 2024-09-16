-module(project_ffi).

-export([log/1]).

log(Message) ->
  erlang:display(Message).
