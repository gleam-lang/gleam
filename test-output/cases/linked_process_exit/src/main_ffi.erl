-module(main_ffi).
-export([spawn_and_exit/0]).

%% Spawn a linked process that exits with a non-standard reason.
%% This tests that the @@main.erl template handles exit reasons
%% that aren't {Reason, StackTrace} tuples (issue #4766).
spawn_and_exit() ->
    spawn_link(fun() ->
        exit({shutdown, <<"test reason">>})
    end),
    %% Wait briefly to ensure the linked process exits and kills us
    receive after 100 -> nil end.
