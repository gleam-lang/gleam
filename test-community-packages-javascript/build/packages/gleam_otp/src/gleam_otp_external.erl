-module(gleam_otp_external).

-export([application_stopped/0, convert_system_message/2]).

% TODO: support other system messages
%   {replace_state, StateFn}
%   {change_code, Mod, Vsn, Extra}
%   {terminate, Reason}
%   {debug, {log, Flag}}
%   {debug, {trace, Flag}}
%   {debug, {log_to_file, FileName}}
%   {debug, {statistics, Flag}}
%   {debug, no_debug}
%   {debug, {install, {Func, FuncState}}}
%   {debug, {install, {FuncId, Func, FuncState}}}
%   {debug, {remove, FuncOrId}}
%   GetStatus(Subject(StatusInfo))
convert_system_message({From, Ref}, Request) when is_pid(From) ->
    Reply = fun(Msg) ->
        erlang:send(From, {Ref, Msg}),
        nil
    end,
    System = fun(Callback) ->
        {system, {Request, Callback}}
    end,
    case Request of
        get_status -> System(fun(Status) -> Reply(process_status(Status)) end);
        get_state -> System(Reply);
        suspend -> System(fun() -> Reply(ok) end);
        resume -> System(fun() -> Reply(ok) end);
        Other -> {unexpeceted, Other}
    end.

process_status({status_info, Module, Parent, Mode, DebugState, State}) ->
    Data = [
        get(), Mode, Parent, DebugState,
        [{header, "Status for Gleam process " ++ pid_to_list(self())},
         {data, [{'Status', Mode}, {'Parent', Parent}, {'State', State}]}]
    ],
    {status, self(), {module, Module}, Data}.

application_stopped() ->
    ok.
