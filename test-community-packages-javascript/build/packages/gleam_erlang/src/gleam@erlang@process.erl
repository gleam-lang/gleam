-module(gleam@erlang@process).
-compile([no_auto_import, nowarn_unused_vars]).

-export([subject_owner/1, self/0, new_subject/0, start/2, send/2, new_selector/0, select/2, select_forever/1, map_selector/2, merge_selector/2, flush_messages/0, selecting_trapped_exits/2, selecting/3, 'receive'/2, selecting_record2/3, selecting_record3/3, selecting_record4/3, selecting_record5/3, selecting_record6/3, selecting_record7/3, selecting_record8/3, selecting_anything/2, selecting_process_down/3, sleep/1, sleep_forever/0, is_alive/1, monitor_process/1, demonitor_process/1, try_call/3, call/3, link/1, unlink/1, send_after/3, cancel_timer/1, kill/1, send_exit/1, send_abnormal_exit/2, trap_exits/1]).
-export_type([subject/1, exit_message/0, exit_reason/0, anything_selector_tag/0, process_monitor_flag/0, process_monitor/0, process_down/0, call_error/1, cancelled/0, kill_flag/0, pid_/0, do_not_leak/0, selector/1, timer/0]).

-opaque subject(EZP) :: {subject, pid_(), gleam@erlang:reference_()} |
    {gleam_phantom, EZP}.

-type exit_message() :: {exit_message, pid_(), exit_reason()}.

-type exit_reason() :: normal | killed | {abnormal, binary()}.

-type anything_selector_tag() :: anything.

-type process_monitor_flag() :: process.

-opaque process_monitor() :: {process_monitor, gleam@erlang:reference_()}.

-type process_down() :: {process_down, pid_(), gleam@dynamic:dynamic()}.

-type call_error(EZQ) :: {callee_down, gleam@dynamic:dynamic()} |
    call_timeout |
    {gleam_phantom, EZQ}.

-type cancelled() :: timer_not_found | {cancelled, integer()}.

-type kill_flag() :: kill.

-type pid_() :: any().

-type do_not_leak() :: any().

-type selector(Payload) :: any() | {gleam_phantom, Payload}.

-type timer() :: any().

-spec subject_owner(subject(any())) -> pid_().
subject_owner(Subject) ->
    erlang:element(2, Subject).

-spec self() -> pid_().
self() ->
    erlang:self().

-spec new_subject() -> subject(any()).
new_subject() ->
    {subject, erlang:self(), erlang:make_ref()}.

-spec start(fun(() -> any()), boolean()) -> pid_().
start(Implementation, Link) ->
    case Link of
        true ->
            erlang:spawn_link(Implementation);

        false ->
            erlang:spawn(Implementation)
    end.

-spec send(subject(EZW), EZW) -> nil.
send(Subject, Message) ->
    erlang:send(
        erlang:element(2, Subject),
        {erlang:element(3, Subject), Message}
    ),
    nil.

-spec new_selector() -> selector(any()).
new_selector() ->
    gleam_erlang_ffi:new_selector().

-spec select(selector(FCK), integer()) -> {ok, FCK} | {error, nil}.
select(Field@0, Field@1) ->
    gleam_erlang_ffi:select(Field@0, Field@1).

-spec select_forever(selector(FCO)) -> FCO.
select_forever(Field@0) ->
    gleam_erlang_ffi:select(Field@0).

-spec map_selector(selector(FCS), fun((FCS) -> FCQ)) -> selector(FCQ).
map_selector(Field@0, Field@1) ->
    gleam_erlang_ffi:map_selector(Field@0, Field@1).

-spec merge_selector(selector(FCU), selector(FCU)) -> selector(FCU).
merge_selector(Field@0, Field@1) ->
    gleam_erlang_ffi:merge_selector(Field@0, Field@1).

-spec flush_messages() -> nil.
flush_messages() ->
    gleam_erlang_ffi:flush_messages().

-spec selecting_trapped_exits(selector(FAC), fun((exit_message()) -> FAC)) -> selector(FAC).
selecting_trapped_exits(Selector, Handler) ->
    Tag = erlang:binary_to_atom(<<"EXIT"/utf8>>),
    Handler@1 = fun(Message) ->
        Reason = erlang:element(3, Message),
        Normal = gleam@dynamic:from(normal),
        Killed = gleam@dynamic:from(killed),
        Reason@2 = case gleam@dynamic:string(Reason) of
            _ when Reason =:= Normal ->
                normal;

            _ when Reason =:= Killed ->
                killed;

            {ok, Reason@1} ->
                {abnormal, Reason@1};

            {error, _} ->
                {abnormal, gleam@string:inspect(Reason)}
        end,
        Handler({exit_message, erlang:element(2, Message), Reason@2})
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 3}, Handler@1).

-spec selecting(selector(FAF), subject(FAH), fun((FAH) -> FAF)) -> selector(FAF).
selecting(Selector, Subject, Transform) ->
    Handler = fun(Message) -> Transform(erlang:element(2, Message)) end,
    gleam_erlang_ffi:insert_selector_handler(
        Selector,
        {erlang:element(3, Subject), 2},
        Handler
    ).

-spec 'receive'(subject(EZY), integer()) -> {ok, EZY} | {error, nil}.
'receive'(Subject, Milliseconds) ->
    _pipe = gleam_erlang_ffi:new_selector(),
    _pipe@1 = selecting(_pipe, Subject, fun(X) -> X end),
    gleam_erlang_ffi:select(_pipe@1, Milliseconds).

-spec selecting_record2(
    selector(FAK),
    any(),
    fun((gleam@dynamic:dynamic()) -> FAK)
) -> selector(FAK).
selecting_record2(Selector, Tag, Transform) ->
    Handler = fun(Message) -> Transform(erlang:element(2, Message)) end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 2}, Handler).

-spec selecting_record3(
    selector(FAO),
    any(),
    fun((gleam@dynamic:dynamic(), gleam@dynamic:dynamic()) -> FAO)
) -> selector(FAO).
selecting_record3(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(erlang:element(2, Message), erlang:element(3, Message))
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 3}, Handler).

-spec selecting_record4(
    selector(FAS),
    any(),
    fun((gleam@dynamic:dynamic(), gleam@dynamic:dynamic(), gleam@dynamic:dynamic()) -> FAS)
) -> selector(FAS).
selecting_record4(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 4}, Handler).

-spec selecting_record5(
    selector(FAW),
    any(),
    fun((gleam@dynamic:dynamic(), gleam@dynamic:dynamic(), gleam@dynamic:dynamic(), gleam@dynamic:dynamic()) -> FAW)
) -> selector(FAW).
selecting_record5(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message),
            erlang:element(5, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 5}, Handler).

-spec selecting_record6(
    selector(FBA),
    any(),
    fun((gleam@dynamic:dynamic(), gleam@dynamic:dynamic(), gleam@dynamic:dynamic(), gleam@dynamic:dynamic(), gleam@dynamic:dynamic()) -> FBA)
) -> selector(FBA).
selecting_record6(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message),
            erlang:element(5, Message),
            erlang:element(6, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 6}, Handler).

-spec selecting_record7(
    selector(FBE),
    any(),
    fun((gleam@dynamic:dynamic(), gleam@dynamic:dynamic(), gleam@dynamic:dynamic(), gleam@dynamic:dynamic(), gleam@dynamic:dynamic(), gleam@dynamic:dynamic()) -> FBE)
) -> selector(FBE).
selecting_record7(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message),
            erlang:element(5, Message),
            erlang:element(6, Message),
            erlang:element(7, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 7}, Handler).

-spec selecting_record8(
    selector(FBI),
    any(),
    fun((gleam@dynamic:dynamic(), gleam@dynamic:dynamic(), gleam@dynamic:dynamic(), gleam@dynamic:dynamic(), gleam@dynamic:dynamic(), gleam@dynamic:dynamic(), gleam@dynamic:dynamic()) -> FBI)
) -> selector(FBI).
selecting_record8(Selector, Tag, Transform) ->
    Handler = fun(Message) ->
        Transform(
            erlang:element(2, Message),
            erlang:element(3, Message),
            erlang:element(4, Message),
            erlang:element(5, Message),
            erlang:element(6, Message),
            erlang:element(7, Message),
            erlang:element(8, Message)
        )
    end,
    gleam_erlang_ffi:insert_selector_handler(Selector, {Tag, 8}, Handler).

-spec selecting_anything(selector(FBM), fun((gleam@dynamic:dynamic()) -> FBM)) -> selector(FBM).
selecting_anything(Selector, Handler) ->
    gleam_erlang_ffi:insert_selector_handler(Selector, anything, Handler).

-spec selecting_process_down(
    selector(FBP),
    process_monitor(),
    fun((process_down()) -> FBP)
) -> selector(FBP).
selecting_process_down(Selector, Monitor, Mapping) ->
    gleam_erlang_ffi:insert_selector_handler(
        Selector,
        erlang:element(2, Monitor),
        Mapping
    ).

-spec sleep(integer()) -> nil.
sleep(Field@0) ->
    gleam_erlang_ffi:sleep(Field@0).

-spec sleep_forever() -> nil.
sleep_forever() ->
    gleam_erlang_ffi:sleep_forever().

-spec is_alive(pid_()) -> boolean().
is_alive(Field@0) ->
    erlang:is_process_alive(Field@0).

-spec monitor_process(pid_()) -> process_monitor().
monitor_process(Pid) ->
    _pipe = process,
    _pipe@1 = erlang:monitor(_pipe, Pid),
    {process_monitor, _pipe@1}.

-spec demonitor_process(process_monitor()) -> nil.
demonitor_process(Field@0) ->
    gleam_erlang_ffi:demonitor(Field@0).

-spec try_call(subject(FBS), fun((subject(FBU)) -> FBS), integer()) -> {ok, FBU} |
    {error, call_error(FBU)}.
try_call(Subject, Make_request, Timeout) ->
    Reply_subject = new_subject(),
    Monitor = monitor_process(subject_owner(Subject)),
    send(Subject, Make_request(Reply_subject)),
    Result = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        _pipe@1 = selecting(
            _pipe,
            Reply_subject,
            fun(Field@0) -> {ok, Field@0} end
        ),
        _pipe@2 = selecting_process_down(
            _pipe@1,
            Monitor,
            fun(Down) -> {error, {callee_down, erlang:element(3, Down)}} end
        ),
        gleam_erlang_ffi:select(_pipe@2, Timeout)
    end,
    gleam_erlang_ffi:demonitor(Monitor),
    case Result of
        {error, nil} ->
            {error, call_timeout};

        {ok, Res} ->
            Res
    end.

-spec call(subject(FBZ), fun((subject(FCB)) -> FBZ), integer()) -> FCB.
call(Subject, Make_request, Timeout) ->
    _assert_subject = try_call(Subject, Make_request, Timeout),
    {ok, Resp} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"gleam/erlang/process"/utf8>>,
                        function => <<"call"/utf8>>,
                        line => 593})
    end,
    Resp.

-spec link(pid_()) -> boolean().
link(Field@0) ->
    gleam_erlang_ffi:link(Field@0).

-spec unlink(pid_()) -> nil.
unlink(Pid) ->
    erlang:unlink(Pid),
    nil.

-spec send_after(subject(FCD), integer(), FCD) -> timer().
send_after(Subject, Delay, Message) ->
    erlang:send_after(
        Delay,
        erlang:element(2, Subject),
        {erlang:element(3, Subject), Message}
    ).

-spec cancel_timer(timer()) -> cancelled().
cancel_timer(Timer) ->
    case gleam@dynamic:int(erlang:cancel_timer(Timer)) of
        {ok, I} ->
            {cancelled, I};

        {error, _} ->
            timer_not_found
    end.

-spec kill(pid_()) -> nil.
kill(Pid) ->
    erlang:exit(Pid, kill),
    nil.

-spec send_exit(pid_()) -> nil.
send_exit(Pid) ->
    erlang:exit(Pid, normal),
    nil.

-spec send_abnormal_exit(pid_(), binary()) -> nil.
send_abnormal_exit(Pid, Reason) ->
    erlang:exit(Pid, {abnormal, Reason}),
    nil.

-spec trap_exits(boolean()) -> nil.
trap_exits(Field@0) ->
    gleam_erlang_ffi:trap_exits(Field@0).
