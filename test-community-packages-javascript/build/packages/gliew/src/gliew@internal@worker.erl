-module(gliew@internal@worker).
-compile([no_auto_import, nowarn_unused_vars]).

-export([start_worker/1, connect/2, disconnect/2]).
-export_type([state/0, worker_message/0]).

-type state() :: {state,
        gleam@erlang@process:subject(worker_message()),
        gleam@option:option(gleam@erlang@process:subject(glisten@handler:handler_message())),
        gleam@option:option(gleam@erlang@process:timer())}.

-type worker_message() :: {live_update, binary()} |
    {connect_socket,
        gleam@erlang@process:subject(glisten@handler:handler_message())} |
    {disconnect_socket,
        gleam@erlang@process:subject(glisten@handler:handler_message())} |
    shutdown.

-spec worker_loop(worker_message(), state()) -> gleam@otp@actor:next(state()).
worker_loop(Msg, State) ->
    case Msg of
        {live_update, Markup} ->
            case erlang:element(3, State) of
                {some, Sock} ->
                    mist@websocket:send(Sock, {text_message, Markup}),
                    {continue, State};

                none ->
                    {continue, State}
            end;

        {connect_socket, Sock@1} ->
            case erlang:element(4, State) of
                {some, Timer} ->
                    gleam@erlang@process:cancel_timer(Timer),
                    nil;

                none ->
                    nil
            end,
            {continue,
                erlang:setelement(
                    4,
                    erlang:setelement(3, State, {some, Sock@1}),
                    none
                )};

        {disconnect_socket, Sock@2} ->
            case erlang:element(3, State) of
                {some, Socket} when Sock@2 =:= Socket ->
                    _pipe = gleam@erlang@process:send_after(
                        erlang:element(2, State),
                        600000,
                        shutdown
                    ),
                    _pipe@1 = {some, _pipe},
                    _pipe@2 = {state, erlang:element(2, State), none, _pipe@1},
                    {continue, _pipe@2};

                _ ->
                    {continue, State}
            end;

        shutdown ->
            gleam@io:println(
                <<"shutting down worker "/utf8,
                    (gleam@string:inspect(
                        gleam@erlang@process:subject_owner(
                            erlang:element(2, State)
                        )
                    ))/binary>>
            ),
            {stop, normal}
    end.

-spec apply_selects(
    gleam@erlang@process:selector(worker_message()),
    list(fun((gleam@erlang@process:selector(worker_message())) -> gleam@erlang@process:selector(worker_message())))
) -> gleam@erlang@process:selector(worker_message()).
apply_selects(Selector, Selects) ->
    _pipe = Selector,
    gleam@list:fold(
        Selects,
        _pipe,
        fun(Selector@1, Selecting) -> Selecting(Selector@1) end
    ).

-spec start_worker(
    list(fun((gleam@erlang@process:selector(worker_message())) -> gleam@erlang@process:selector(worker_message())))
) -> {ok, gleam@erlang@process:subject(worker_message())} |
    {error, gleam@otp@actor:start_error()}.
start_worker(Selects) ->
    gleam@otp@actor:start_spec(
        {spec,
            fun() ->
                Subject = gleam@erlang@process:new_subject(),
                Selector = begin
                    _pipe = gleam_erlang_ffi:new_selector(),
                    _pipe@1 = gleam@erlang@process:selecting(
                        _pipe,
                        Subject,
                        fun gleam@function:identity/1
                    ),
                    apply_selects(_pipe@1, Selects)
                end,
                Timer = gleam@erlang@process:send_after(
                    Subject,
                    600000,
                    shutdown
                ),
                {ready, {state, Subject, none, {some, Timer}}, Selector}
            end,
            1000,
            fun worker_loop/2}
    ).

-spec connect(
    gleam@erlang@process:subject(worker_message()),
    gleam@erlang@process:subject(glisten@handler:handler_message())
) -> nil.
connect(Worker, Socket) ->
    gleam@erlang@process:send(Worker, {connect_socket, Socket}).

-spec disconnect(
    gleam@erlang@process:subject(worker_message()),
    gleam@erlang@process:subject(glisten@handler:handler_message())
) -> nil.
disconnect(Worker, Socket) ->
    gleam@erlang@process:send(Worker, {disconnect_socket, Socket}).
