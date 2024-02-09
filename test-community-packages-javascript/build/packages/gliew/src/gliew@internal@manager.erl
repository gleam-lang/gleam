-module(gliew@internal@manager).
-compile([no_auto_import, nowarn_unused_vars]).

-export([process_tree/3, get_worker/3, start_manager/0]).
-export_type([loop_state/0, session/0, message/0]).

-type loop_state() :: {loop_state, gleam@map:map_(binary(), session())}.

-type session() :: {session,
        binary(),
        binary(),
        list(fun((gleam@erlang@process:selector(gliew@internal@worker:worker_message())) -> gleam@erlang@process:selector(gliew@internal@worker:worker_message()))),
        nakai@html:node_(gliew@internal@event:event()),
        gleam@option:option(gleam@erlang@process:subject(gliew@internal@worker:worker_message()))}.

-type message() :: {process_tree,
        gleam@erlang@process:subject(nakai@html:node_(gliew@internal@event:event())),
        gleam@http@request:request(mist@internal@http:body()),
        nakai@html:node_(gliew@internal@event:event())} |
    {get_worker,
        gleam@erlang@process:subject({ok,
                gleam@erlang@process:subject(gliew@internal@worker:worker_message())} |
            {error, nil}),
        binary(),
        binary()}.

-spec validate_session(binary(), session()) -> {ok, session()} | {error, nil}.
validate_session(Csrf, Sess) ->
    case Sess of
        {session, _, Token, _, _, _} when Csrf =:= Token ->
            {ok, Sess};

        _ ->
            {error, nil}
    end.

-spec get_worker_from_sess(session()) -> {ok,
        gleam@erlang@process:subject(gliew@internal@worker:worker_message())} |
    {error, nil}.
get_worker_from_sess(Sess) ->
    case erlang:element(6, Sess) of
        {some, Worker} ->
            {ok, Worker};

        none ->
            case gliew@internal@worker:start_worker(erlang:element(4, Sess)) of
                {ok, Worker@1} ->
                    {ok, Worker@1};

                {error, _} ->
                    {error, nil}
            end
    end.

-spec extract_event(list(nakai@html@attrs:attr(gliew@internal@event:event()))) -> {ok,
        gliew@internal@event:event()} |
    {error, nil}.
extract_event(Attrs) ->
    _pipe = Attrs,
    gleam@list:find_map(_pipe, fun(Attr) -> case Attr of
                {event, <<"gliew-event"/utf8>>, Event} ->
                    {ok, Event};

                _ ->
                    {error, nil}
            end end).

-spec wrap_live_view(
    nakai@html:node_(gliew@internal@event:event()),
    binary(),
    binary()
) -> nakai@html:node_(gliew@internal@event:event()).
wrap_live_view(Markup, Session_id, Csrf) ->
    nakai@html:'div'(
        [{attr, <<"hx-ext"/utf8>>, <<"ws"/utf8>>},
            {attr,
                <<"ws-connect"/utf8>>,
                <<<<<<"/connect?session="/utf8, Session_id/binary>>/binary,
                        "&csrf="/utf8>>/binary,
                    Csrf/binary>>},
            {attr, <<"hx-ext"/utf8>>, <<"morph"/utf8>>}],
        [Markup]
    ).

-spec process_tree(
    gleam@erlang@process:subject(message()),
    gleam@http@request:request(mist@internal@http:body()),
    nakai@html:node_(gliew@internal@event:event())
) -> nakai@html:node_(gliew@internal@event:event()).
process_tree(Subject, Request, Tree) ->
    gleam@erlang@process:call(
        Subject,
        fun(_capture) -> {process_tree, _capture, Request, Tree} end,
        1000
    ).

-spec get_worker(gleam@erlang@process:subject(message()), binary(), binary()) -> {ok,
        gleam@erlang@process:subject(gliew@internal@worker:worker_message())} |
    {error, nil}.
get_worker(Manager, Id, Csrf) ->
    gleam@erlang@process:call(
        Manager,
        fun(_capture) -> {get_worker, _capture, Id, Csrf} end,
        1000
    ).

-spec extract_selects(
    list(fun((gleam@erlang@process:selector(gliew@internal@worker:worker_message())) -> gleam@erlang@process:selector(gliew@internal@worker:worker_message()))),
    nakai@html:node_(gliew@internal@event:event())
) -> list(fun((gleam@erlang@process:selector(gliew@internal@worker:worker_message())) -> gleam@erlang@process:selector(gliew@internal@worker:worker_message()))).
extract_selects(Selects, Node) ->
    case Node of
        {element, _, Attrs, Children} ->
            case extract_event(Attrs) of
                {ok, {live_mount, Selector}} ->
                    _pipe = Selects,
                    gleam@list:prepend(_pipe, Selector);

                {error, nil} ->
                    _pipe@1 = Children,
                    gleam@list:fold(_pipe@1, Selects, fun extract_selects/2)
            end;

        {leaf_element, _, Attrs@1} ->
            case extract_event(Attrs@1) of
                {ok, {live_mount, Selector@1}} ->
                    _pipe@2 = Selects,
                    gleam@list:prepend(_pipe@2, Selector@1);

                {error, nil} ->
                    Selects
            end;

        _ ->
            Selects
    end.

-spec loop(message(), loop_state()) -> gleam@otp@actor:next(loop_state()).
loop(Message, State) ->
    case Message of
        {process_tree, From, _, Tree} ->
            case extract_selects([], Tree) of
                [] ->
                    gleam@erlang@process:send(From, Tree),
                    {continue, State};

                Selects ->
                    Sess_id = <<"gliew-"/utf8,
                        (gliew@internal@util:random_hex_string(10))/binary>>,
                    Csrf = <<"g-"/utf8,
                        (gliew@internal@util:random_hex_string(24))/binary>>,
                    gleam@erlang@process:send(
                        From,
                        begin
                            _pipe = Tree,
                            wrap_live_view(_pipe, Sess_id, Csrf)
                        end
                    ),
                    {continue,
                        {loop_state,
                            begin
                                _pipe@1 = erlang:element(2, State),
                                gleam@map:insert(
                                    _pipe@1,
                                    Sess_id,
                                    {session,
                                        Sess_id,
                                        Csrf,
                                        Selects,
                                        Tree,
                                        none}
                                )
                            end}}
            end;

        {get_worker, From@1, Id, Csrf@1} ->
            case begin
                _pipe@2 = erlang:element(2, State),
                gleam@map:get(_pipe@2, Id)
            end of
                {ok, Sess} ->
                    case begin
                        _pipe@3 = Sess,
                        _pipe@4 = validate_session(Csrf@1, _pipe@3),
                        _pipe@5 = gleam@result:map(
                            _pipe@4,
                            fun get_worker_from_sess/1
                        ),
                        gleam@result:flatten(_pipe@5)
                    end of
                        {ok, Worker} ->
                            gleam@erlang@process:send(From@1, {ok, Worker}),
                            _pipe@6 = erlang:element(2, State),
                            _pipe@7 = gleam@map:insert(
                                _pipe@6,
                                Id,
                                erlang:setelement(6, Sess, {some, Worker})
                            ),
                            _pipe@8 = {loop_state, _pipe@7},
                            {continue, _pipe@8};

                        {error, nil} ->
                            gleam@erlang@process:send(From@1, {error, nil}),
                            {continue, State}
                    end;

                {error, nil} ->
                    gleam@erlang@process:send(From@1, {error, nil}),
                    {continue, State}
            end
    end.

-spec start_manager() -> {ok, gleam@erlang@process:subject(message())} |
    {error, gleam@otp@actor:start_error()}.
start_manager() ->
    gleam@otp@actor:start({loop_state, gleam@map:new()}, fun loop/2).
