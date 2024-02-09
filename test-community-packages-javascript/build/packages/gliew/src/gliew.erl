-module(gliew).
-compile([no_auto_import, nowarn_unused_vars]).

-export([view/2, response/1, with_header/3, with_body/2, morph/0, append/0, on_click/3, live_mount/3, script/0, new/2, serve/1]).
-export_type([response/0, server/0]).

-opaque response() :: {view,
        integer(),
        list({binary(), binary()}),
        nakai@html:node_(gliew@internal@event:event())} |
    {response,
        integer(),
        list({binary(), binary()}),
        gleam@option:option(binary())}.

-type server() :: {server,
        integer(),
        fun((nakai@html:node_(gliew@internal@event:event())) -> nakai@html:node_(gliew@internal@event:event())),
        fun((gleam@http@request:request(mist@internal@http:body())) -> response())}.

-spec view(nakai@html:node_(gliew@internal@event:event()), integer()) -> response().
view(Node, Status) ->
    {view, Status, [], Node}.

-spec response(integer()) -> response().
response(Status) ->
    {response, Status, [], none}.

-spec with_header(response(), binary(), binary()) -> response().
with_header(Response, Key, Value) ->
    case Response of
        {view, Status, Headers, Node} ->
            _pipe = Headers,
            _pipe@1 = gleam@list:prepend(_pipe, {Key, Value}),
            {view, Status, _pipe@1, Node};

        {response, Status@1, Headers@1, Body} ->
            _pipe@2 = Headers@1,
            _pipe@3 = gleam@list:prepend(_pipe@2, {Key, Value}),
            {response, Status@1, _pipe@3, Body}
    end.

-spec with_body(response(), binary()) -> response().
with_body(Response, Body) ->
    case Response of
        {response, Status, Headers, _} ->
            {response, Status, Headers, {some, Body}};

        _ ->
            Response
    end.

-spec morph() -> nakai@html@attrs:attr(gliew@internal@event:event()).
morph() ->
    {event, <<"gliew-event"/utf8>>, morph}.

-spec append() -> nakai@html@attrs:attr(gliew@internal@event:event()).
append() ->
    {event, <<"gliew-event"/utf8>>, append}.

-spec add_attr(
    nakai@html:node_(gliew@internal@event:event()),
    nakai@html@attrs:attr(gliew@internal@event:event())
) -> nakai@html:node_(gliew@internal@event:event()).
add_attr(Node, Attr) ->
    case Node of
        {element, Tag, Attrs, Children} ->
            _pipe = Attrs,
            _pipe@1 = gleam@list:prepend(_pipe, Attr),
            {element, Tag, _pipe@1, Children};

        {leaf_element, Tag@1, Attrs@1} ->
            _pipe@2 = Attrs@1,
            _pipe@3 = gleam@list:prepend(_pipe@2, Attr),
            {leaf_element, Tag@1, _pipe@3};

        {html, Attrs@2, Children@1} ->
            _pipe@4 = Attrs@2,
            _pipe@5 = gleam@list:prepend(_pipe@4, Attr),
            {html, _pipe@5, Children@1};

        {body, Attrs@3, Children@2} ->
            _pipe@6 = Attrs@3,
            _pipe@7 = gleam@list:prepend(_pipe@6, Attr),
            {body, _pipe@7, Children@2};

        Other ->
            Other
    end.

-spec on_click(
    nakai@html:node_(gliew@internal@event:event()),
    gleam@http:method(),
    binary()
) -> nakai@html:node_(gliew@internal@event:event()).
on_click(Node, Method, Path) ->
    case Method of
        get ->
            _pipe = Node,
            _pipe@1 = add_attr(_pipe, {attr, <<"hx-get"/utf8>>, Path}),
            add_attr(_pipe@1, {attr, <<"hx-swap"/utf8>>, <<"none"/utf8>>});

        post ->
            _pipe@2 = Node,
            _pipe@3 = add_attr(_pipe@2, {attr, <<"hx-post"/utf8>>, Path}),
            add_attr(_pipe@3, {attr, <<"hx-swap"/utf8>>, <<"none"/utf8>>});

        put ->
            _pipe@4 = Node,
            _pipe@5 = add_attr(_pipe@4, {attr, <<"hx-put"/utf8>>, Path}),
            add_attr(_pipe@5, {attr, <<"hx-swap"/utf8>>, <<"none"/utf8>>});

        patch ->
            _pipe@6 = Node,
            _pipe@7 = add_attr(_pipe@6, {attr, <<"hx-patch"/utf8>>, Path}),
            add_attr(_pipe@7, {attr, <<"hx-swap"/utf8>>, <<"none"/utf8>>});

        delete ->
            _pipe@8 = Node,
            _pipe@9 = add_attr(_pipe@8, {attr, <<"hx-delete"/utf8>>, Path}),
            add_attr(_pipe@9, {attr, <<"hx-swap"/utf8>>, <<"none"/utf8>>});

        _ ->
            Node
    end.

-spec insert_event(
    gliew@internal@event:event(),
    nakai@html:node_(gliew@internal@event:event())
) -> nakai@html:node_(gliew@internal@event:event()).
insert_event(Event, Node) ->
    _pipe = Node,
    add_attr(_pipe, {event, <<"gliew-event"/utf8>>, Event}).

-spec map_events(
    list(nakai@html@attrs:attr(gliew@internal@event:event())),
    boolean()
) -> list(nakai@html@attrs:attr(gliew@internal@event:event())).
map_events(Attrs, Render_events) ->
    case Render_events of
        true ->
            gleam@list:map(Attrs, fun(Attr) -> case Attr of
                        {event, <<"gliew-event"/utf8>>, morph} ->
                            {attr, <<"hx-swap-oob"/utf8>>, <<"morph"/utf8>>};

                        {event, <<"gliew-event"/utf8>>, append} ->
                            {attr, <<"hx-swap-oob"/utf8>>, <<"beforeend"/utf8>>};

                        Other ->
                            Other
                    end end);

        false ->
            Attrs
    end.

-spec find_id(list(nakai@html@attrs:attr(gliew@internal@event:event()))) -> {ok,
        binary()} |
    {error, nil}.
find_id(Attrs) ->
    _pipe = Attrs,
    gleam@list:find_map(_pipe, fun(Attr) -> case Attr of
                {attr, <<"id"/utf8>>, Id} ->
                    {ok, Id};

                _ ->
                    {error, nil}
            end end).

-spec random_id() -> binary().
random_id() ->
    <<"g-"/utf8, (gliew@internal@util:random_hex_string(3))/binary>>.

-spec extract_id(nakai@html:node_(gliew@internal@event:event())) -> binary().
extract_id(Node) ->
    _pipe = case Node of
        {element, _, Attrs, _} ->
            find_id(Attrs);

        {leaf_element, _, Attrs@1} ->
            find_id(Attrs@1);

        _ ->
            {error, nil}
    end,
    gleam@result:unwrap(_pipe, random_id()).

-spec has_id(list(nakai@html@attrs:attr(any()))) -> boolean().
has_id(Attrs) ->
    _pipe = Attrs,
    gleam@list:any(_pipe, fun(A) -> case A of
                {attr, <<"id"/utf8>>, _} ->
                    true;

                _ ->
                    false
            end end).

-spec ensure_id(
    list(nakai@html@attrs:attr(gliew@internal@event:event())),
    gleam@option:option(binary())
) -> list(nakai@html@attrs:attr(gliew@internal@event:event())).
ensure_id(Attrs, Id) ->
    case has_id(Attrs) of
        true ->
            case Id of
                {some, Id@1} ->
                    gleam@list:map(Attrs, fun(Attr) -> case Attr of
                                {attr, <<"id"/utf8>>, _} ->
                                    nakai@html@attrs:id(Id@1);

                                Other ->
                                    Other
                            end end);

                none ->
                    Attrs
            end;

        false ->
            _pipe = Attrs,
            gleam@list:prepend(
                _pipe,
                nakai@html@attrs:id(
                    begin
                        _pipe@1 = Id,
                        gleam@option:unwrap(_pipe@1, random_id())
                    end
                )
            )
    end.

-spec process_tree(
    nakai@html:node_(gliew@internal@event:event()),
    gleam@option:option(binary()),
    boolean()
) -> nakai@html:node_(gliew@internal@event:event()).
process_tree(Node, Id, Render_events) ->
    case Node of
        {element, Tag, Attrs, Children} ->
            _pipe = Attrs,
            _pipe@1 = ensure_id(_pipe, Id),
            _pipe@2 = map_events(_pipe@1, Render_events),
            {element, Tag, _pipe@2, Children};

        {leaf_element, Tag@1, Attrs@1} ->
            _pipe@3 = Attrs@1,
            _pipe@4 = ensure_id(_pipe@3, Id),
            _pipe@5 = map_events(_pipe@4, Render_events),
            {leaf_element, Tag@1, _pipe@5};

        Other ->
            Other
    end.

-spec live_mount(
    fun((MEC) -> gleam@erlang@process:subject(MED)),
    MEC,
    fun((gleam@option:option(MED)) -> nakai@html:node_(gliew@internal@event:event()))
) -> nakai@html:node_(gliew@internal@event:event()).
live_mount(Mount, Context, Render) ->
    Tree = begin
        _pipe = Render(none),
        process_tree(_pipe, none, false)
    end,
    Id = extract_id(Tree),
    _pipe@5 = fun(Selector) ->
        Subject = Mount(Context),
        _pipe@1 = Selector,
        gleam@erlang@process:selecting(
            _pipe@1,
            Subject,
            fun(Val) -> _pipe@2 = Render({some, Val}),
                _pipe@3 = process_tree(_pipe@2, {some, Id}, true),
                _pipe@4 = nakai:to_inline_string(_pipe@3),
                {live_update, _pipe@4} end
        )
    end,
    _pipe@6 = {live_mount, _pipe@5},
    insert_event(_pipe@6, Tree).

-spec script() -> nakai@html:node_(any()).
script() ->
    {fragment,
        [{element,
                <<"script"/utf8>>,
                [nakai@html@attrs:src(
                        <<"https://unpkg.com/htmx.org@1.9.2/dist/htmx.min.js"/utf8>>
                    )],
                []},
            {element,
                <<"script"/utf8>>,
                [nakai@html@attrs:src(
                        <<"https://unpkg.com/htmx.org@1.9.2/dist/ext/ws.js"/utf8>>
                    )],
                []},
            {element,
                <<"script"/utf8>>,
                [nakai@html@attrs:src(
                        <<"https://unpkg.com/idiomorph@0.0.8/dist/idiomorph-ext.min.js"/utf8>>
                    )],
                []}]}.

-spec default_layout(nakai@html:node_(gliew@internal@event:event())) -> nakai@html:node_(gliew@internal@event:event()).
default_layout(Content) ->
    {html, [], [{head, [script()]}, {body, [], [Content]}]}.

-spec new(
    integer(),
    fun((gleam@http@request:request(mist@internal@http:body())) -> response())
) -> server().
new(Port, Handler) ->
    {server, Port, fun default_layout/1, Handler}.

-spec add_headers(gleam@http@response:response(MFW), list({binary(), binary()})) -> gleam@http@response:response(MFW).
add_headers(Response, Headers) ->
    _pipe = Headers,
    gleam@list:fold(_pipe, Response, fun(Res, Pair) -> _pipe@1 = Res,
            gleam@http@response:prepend_header(
                _pipe@1,
                erlang:element(1, Pair),
                erlang:element(2, Pair)
            ) end).

-spec to_mist_response(
    gleam@http@response:response(any()),
    gleam@option:option(binary())
) -> mist@internal@handler:handler_response().
to_mist_response(Response, Body) ->
    case Body of
        {some, Body@1} ->
            _pipe = Response,
            mist:bit_builder_response(
                _pipe,
                gleam@bit_builder:from_string(Body@1)
            );

        none ->
            _pipe@1 = Response,
            mist:empty_response(_pipe@1)
    end.

-spec upgrade_connection(
    gleam@erlang@process:subject(gliew@internal@manager:message()),
    binary(),
    binary()
) -> mist@internal@handler:handler_response().
upgrade_connection(Manager, Session, Csrf) ->
    case gliew@internal@manager:get_worker(Manager, Session, Csrf) of
        {ok, Worker} ->
            _pipe = fun(_, _) -> {ok, nil} end,
            _pipe@1 = mist@websocket:with_handler(_pipe),
            _pipe@2 = mist@websocket:on_init(
                _pipe@1,
                fun(Socket) -> gliew@internal@worker:connect(Worker, Socket) end
            ),
            _pipe@3 = mist@websocket:on_close(
                _pipe@2,
                fun(Socket@1) ->
                    gliew@internal@worker:disconnect(Worker, Socket@1)
                end
            ),
            mist:upgrade(_pipe@3);

        {error, nil} ->
            _pipe@4 = gleam@http@response:new(401),
            mist:empty_response(_pipe@4)
    end.

-spec get_params(list({binary(), binary()})) -> {ok, {binary(), binary()}} |
    {error, nil}.
get_params(Params) ->
    Pmap = gleam@map:from_list(Params),
    gleam@result:then(
        gleam@map:get(Pmap, <<"session"/utf8>>),
        fun(Session) ->
            gleam@result:then(
                gleam@map:get(Pmap, <<"csrf"/utf8>>),
                fun(Csrf) -> {ok, {Session, Csrf}} end
            )
        end
    ).

-spec parse_params(gleam@http@request:request(mist@internal@http:body())) -> {ok,
        {binary(), binary()}} |
    {error, nil}.
parse_params(Req) ->
    case erlang:element(9, Req) of
        {some, Params} ->
            case gleam@uri:parse_query(Params) of
                {ok, Params@1} ->
                    _pipe@1 = gleam@list:map(
                        Params@1,
                        fun(P) ->
                            {erlang:element(1, P),
                                begin
                                    _pipe = erlang:element(2, P),
                                    gleam@string:replace(
                                        _pipe,
                                        <<" "/utf8>>,
                                        <<"+"/utf8>>
                                    )
                                end}
                        end
                    ),
                    get_params(_pipe@1);

                {error, nil} ->
                    {error, nil}
            end;

        none ->
            {error, nil}
    end.

-spec handle_ws_connect(
    gleam@erlang@process:subject(gliew@internal@manager:message()),
    gleam@http@request:request(mist@internal@http:body())
) -> mist@internal@handler:handler_response().
handle_ws_connect(Manager, Req) ->
    case parse_params(Req) of
        {ok, {Id, Csrf}} ->
            upgrade_connection(Manager, Id, Csrf);

        {error, nil} ->
            _pipe = gleam@http@response:new(401),
            mist:empty_response(_pipe)
    end.

-spec handler_func(
    gleam@erlang@process:subject(gliew@internal@manager:message()),
    fun((nakai@html:node_(gliew@internal@event:event())) -> nakai@html:node_(gliew@internal@event:event())),
    fun((gleam@http@request:request(mist@internal@http:body())) -> response())
) -> fun((glisten@handler:handler_message(), glisten@handler:loop_state(mist@internal@handler:state())) -> gleam@otp@actor:next(glisten@handler:loop_state(mist@internal@handler:state()))).
handler_func(Manager, Layout, Handler) ->
    _pipe@7 = fun(Req) ->
        case {erlang:element(2, Req), erlang:element(8, Req)} of
            {get, <<"/connect"/utf8>>} ->
                handle_ws_connect(Manager, Req);

            {_, _} ->
                case Handler(Req) of
                    {response, Status, Headers, Body} ->
                        _pipe = gleam@http@response:new(Status),
                        _pipe@1 = add_headers(_pipe, Headers),
                        to_mist_response(_pipe@1, Body);

                    {view, Status@1, Headers@1, Node} ->
                        _pipe@2 = gleam@http@response:new(Status@1),
                        _pipe@3 = add_headers(_pipe@2, Headers@1),
                        mist:bit_builder_response(
                            _pipe@3,
                            begin
                                _pipe@4 = gliew@internal@manager:process_tree(
                                    Manager,
                                    Req,
                                    Node
                                ),
                                _pipe@5 = Layout(_pipe@4),
                                _pipe@6 = nakai:to_string_builder(_pipe@5),
                                gleam@bit_builder:from_string_builder(_pipe@6)
                            end
                        )
                end
        end
    end,
    mist:handler_func(_pipe@7).

-spec serve(server()) -> {ok, nil} | {error, glisten:start_error()}.
serve(Server) ->
    gleam@result:'try'(
        begin
            _pipe = gliew@internal@manager:start_manager(),
            gleam@result:map_error(_pipe, fun(Err) -> case Err of
                        init_timeout ->
                            acceptor_timeout;

                        {init_failed, Reason} ->
                            {acceptor_failed, Reason};

                        {init_crashed, Any} ->
                            {acceptor_crashed, Any}
                    end end)
        end,
        fun(Manager) ->
            mist:serve(
                erlang:element(2, Server),
                handler_func(
                    Manager,
                    erlang:element(3, Server),
                    erlang:element(4, Server)
                )
            )
        end
    ).
