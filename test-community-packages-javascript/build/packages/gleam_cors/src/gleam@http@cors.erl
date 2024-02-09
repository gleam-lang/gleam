-module(gleam@http@cors).
-compile([no_auto_import, nowarn_unused_vars]).

-export([middleware/3]).
-export_type([config/0, allowed_origins/0]).

-type config() :: {config,
        allowed_origins(),
        gleam@set:set(gleam@http:method()),
        gleam@set:set(binary())}.

-type allowed_origins() :: allow_all | {allow_some, gleam@set:set(binary())}.

-spec parse_allowed_origins(list(binary())) -> {ok, allowed_origins()} |
    {error, nil}.
parse_allowed_origins(L) ->
    case {gleam@list:contains(L, <<"*"/utf8>>), L} of
        {true, _} ->
            {ok, allow_all};

        {_, Origins} ->
            Origins_set = begin
                _pipe = Origins,
                _pipe@1 = gleam@list:map(_pipe, fun gleam@string:lowercase/1),
                _pipe@2 = gleam@set:from_list(_pipe@1),
                gleam@set:delete(_pipe@2, <<""/utf8>>)
            end,
            case gleam@set:size(Origins_set) of
                0 ->
                    {error, nil};

                _ ->
                    _pipe@3 = {allow_some, Origins_set},
                    {ok, _pipe@3}
            end
    end.

-spec parse_allowed_methods(list(gleam@http:method())) -> {ok,
        gleam@set:set(gleam@http:method())} |
    {error, nil}.
parse_allowed_methods(L) ->
    Methods_set = gleam@set:from_list(L),
    case gleam@set:size(Methods_set) of
        0 ->
            {error, nil};

        _ ->
            {ok, Methods_set}
    end.

-spec parse_allowed_headers(list(binary())) -> {ok, gleam@set:set(binary())} |
    {error, nil}.
parse_allowed_headers(L) ->
    Headers_set = begin
        _pipe = L,
        _pipe@1 = gleam@list:map(_pipe, fun gleam@string:lowercase/1),
        gleam@set:from_list(_pipe@1)
    end,
    {ok, Headers_set}.

-spec parse_config(list(binary()), list(gleam@http:method()), list(binary())) -> {ok,
        config()} |
    {error, nil}.
parse_config(Allowed_origins, Allowed_methods, Allowed_headers) ->
    gleam@result:'try'(
        parse_allowed_origins(Allowed_origins),
        fun(Allowed_origins@1) ->
            gleam@result:'try'(
                parse_allowed_methods(Allowed_methods),
                fun(Allowed_methods@1) ->
                    gleam@result:'try'(
                        parse_allowed_headers(Allowed_headers),
                        fun(Allowed_headers@1) ->
                            _pipe = {config,
                                Allowed_origins@1,
                                Allowed_methods@1,
                                Allowed_headers@1},
                            {ok, _pipe}
                        end
                    )
                end
            )
        end
    ).

-spec get_origin(gleam@http@request:request(any())) -> binary().
get_origin(Request) ->
    _pipe = gleam@http@request:get_header(Request, <<"origin"/utf8>>),
    gleam@result:unwrap(_pipe, <<""/utf8>>).

-spec is_origin_allowed(binary(), allowed_origins()) -> boolean().
is_origin_allowed(Origin, Allowed_origins) ->
    case Allowed_origins of
        allow_all ->
            true;

        {allow_some, Origins} ->
            gleam@set:contains(Origins, gleam@string:lowercase(Origin))
    end.

-spec is_method_allowed(gleam@http:method(), gleam@set:set(gleam@http:method())) -> boolean().
is_method_allowed(Method, Allowed_methods) ->
    gleam@set:contains(Allowed_methods, Method).

-spec are_headers_allowed(list(binary()), gleam@set:set(binary())) -> boolean().
are_headers_allowed(Request_headers, Allowed_headers) ->
    gleam@list:all(
        Request_headers,
        fun(Header) ->
            gleam@set:contains(Allowed_headers, gleam@string:lowercase(Header))
        end
    ).

-spec prepend_allow_origin_header(
    gleam@http@response:response(gleam@bit_builder:bit_builder()),
    binary(),
    allowed_origins()
) -> gleam@http@response:response(gleam@bit_builder:bit_builder()).
prepend_allow_origin_header(Response, Origin, Allowed_origins) ->
    case Allowed_origins of
        allow_all ->
            _pipe = Response,
            gleam@http@response:prepend_header(
                _pipe,
                <<"Access-Control-Allow-Origin"/utf8>>,
                <<"*"/utf8>>
            );

        {allow_some, _} ->
            _pipe@1 = Response,
            _pipe@2 = gleam@http@response:prepend_header(
                _pipe@1,
                <<"Access-Control-Allow-Origin"/utf8>>,
                Origin
            ),
            gleam@http@response:prepend_header(
                _pipe@2,
                <<"Vary"/utf8>>,
                <<"Origin"/utf8>>
            )
    end.

-spec handle_other_request(
    fun((gleam@http@request:request(FTP)) -> gleam@http@response:response(gleam@bit_builder:bit_builder())),
    gleam@http@request:request(FTP),
    allowed_origins()
) -> gleam@http@response:response(gleam@bit_builder:bit_builder()).
handle_other_request(Service, Request, Allowed_origins) ->
    Origin = get_origin(Request),
    Response = Service(Request),
    case is_origin_allowed(Origin, Allowed_origins) of
        true ->
            _pipe = Response,
            prepend_allow_origin_header(_pipe, Origin, Allowed_origins);

        false ->
            Response
    end.

-spec prepend_allow_headers_header(
    gleam@http@response:response(gleam@bit_builder:bit_builder()),
    list(binary())
) -> gleam@http@response:response(gleam@bit_builder:bit_builder()).
prepend_allow_headers_header(Response, Headers) ->
    case gleam@list:length(Headers) of
        0 ->
            Response;

        _ ->
            _pipe = gleam@string:join(Headers, <<", "/utf8>>),
            gleam@http@response:prepend_header(
                Response,
                <<"Access-Control-Allow-Headers"/utf8>>,
                _pipe
            )
    end.

-spec handle_options_request(gleam@http@request:request(any()), config()) -> gleam@http@response:response(gleam@bit_builder:bit_builder()).
handle_options_request(Request, Config) ->
    Response = begin
        _pipe = gleam@http@response:new(200),
        gleam@http@response:set_body(_pipe, gleam@bit_builder:new())
    end,
    Origin = get_origin(Request),
    Ac_request_method = begin
        _pipe@1 = gleam@http@request:get_header(
            Request,
            <<"Access-Control-Request-Method"/utf8>>
        ),
        _pipe@2 = gleam@result:then(_pipe@1, fun gleam@http:parse_method/1),
        gleam@result:unwrap(_pipe@2, {other, <<""/utf8>>})
    end,
    Ac_request_headers = begin
        _pipe@3 = gleam@http@request:get_header(
            Request,
            <<"Access-Control-Request-Headers"/utf8>>
        ),
        _pipe@4 = gleam@result:map(
            _pipe@3,
            fun(_capture) -> gleam@string:split(_capture, <<", "/utf8>>) end
        ),
        gleam@result:unwrap(_pipe@4, [])
    end,
    Is_request_allowed = (is_origin_allowed(Origin, erlang:element(2, Config))
    andalso is_method_allowed(Ac_request_method, erlang:element(3, Config)))
    andalso are_headers_allowed(Ac_request_headers, erlang:element(4, Config)),
    case Is_request_allowed of
        true ->
            _pipe@5 = Response,
            _pipe@6 = prepend_allow_origin_header(
                _pipe@5,
                Origin,
                erlang:element(2, Config)
            ),
            prepend_allow_headers_header(_pipe@6, Ac_request_headers);

        false ->
            Response
    end.

-spec middleware_from_config(config()) -> fun((fun((gleam@http@request:request(FTH)) -> gleam@http@response:response(gleam@bit_builder:bit_builder()))) -> fun((gleam@http@request:request(FTH)) -> gleam@http@response:response(gleam@bit_builder:bit_builder()))).
middleware_from_config(Config) ->
    fun(Service) -> fun(Request) -> case erlang:element(2, Request) of
                options ->
                    handle_options_request(Request, Config);

                _ ->
                    handle_other_request(
                        Service,
                        Request,
                        erlang:element(2, Config)
                    )
            end end end.

-spec middleware(list(binary()), list(gleam@http:method()), list(binary())) -> fun((fun((gleam@http@request:request(FTC)) -> gleam@http@response:response(gleam@bit_builder:bit_builder()))) -> fun((gleam@http@request:request(FTC)) -> gleam@http@response:response(gleam@bit_builder:bit_builder()))).
middleware(Allowed_origins, Allowed_methods, Allowed_headers) ->
    case parse_config(Allowed_origins, Allowed_methods, Allowed_headers) of
        {ok, Config} ->
            middleware_from_config(Config);

        {error, _} ->
            fun gleam@function:identity/1
    end.
