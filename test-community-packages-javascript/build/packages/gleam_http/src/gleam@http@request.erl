-module(gleam@http@request).
-compile([no_auto_import, nowarn_unused_vars]).

-export([to_uri/1, from_uri/1, get_header/2, set_header/3, prepend_header/3, set_body/2, map/2, path_segments/1, get_query/1, set_query/2, set_method/2, new/0, to/1, set_scheme/2, set_host/2, set_port/2, set_path/2, set_cookie/3, get_cookies/1]).
-export_type([request/1]).

-type request(FBL) :: {request,
        gleam@http:method(),
        list({binary(), binary()}),
        FBL,
        gleam@http:scheme(),
        binary(),
        gleam@option:option(integer()),
        binary(),
        gleam@option:option(binary())}.

-spec to_uri(request(any())) -> gleam@uri:uri().
to_uri(Request) ->
    {uri,
        {some, gleam@http:scheme_to_string(erlang:element(5, Request))},
        none,
        {some, erlang:element(6, Request)},
        erlang:element(7, Request),
        erlang:element(8, Request),
        erlang:element(9, Request),
        none}.

-spec from_uri(gleam@uri:uri()) -> {ok, request(binary())} | {error, nil}.
from_uri(Uri) ->
    gleam@result:then(
        begin
            _pipe = erlang:element(2, Uri),
            _pipe@1 = gleam@option:unwrap(_pipe, <<""/utf8>>),
            gleam@http:scheme_from_string(_pipe@1)
        end,
        fun(Scheme) ->
            gleam@result:then(
                begin
                    _pipe@2 = erlang:element(4, Uri),
                    gleam@option:to_result(_pipe@2, nil)
                end,
                fun(Host) ->
                    Req = {request,
                        get,
                        [],
                        <<""/utf8>>,
                        Scheme,
                        Host,
                        erlang:element(5, Uri),
                        erlang:element(6, Uri),
                        erlang:element(7, Uri)},
                    {ok, Req}
                end
            )
        end
    ).

-spec get_header(request(any()), binary()) -> {ok, binary()} | {error, nil}.
get_header(Request, Key) ->
    gleam@list:key_find(erlang:element(3, Request), gleam@string:lowercase(Key)).

-spec set_header(request(FBV), binary(), binary()) -> request(FBV).
set_header(Request, Key, Value) ->
    Headers = gleam@list:key_set(
        erlang:element(3, Request),
        gleam@string:lowercase(Key),
        Value
    ),
    erlang:setelement(3, Request, Headers).

-spec prepend_header(request(FBY), binary(), binary()) -> request(FBY).
prepend_header(Request, Key, Value) ->
    Headers = [{gleam@string:lowercase(Key), Value} |
        erlang:element(3, Request)],
    erlang:setelement(3, Request, Headers).

-spec set_body(request(any()), FCD) -> request(FCD).
set_body(Req, Body) ->
    {request, Method, Headers, _, Scheme, Host, Port, Path, Query} = Req,
    {request, Method, Headers, Body, Scheme, Host, Port, Path, Query}.

-spec map(request(FCF), fun((FCF) -> FCH)) -> request(FCH).
map(Request, Transform) ->
    _pipe = erlang:element(4, Request),
    _pipe@1 = Transform(_pipe),
    set_body(Request, _pipe@1).

-spec path_segments(request(any())) -> list(binary()).
path_segments(Request) ->
    _pipe = erlang:element(8, Request),
    gleam@uri:path_segments(_pipe).

-spec get_query(request(any())) -> {ok, list({binary(), binary()})} |
    {error, nil}.
get_query(Request) ->
    case erlang:element(9, Request) of
        {some, Query_string} ->
            gleam@uri:parse_query(Query_string);

        none ->
            {ok, []}
    end.

-spec set_query(request(FCR), list({binary(), binary()})) -> request(FCR).
set_query(Req, Query) ->
    Pair = fun(T) ->
        gleam@string_builder:from_strings(
            [erlang:element(1, T), <<"="/utf8>>, erlang:element(2, T)]
        )
    end,
    Query@1 = begin
        _pipe = Query,
        _pipe@1 = gleam@list:map(_pipe, Pair),
        _pipe@2 = gleam@list:intersperse(
            _pipe@1,
            gleam@string_builder:from_string(<<"&"/utf8>>)
        ),
        _pipe@3 = gleam@string_builder:concat(_pipe@2),
        _pipe@4 = gleam@string_builder:to_string(_pipe@3),
        {some, _pipe@4}
    end,
    erlang:setelement(9, Req, Query@1).

-spec set_method(request(FCV), gleam@http:method()) -> request(FCV).
set_method(Req, Method) ->
    erlang:setelement(2, Req, Method).

-spec new() -> request(binary()).
new() ->
    {request,
        get,
        [],
        <<""/utf8>>,
        https,
        <<"localhost"/utf8>>,
        none,
        <<""/utf8>>,
        none}.

-spec to(binary()) -> {ok, request(binary())} | {error, nil}.
to(Url) ->
    _pipe = Url,
    _pipe@1 = gleam@uri:parse(_pipe),
    gleam@result:then(_pipe@1, fun from_uri/1).

-spec set_scheme(request(FDC), gleam@http:scheme()) -> request(FDC).
set_scheme(Req, Scheme) ->
    erlang:setelement(5, Req, Scheme).

-spec set_host(request(FDF), binary()) -> request(FDF).
set_host(Req, Host) ->
    erlang:setelement(6, Req, Host).

-spec set_port(request(FDI), integer()) -> request(FDI).
set_port(Req, Port) ->
    erlang:setelement(7, Req, {some, Port}).

-spec set_path(request(FDL), binary()) -> request(FDL).
set_path(Req, Path) ->
    erlang:setelement(8, Req, Path).

-spec set_cookie(request(FDO), binary(), binary()) -> request(FDO).
set_cookie(Req, Name, Value) ->
    New_cookie_string = gleam@string:join([Name, Value], <<"="/utf8>>),
    {Cookies_string@2, Headers@1} = case gleam@list:key_pop(
        erlang:element(3, Req),
        <<"cookie"/utf8>>
    ) of
        {ok, {Cookies_string, Headers}} ->
            Cookies_string@1 = gleam@string:join(
                [Cookies_string, New_cookie_string],
                <<"; "/utf8>>
            ),
            {Cookies_string@1, Headers};

        {error, nil} ->
            {New_cookie_string, erlang:element(3, Req)}
    end,
    erlang:setelement(
        3,
        Req,
        [{<<"cookie"/utf8>>, Cookies_string@2} | Headers@1]
    ).

-spec get_cookies(request(any())) -> list({binary(), binary()}).
get_cookies(Req) ->
    {request, _, Headers, _, _, _, _, _, _} = Req,
    _pipe = Headers,
    _pipe@1 = gleam@list:filter_map(
        _pipe,
        fun(Header) ->
            {Name, Value} = Header,
            case Name of
                <<"cookie"/utf8>> ->
                    {ok, gleam@http@cookie:parse(Value)};

                _ ->
                    {error, nil}
            end
        end
    ),
    gleam@list:flatten(_pipe@1).
