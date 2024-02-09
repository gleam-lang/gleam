-module(mist@internal@http).
-compile([no_auto_import, nowarn_unused_vars]).

-export([from_header/1, upgrade_socket/1, add_default_headers/1, upgrade/3, parse_headers/4, parse_request/3, read_data/4, read_body/1]).
-export_type([packet_type/0, http_uri/0, http_packet/0, decoded_packet/0, decode_error/0, buffer/0, body/0, http_response_body/0]).

-type packet_type() :: http | httph_bin | http_bin.

-type http_uri() :: {abs_path, bitstring()}.

-type http_packet() :: {http_request,
        gleam@dynamic:dynamic(),
        http_uri(),
        {integer(), integer()}} |
    {http_header,
        integer(),
        gleam@erlang@atom:atom_(),
        bitstring(),
        bitstring()}.

-type decoded_packet() :: {binary_data, http_packet(), bitstring()} |
    {end_of_headers, bitstring()} |
    {more_data, gleam@option:option(integer())}.

-type decode_error() :: malformed_request |
    invalid_method |
    invalid_path |
    unknown_header |
    unknown_method |
    invalid_body |
    discard_packet.

-type buffer() :: {buffer, integer(), bitstring()}.

-opaque body() :: {unread, bitstring(), glisten@socket:socket()} |
    {read, bitstring()}.

-type http_response_body() :: {bit_builder_body,
        gleam@bit_builder:bit_builder()} |
    {chunked, gleam@iterator:iterator(gleam@bit_builder:bit_builder())} |
    {file_body,
        mist@internal@file:file_descriptor(),
        binary(),
        integer(),
        integer()}.

-spec from_header(bitstring()) -> binary().
from_header(Value) ->
    _assert_subject = gleam@bit_string:to_string(Value),
    {ok, Value@1} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"mist/internal/http"/utf8>>,
                        function => <<"from_header"/utf8>>,
                        line => 64})
    end,
    gleam@string:lowercase(Value@1).

-spec upgrade_socket(gleam@http@request:request(body())) -> {ok,
        gleam@http@response:response(gleam@bit_builder:bit_builder())} |
    {error, gleam@http@request:request(body())}.
upgrade_socket(Req) ->
    gleam@result:then(
        begin
            _pipe = gleam@http@request:get_header(Req, <<"upgrade"/utf8>>),
            gleam@result:replace_error(_pipe, Req)
        end,
        fun(_) ->
            gleam@result:then(
                begin
                    _pipe@1 = gleam@http@request:get_header(
                        Req,
                        <<"sec-websocket-key"/utf8>>
                    ),
                    gleam@result:replace_error(_pipe@1, Req)
                end,
                fun(Key) ->
                    gleam@result:then(
                        begin
                            _pipe@2 = gleam@http@request:get_header(
                                Req,
                                <<"sec-websocket-version"/utf8>>
                            ),
                            gleam@result:replace_error(_pipe@2, Req)
                        end,
                        fun(_) ->
                            Accept_key = mist@internal@websocket:parse_key(Key),
                            _pipe@3 = gleam@http@response:new(101),
                            _pipe@4 = gleam@http@response:set_body(
                                _pipe@3,
                                gleam@bit_builder:new()
                            ),
                            _pipe@5 = gleam@http@response:prepend_header(
                                _pipe@4,
                                <<"Upgrade"/utf8>>,
                                <<"websocket"/utf8>>
                            ),
                            _pipe@6 = gleam@http@response:prepend_header(
                                _pipe@5,
                                <<"Connection"/utf8>>,
                                <<"Upgrade"/utf8>>
                            ),
                            _pipe@7 = gleam@http@response:prepend_header(
                                _pipe@6,
                                <<"Sec-WebSocket-Accept"/utf8>>,
                                Accept_key
                            ),
                            {ok, _pipe@7}
                        end
                    )
                end
            )
        end
    ).

-spec add_default_headers(
    gleam@http@response:response(gleam@bit_builder:bit_builder())
) -> gleam@http@response:response(gleam@bit_builder:bit_builder()).
add_default_headers(Resp) ->
    Body_size = gleam@bit_builder:byte_size(erlang:element(4, Resp)),
    Headers = begin
        _pipe = gleam@map:from_list(
            [{<<"content-length"/utf8>>, gleam@int:to_string(Body_size)},
                {<<"connection"/utf8>>, <<"keep-alive"/utf8>>}]
        ),
        _pipe@1 = gleam@list:fold(
            erlang:element(3, Resp),
            _pipe,
            fun(Defaults, Tup) ->
                {Key, Value} = Tup,
                gleam@map:insert(Defaults, Key, Value)
            end
        ),
        gleam@map:to_list(_pipe@1)
    end,
    erlang:setelement(3, Resp, Headers).

-spec upgrade(
    glisten@socket:socket(),
    glisten@socket@transport:transport(),
    gleam@http@request:request(body())
) -> {ok, nil} | {error, nil}.
upgrade(Socket, Transport, Req) ->
    gleam@result:then(
        begin
            _pipe = upgrade_socket(Req),
            gleam@result:nil_error(_pipe)
        end,
        fun(Resp) ->
            gleam@result:then(
                begin
                    _pipe@1 = Resp,
                    _pipe@2 = add_default_headers(_pipe@1),
                    _pipe@3 = mist@internal@encoder:to_bit_builder(_pipe@2),
                    _pipe@4 = (erlang:element(11, Transport))(Socket, _pipe@3),
                    gleam@result:nil_error(_pipe@4)
                end,
                fun(_) -> {ok, nil} end
            )
        end
    ).

-spec is_continue(gleam@http@request:request(body())) -> boolean().
is_continue(Req) ->
    _pipe = erlang:element(3, Req),
    _pipe@1 = gleam@list:find(
        _pipe,
        fun(Tup) ->
            (gleam@pair:first(Tup) =:= <<"expect"/utf8>>) andalso (gleam@pair:second(
                Tup
            )
            =:= <<"100-continue"/utf8>>)
        end
    ),
    gleam@result:is_ok(_pipe@1).

-spec parse_headers(
    bitstring(),
    glisten@socket:socket(),
    glisten@socket@transport:transport(),
    gleam@map:map_(binary(), binary())
) -> {ok, {gleam@map:map_(binary(), binary()), bitstring()}} |
    {error, decode_error()}.
parse_headers(Bs, Socket, Transport, Headers) ->
    case mist_ffi:decode_packet(httph_bin, Bs, []) of
        {ok, {binary_data, {http_header, _, _, Field, Value}, Rest}} ->
            Field@1 = from_header(Field),
            _assert_subject = gleam@bit_string:to_string(Value),
            {ok, Value@1} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"mist/internal/http"/utf8>>,
                                function => <<"parse_headers"/utf8>>,
                                line => 82})
            end,
            _pipe = Headers,
            _pipe@1 = gleam@map:insert(_pipe, Field@1, Value@1),
            parse_headers(Rest, Socket, Transport, _pipe@1);

        {ok, {end_of_headers, Rest@1}} ->
            {ok, {Headers, Rest@1}};

        {ok, {more_data, Size}} ->
            Amount_to_read = gleam@option:unwrap(Size, 0),
            gleam@result:then(
                read_data(
                    Socket,
                    Transport,
                    {buffer, Amount_to_read, Bs},
                    unknown_header
                ),
                fun(Next) -> parse_headers(Next, Socket, Transport, Headers) end
            );

        _ ->
            {error, unknown_header}
    end.

-spec parse_request(
    bitstring(),
    glisten@socket:socket(),
    glisten@socket@transport:transport()
) -> {ok, gleam@http@request:request(body())} | {error, decode_error()}.
parse_request(Bs, Socket, Transport) ->
    case mist_ffi:decode_packet(http_bin, Bs, []) of
        {ok,
            {binary_data,
                {http_request, Http_method, {abs_path, Path}, _},
                Rest}} ->
            gleam@result:then(
                begin
                    _pipe = Http_method,
                    _pipe@1 = gleam_erlang_ffi:atom_from_dynamic(_pipe),
                    _pipe@2 = gleam@result:map(
                        _pipe@1,
                        fun gleam@erlang@atom:to_string/1
                    ),
                    _pipe@3 = gleam@result:'or'(
                        _pipe@2,
                        gleam@dynamic:string(Http_method)
                    ),
                    _pipe@4 = gleam@result:nil_error(_pipe@3),
                    _pipe@5 = gleam@result:then(
                        _pipe@4,
                        fun gleam@http:parse_method/1
                    ),
                    gleam@result:replace_error(_pipe@5, unknown_method)
                end,
                fun(Method) ->
                    gleam@result:then(
                        parse_headers(Rest, Socket, Transport, gleam@map:new()),
                        fun(_use0) ->
                            {Headers, Rest@1} = _use0,
                            gleam@result:then(
                                begin
                                    _pipe@6 = Path,
                                    _pipe@7 = gleam@bit_string:to_string(
                                        _pipe@6
                                    ),
                                    gleam@result:replace_error(
                                        _pipe@7,
                                        invalid_path
                                    )
                                end,
                                fun(Path@1) ->
                                    {Path@4, Query@1} = case gleam@string:split(
                                        Path@1,
                                        <<"?"/utf8>>
                                    ) of
                                        [Path@2] ->
                                            {Path@2, []};

                                        [Path@3, Query_string] ->
                                            Query = begin
                                                _pipe@8 = Query_string,
                                                _pipe@9 = gleam@uri:parse_query(
                                                    _pipe@8
                                                ),
                                                gleam@result:unwrap(_pipe@9, [])
                                            end,
                                            {Path@3, Query}
                                    end,
                                    Req = begin
                                        _pipe@10 = gleam@http@request:new(),
                                        _pipe@11 = gleam@http@request:set_scheme(
                                            _pipe@10,
                                            case Transport of
                                                {ssl,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _} ->
                                                    https;

                                                {tcp,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _,
                                                    _} ->
                                                    http
                                            end
                                        ),
                                        _pipe@12 = gleam@http@request:set_body(
                                            _pipe@11,
                                            {unread, Rest@1, Socket}
                                        ),
                                        _pipe@13 = gleam@http@request:set_method(
                                            _pipe@12,
                                            Method
                                        ),
                                        _pipe@14 = gleam@http@request:set_path(
                                            _pipe@13,
                                            Path@4
                                        ),
                                        gleam@http@request:set_query(
                                            _pipe@14,
                                            Query@1
                                        )
                                    end,
                                    {ok,
                                        erlang:setelement(
                                            3,
                                            Req,
                                            gleam@map:to_list(Headers)
                                        )}
                                end
                            )
                        end
                    )
                end
            );

        _ ->
            {error, discard_packet}
    end.

-spec read_data(
    glisten@socket:socket(),
    glisten@socket@transport:transport(),
    buffer(),
    decode_error()
) -> {ok, bitstring()} | {error, decode_error()}.
read_data(Socket, Transport, Buffer, Error) ->
    To_read = gleam@int:min(erlang:element(2, Buffer), 1000000),
    Timeout = 15000,
    gleam@result:then(
        begin
            _pipe = Socket,
            _pipe@1 = (erlang:element(10, Transport))(_pipe, To_read, Timeout),
            gleam@result:replace_error(_pipe@1, Error)
        end,
        fun(Data) ->
            Next_buffer = {buffer,
                erlang:element(2, Buffer) - To_read,
                <<(erlang:element(3, Buffer))/bitstring, Data/bitstring>>},
            case erlang:element(2, Next_buffer) > 0 of
                true ->
                    read_data(Socket, Transport, Next_buffer, Error);

                false ->
                    {ok, erlang:element(3, Next_buffer)}
            end
        end
    ).

-spec read_chunk(
    glisten@socket:socket(),
    glisten@socket@transport:transport(),
    buffer(),
    gleam@bit_builder:bit_builder()
) -> {ok, gleam@bit_builder:bit_builder()} | {error, decode_error()}.
read_chunk(Socket, Transport, Buffer, Body) ->
    case {erlang:element(3, Buffer),
        mist_ffi:binary_match(
            erlang:element(3, Buffer),
            <<13/integer, 10/integer>>
        )} of
        {_, {ok, {Offset, _}}} ->
            _assert_subject = erlang:element(3, Buffer),
            <<Chunk:Offset/binary, _/integer, _/integer, Rest/binary>> = case _assert_subject of
                <<_:Offset/binary, _/integer, _/integer, _/binary>> -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"mist/internal/http"/utf8>>,
                                function => <<"read_chunk"/utf8>>,
                                line => 147})
            end,
            gleam@result:then(
                begin
                    _pipe = Chunk,
                    _pipe@1 = gleam@bit_string:to_string(_pipe),
                    _pipe@2 = gleam@result:map(
                        _pipe@1,
                        fun gleam@erlang@charlist:from_string/1
                    ),
                    gleam@result:replace_error(_pipe@2, invalid_body)
                end,
                fun(Chunk_size) ->
                    gleam@result:then(
                        begin
                            _pipe@3 = mist_ffi:string_to_int(Chunk_size, 16),
                            gleam@result:replace_error(_pipe@3, invalid_body)
                        end,
                        fun(Size) -> case Size of
                                0 ->
                                    {ok, Body};

                                Size@1 ->
                                    case Rest of
                                        <<Next_chunk:Size@1/binary,
                                            13/integer,
                                            10/integer,
                                            Rest@1/binary>> ->
                                            read_chunk(
                                                Socket,
                                                Transport,
                                                {buffer, 0, Rest@1},
                                                gleam@bit_builder:append(
                                                    Body,
                                                    Next_chunk
                                                )
                                            );

                                        _ ->
                                            gleam@result:then(
                                                read_data(
                                                    Socket,
                                                    Transport,
                                                    {buffer,
                                                        0,
                                                        erlang:element(
                                                            3,
                                                            Buffer
                                                        )},
                                                    invalid_body
                                                ),
                                                fun(Next) ->
                                                    read_chunk(
                                                        Socket,
                                                        Transport,
                                                        {buffer, 0, Next},
                                                        Body
                                                    )
                                                end
                                            )
                                    end
                            end end
                    )
                end
            );

        {<<>>, _} ->
            gleam@result:then(
                read_data(
                    Socket,
                    Transport,
                    {buffer, 0, erlang:element(3, Buffer)},
                    invalid_body
                ),
                fun(Next@1) ->
                    read_chunk(Socket, Transport, {buffer, 0, Next@1}, Body)
                end
            );

        {_, {error, nil}} ->
            {error, invalid_body}
    end.

-spec read_body(gleam@http@request:request(body())) -> {ok,
        gleam@http@request:request(bitstring())} |
    {error, decode_error()}.
read_body(Req) ->
    Transport = case erlang:element(5, Req) of
        https ->
            glisten@socket@transport:ssl();

        http ->
            glisten@socket@transport:tcp()
    end,
    case {gleam@http@request:get_header(Req, <<"transfer-encoding"/utf8>>),
        erlang:element(4, Req)} of
        {{ok, <<"chunked"/utf8>>}, {unread, Rest, Socket}} ->
            gleam@result:then(
                read_chunk(
                    Socket,
                    Transport,
                    {buffer, 0, Rest},
                    gleam@bit_builder:new()
                ),
                fun(Chunk) ->
                    {ok,
                        gleam@http@request:set_body(
                            Req,
                            gleam@bit_builder:to_bit_string(Chunk)
                        )}
                end
            );

        {_, {unread, Rest@1, Socket@1}} ->
            _ = case is_continue(Req) of
                true ->
                    _assert_subject = begin
                        _pipe = gleam@http@response:new(100),
                        _pipe@1 = gleam@http@response:set_body(
                            _pipe,
                            gleam@bit_builder:new()
                        ),
                        _pipe@2 = mist@internal@encoder:to_bit_builder(_pipe@1),
                        (erlang:element(11, Transport))(Socket@1, _pipe@2)
                    end,
                    {ok, nil} = case _assert_subject of
                        {ok, nil} -> _assert_subject;
                        _assert_fail ->
                            erlang:error(#{gleam_error => assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail,
                                        module => <<"mist/internal/http"/utf8>>,
                                        function => <<"read_body"/utf8>>,
                                        line => 276})
                    end,
                    nil;

                false ->
                    nil
            end,
            Body_size = begin
                _pipe@3 = erlang:element(3, Req),
                _pipe@4 = gleam@list:find(
                    _pipe@3,
                    fun(Tup) ->
                        gleam@pair:first(Tup) =:= <<"content-length"/utf8>>
                    end
                ),
                _pipe@5 = gleam@result:map(_pipe@4, fun gleam@pair:second/1),
                _pipe@6 = gleam@result:then(_pipe@5, fun gleam@int:parse/1),
                gleam@result:unwrap(_pipe@6, 0)
            end,
            Remaining = Body_size - gleam@bit_string:byte_size(Rest@1),
            _pipe@7 = case {Body_size, Remaining} of
                {0, 0} ->
                    {ok, <<>>};

                {0, _} ->
                    {ok, Rest@1};

                {_, 0} ->
                    {ok, Rest@1};

                {_, _} ->
                    read_data(
                        Socket@1,
                        Transport,
                        {buffer, Remaining, Rest@1},
                        invalid_body
                    )
            end,
            _pipe@8 = gleam@result:map(
                _pipe@7,
                fun(_capture) -> gleam@http@request:set_body(Req, _capture) end
            ),
            gleam@result:replace_error(_pipe@8, invalid_body);

        {_, {read, _}} ->
            {error, invalid_body}
    end.
