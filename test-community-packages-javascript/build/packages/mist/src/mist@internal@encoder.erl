-module(mist@internal@encoder).
-compile([no_auto_import, nowarn_unused_vars]).

-export([status_to_bit_string/1, encode_headers/1, response_builder/2, to_bit_builder/1]).

-spec status_to_bit_string(integer()) -> bitstring().
status_to_bit_string(Status) ->
    case Status of
        100 ->
            <<"Continue"/utf8>>;

        101 ->
            <<"Switching Protocols"/utf8>>;

        103 ->
            <<"Early Hints"/utf8>>;

        200 ->
            <<"OK"/utf8>>;

        201 ->
            <<"Created"/utf8>>;

        202 ->
            <<"Accepted"/utf8>>;

        203 ->
            <<"Non-Authoritative Information"/utf8>>;

        204 ->
            <<"No Content"/utf8>>;

        205 ->
            <<"Reset Content"/utf8>>;

        206 ->
            <<"Partial Content"/utf8>>;

        300 ->
            <<"Multiple Choices"/utf8>>;

        301 ->
            <<"Moved Permanently"/utf8>>;

        302 ->
            <<"Found"/utf8>>;

        303 ->
            <<"See Other"/utf8>>;

        304 ->
            <<"Not Modified"/utf8>>;

        307 ->
            <<"Temporary Redirect"/utf8>>;

        308 ->
            <<"Permanent Redirect"/utf8>>;

        400 ->
            <<"Bad Request"/utf8>>;

        401 ->
            <<"Unauthorized"/utf8>>;

        402 ->
            <<"Payment Required"/utf8>>;

        403 ->
            <<"Forbidden"/utf8>>;

        404 ->
            <<"Not Found"/utf8>>;

        405 ->
            <<"Method Not Allowed"/utf8>>;

        406 ->
            <<"Not Acceptable"/utf8>>;

        407 ->
            <<"Proxy Authentication Required"/utf8>>;

        408 ->
            <<"Request Timeout"/utf8>>;

        409 ->
            <<"Conflict"/utf8>>;

        410 ->
            <<"Gone"/utf8>>;

        411 ->
            <<"Length Required"/utf8>>;

        412 ->
            <<"Precondition Failed"/utf8>>;

        413 ->
            <<"Payload Too Large"/utf8>>;

        414 ->
            <<"URI Too Long"/utf8>>;

        415 ->
            <<"Unsupported Media Type"/utf8>>;

        416 ->
            <<"Range Not Satisfiable"/utf8>>;

        417 ->
            <<"Expectation Failed"/utf8>>;

        418 ->
            <<"I'm a teapot"/utf8>>;

        422 ->
            <<"Unprocessable Entity"/utf8>>;

        425 ->
            <<"Too Early"/utf8>>;

        426 ->
            <<"Upgrade Required"/utf8>>;

        428 ->
            <<"Precondition Required"/utf8>>;

        429 ->
            <<"Too Many Requests"/utf8>>;

        431 ->
            <<"Request Header Fields Too Large"/utf8>>;

        451 ->
            <<"Unavailable For Legal Reasons"/utf8>>;

        500 ->
            <<"Internal Server Error"/utf8>>;

        501 ->
            <<"Not Implemented"/utf8>>;

        502 ->
            <<"Bad Gateway"/utf8>>;

        503 ->
            <<"Service Unavailable"/utf8>>;

        504 ->
            <<"Gateway Timeout"/utf8>>;

        505 ->
            <<"HTTP Version Not Supported"/utf8>>;

        506 ->
            <<"Variant Also Negotiates"/utf8>>;

        507 ->
            <<"Insufficient Storage"/utf8>>;

        508 ->
            <<"Loop Detected"/utf8>>;

        510 ->
            <<"Not Extended"/utf8>>;

        511 ->
            <<"Network Authentication Required"/utf8>>
    end.

-spec encode_headers(list({binary(), binary()})) -> gleam@bit_builder:bit_builder().
encode_headers(Headers) ->
    gleam@list:fold(
        Headers,
        gleam@bit_builder:new(),
        fun(Builder, Tup) ->
            {Header, Value} = Tup,
            _pipe = Builder,
            _pipe@1 = gleam@bit_builder:append_string(_pipe, Header),
            _pipe@2 = gleam@bit_builder:append(_pipe@1, <<": "/utf8>>),
            _pipe@3 = gleam@bit_builder:append_string(_pipe@2, Value),
            gleam@bit_builder:append(_pipe@3, <<"\r\n"/utf8>>)
        end
    ).

-spec response_builder(integer(), list({binary(), binary()})) -> gleam@bit_builder:bit_builder().
response_builder(Status, Headers) ->
    Status_string = begin
        _pipe = Status,
        _pipe@1 = gleam@int:to_string(_pipe),
        _pipe@2 = gleam@bit_builder:from_string(_pipe@1),
        _pipe@3 = gleam@bit_builder:append(_pipe@2, <<" "/utf8>>),
        gleam@bit_builder:append(_pipe@3, status_to_bit_string(Status))
    end,
    _pipe@4 = gleam@bit_builder:new(),
    _pipe@5 = gleam@bit_builder:append(_pipe@4, <<"HTTP/1.1 "/utf8>>),
    _pipe@6 = gleam@bit_builder:append_builder(_pipe@5, Status_string),
    _pipe@7 = gleam@bit_builder:append(_pipe@6, <<"\r\n"/utf8>>),
    _pipe@8 = gleam@bit_builder:append_builder(_pipe@7, encode_headers(Headers)),
    gleam@bit_builder:append(_pipe@8, <<"\r\n"/utf8>>).

-spec to_bit_builder(
    gleam@http@response:response(gleam@bit_builder:bit_builder())
) -> gleam@bit_builder:bit_builder().
to_bit_builder(Resp) ->
    _pipe = erlang:element(2, Resp),
    _pipe@1 = response_builder(_pipe, erlang:element(3, Resp)),
    gleam@bit_builder:append_builder(_pipe@1, erlang:element(4, Resp)).
