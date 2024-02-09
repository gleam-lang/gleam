-module(mist).
-compile([no_auto_import, nowarn_unused_vars]).

-export([run_service/3, run_service_ssl/5, serve/2, serve_ssl/4, handler_func/1, read_body/1, upgrade/1, empty_response/1, bit_builder_response/2, file_response/3, chunked_response/2]).

-spec run_service(
    integer(),
    fun((gleam@http@request:request(bitstring())) -> gleam@http@response:response(gleam@bit_builder:bit_builder())),
    integer()
) -> {ok, nil} | {error, glisten:start_error()}.
run_service(Port, Handler, Max_body_limit) ->
    _pipe = Handler,
    _pipe@1 = mist@internal@handler:with(_pipe, Max_body_limit),
    _pipe@2 = glisten@acceptor:new_pool_with_data(
        _pipe@1,
        mist@internal@handler:new_state()
    ),
    glisten:serve(Port, _pipe@2).

-spec run_service_ssl(
    integer(),
    binary(),
    binary(),
    fun((gleam@http@request:request(bitstring())) -> gleam@http@response:response(gleam@bit_builder:bit_builder())),
    integer()
) -> {ok, nil} | {error, glisten:start_error()}.
run_service_ssl(Port, Certfile, Keyfile, Handler, Max_body_limit) ->
    _pipe = Handler,
    _pipe@1 = mist@internal@handler:with(_pipe, Max_body_limit),
    _pipe@2 = glisten@acceptor:new_pool_with_data(
        _pipe@1,
        mist@internal@handler:new_state()
    ),
    glisten:serve_ssl(Port, Certfile, Keyfile, _pipe@2).

-spec serve(
    integer(),
    fun((glisten@handler:handler_message(), glisten@handler:loop_state(mist@internal@handler:state())) -> gleam@otp@actor:next(glisten@handler:loop_state(mist@internal@handler:state())))
) -> {ok, nil} | {error, glisten:start_error()}.
serve(Port, Handler) ->
    _pipe = Handler,
    _pipe@1 = glisten@acceptor:new_pool_with_data(
        _pipe,
        mist@internal@handler:new_state()
    ),
    glisten:serve(Port, _pipe@1).

-spec serve_ssl(
    integer(),
    binary(),
    binary(),
    fun((glisten@handler:handler_message(), glisten@handler:loop_state(mist@internal@handler:state())) -> gleam@otp@actor:next(glisten@handler:loop_state(mist@internal@handler:state())))
) -> {ok, nil} | {error, glisten:start_error()}.
serve_ssl(Port, Certfile, Keyfile, Handler) ->
    _pipe = Handler,
    _pipe@1 = glisten@acceptor:new_pool_with_data(
        _pipe,
        mist@internal@handler:new_state()
    ),
    glisten:serve_ssl(Port, Certfile, Keyfile, _pipe@1).

-spec handler_func(
    fun((gleam@http@request:request(mist@internal@http:body())) -> mist@internal@handler:handler_response())
) -> fun((glisten@handler:handler_message(), glisten@handler:loop_state(mist@internal@handler:state())) -> gleam@otp@actor:next(glisten@handler:loop_state(mist@internal@handler:state()))).
handler_func(Handler) ->
    mist@internal@handler:with_func(Handler).

-spec read_body(gleam@http@request:request(mist@internal@http:body())) -> {ok,
        gleam@http@request:request(bitstring())} |
    {error, mist@internal@http:decode_error()}.
read_body(Req) ->
    mist@internal@http:read_body(Req).

-spec upgrade(mist@internal@websocket:websocket_handler()) -> mist@internal@handler:handler_response().
upgrade(Websocket_handler) ->
    {upgrade, Websocket_handler}.

-spec empty_response(gleam@http@response:response(any())) -> mist@internal@handler:handler_response().
empty_response(Resp) ->
    _pipe = Resp,
    _pipe@1 = gleam@http@response:set_body(
        _pipe,
        {bit_builder_body, gleam@bit_builder:new()}
    ),
    {response, _pipe@1}.

-spec bit_builder_response(
    gleam@http@response:response(any()),
    gleam@bit_builder:bit_builder()
) -> mist@internal@handler:handler_response().
bit_builder_response(Resp, Data) ->
    _pipe = Resp,
    _pipe@1 = gleam@http@response:set_body(_pipe, {bit_builder_body, Data}),
    {response, _pipe@1}.

-spec file_response(gleam@http@response:response(any()), binary(), binary()) -> {ok,
        mist@internal@handler:handler_response()} |
    {error, mist@internal@file:file_error()}.
file_response(Resp, Path, Content_type) ->
    File_path = gleam@bit_string:from_string(Path),
    Size = filelib:file_size(File_path),
    gleam@result:map(mist_ffi:file_open(File_path), fun(Fd) -> _pipe = Resp,
            _pipe@1 = gleam@http@response:set_body(
                _pipe,
                {file_body, Fd, Content_type, 0, Size}
            ),
            {response, _pipe@1} end).

-spec chunked_response(
    gleam@http@response:response(any()),
    gleam@iterator:iterator(gleam@bit_builder:bit_builder())
) -> mist@internal@handler:handler_response().
chunked_response(Resp, Iter) ->
    _pipe = Resp,
    _pipe@1 = gleam@http@response:set_body(_pipe, {chunked, Iter}),
    {response, _pipe@1}.
