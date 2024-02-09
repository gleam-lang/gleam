-module(mist@websocket).
-compile([no_auto_import, nowarn_unused_vars]).

-export([send/2, echo_handler/2, with_handler/1, on_init/2, on_close/2]).

-spec send(
    gleam@erlang@process:subject(glisten@handler:handler_message()),
    mist@internal@websocket:message()
) -> nil.
send(Sender, Message) ->
    _pipe@2 = case Message of
        {text_message, Data} ->
            _pipe = Data,
            _pipe@1 = gleam@bit_string:from_string(_pipe),
            mist@internal@websocket:to_text_frame(_pipe@1);

        {binary_message, Data@1} ->
            mist@internal@websocket:to_binary_frame(Data@1)
    end,
    _pipe@3 = {send_message, _pipe@2},
    gleam@erlang@process:send(Sender, _pipe@3),
    nil.

-spec echo_handler(
    mist@internal@websocket:message(),
    gleam@erlang@process:subject(glisten@handler:handler_message())
) -> {ok, nil} | {error, nil}.
echo_handler(Message, Sender) ->
    _ = send(Sender, Message),
    {ok, nil}.

-spec with_handler(
    fun((mist@internal@websocket:message(), gleam@erlang@process:subject(glisten@handler:handler_message())) -> {ok,
            nil} |
        {error, nil})
) -> mist@internal@websocket:websocket_handler().
with_handler(Func) ->
    {websocket_handler, none, none, Func}.

-spec on_init(
    mist@internal@websocket:websocket_handler(),
    fun((gleam@erlang@process:subject(glisten@handler:handler_message())) -> nil)
) -> mist@internal@websocket:websocket_handler().
on_init(Handler, Func) ->
    erlang:setelement(3, Handler, {some, Func}).

-spec on_close(
    mist@internal@websocket:websocket_handler(),
    fun((gleam@erlang@process:subject(glisten@handler:handler_message())) -> nil)
) -> mist@internal@websocket:websocket_handler().
on_close(Handler, Func) ->
    erlang:setelement(2, Handler, {some, Func}).
