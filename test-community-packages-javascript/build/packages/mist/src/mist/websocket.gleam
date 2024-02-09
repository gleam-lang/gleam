import gleam/bit_string
import gleam/erlang/process.{Subject}
import gleam/option.{None, Some}
import glisten/handler.{HandlerMessage, SendMessage}
import mist/internal/websocket.{
  BinaryMessage, EventHandler, Handler, Message, TextMessage, WebsocketHandler,
  to_binary_frame, to_text_frame,
}

/// Helper to encapsulate the logic to send a provided message over the
/// WebSocket
pub fn send(sender: Subject(HandlerMessage), message: Message) -> Nil {
  case message {
    TextMessage(data) ->
      data
      |> bit_string.from_string
      |> to_text_frame
    BinaryMessage(data) -> to_binary_frame(data)
  }
  |> SendMessage
  |> process.send(sender, _)

  Nil
}

pub fn echo_handler(
  message: Message,
  sender: Subject(HandlerMessage),
) -> Result(Nil, Nil) {
  let _ = send(sender, message)

  Ok(Nil)
}

pub fn with_handler(func: Handler) -> WebsocketHandler {
  WebsocketHandler(on_close: None, on_init: None, handler: func)
}

pub fn on_init(
  handler: WebsocketHandler,
  func: EventHandler,
) -> WebsocketHandler {
  WebsocketHandler(..handler, on_init: Some(func))
}

pub fn on_close(
  handler: WebsocketHandler,
  func: EventHandler,
) -> WebsocketHandler {
  WebsocketHandler(..handler, on_close: Some(func))
}
