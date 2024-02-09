import gleam/bit_builder.{BitBuilder}
import gleam/bit_string
import gleam/http/request.{Request}
import gleam/http/response.{Response as HttpResponse}
import gleam/iterator.{Iterator}
import gleam/result
import glisten
import glisten/acceptor
import glisten/handler.{LoopFn} as glisten_handler
import mist/internal/handler.{
  HandlerResponse, Response as MistResponse, State, Upgrade,
}
import mist/internal/http.{BitBuilderBody, Body as HTTPBody, Chunked, FileBody}
import mist/internal/file.{FileError}
import mist/internal/websocket.{WebsocketHandler}

/// This type reflects whether the body has been read from the socket yet.
pub type Body =
  HTTPBody

/// Mist supports `BitBuilder`, `FileBody`, and `Chunked` response types. The
/// helper methods provided can generate these from a `gleam/http/response`
/// Response. This type is re-exported for pulling specific handlers out into
/// separate functions.
pub type Response =
  HandlerResponse

/// Runs an HTTP Request->Response server at the given port, with your defined
/// handler. This will automatically read the full body contents up to the
/// specified `max_body_limit` in bytes. If you'd prefer to have finer-grain
/// control over this behavior, consider using `mist.serve`.
pub fn run_service(
  port: Int,
  handler: handler.Handler,
  max_body_limit max_body_limit: Int,
) -> Result(Nil, glisten.StartError) {
  handler
  |> handler.with(max_body_limit)
  |> acceptor.new_pool_with_data(handler.new_state())
  |> glisten.serve(port, _)
}

/// Similar setup and behavior to `run_service`, but instead takes in the SSL
/// certificate/key and serves over HTTPS.
pub fn run_service_ssl(
  port port: Int,
  certfile certfile: String,
  keyfile keyfile: String,
  handler handler: handler.Handler,
  max_body_limit max_body_limit: Int,
) -> Result(Nil, glisten.StartError) {
  handler
  |> handler.with(max_body_limit)
  |> acceptor.new_pool_with_data(handler.new_state())
  |> glisten.serve_ssl(
    port: port,
    certfile: certfile,
    keyfile: keyfile,
    with_pool: _,
  )
}

/// Slightly more flexible alternative to `run_service`. This allows hooking
/// into the `mist.handler_func` method. Note that the request body
/// will not be automatically read. You will need to call `mist.read_body`.
/// Ensure that this is only called _once_ per request.
pub fn serve(
  port port: Int,
  handler handler: LoopFn(State),
) -> Result(Nil, glisten.StartError) {
  handler
  |> acceptor.new_pool_with_data(handler.new_state())
  |> glisten.serve(port, _)
}

/// Similar to the `run_service` method, `serve` also has a similar SSL method.
pub fn serve_ssl(
  port port: Int,
  certfile certfile: String,
  keyfile keyfile: String,
  handler handler: LoopFn(State),
) -> Result(Nil, glisten.StartError) {
  handler
  |> acceptor.new_pool_with_data(handler.new_state())
  |> glisten.serve_ssl(
    port: port,
    certfile: certfile,
    keyfile: keyfile,
    with_pool: _,
  )
}

/// Handles converting the mist `Response` type into a gleam HTTP Response. Use
/// this when calling `mist.serve` to start your application.
pub fn handler_func(
  handler: handler.HandlerFunc,
) -> glisten_handler.LoopFn(handler.State) {
  handler.with_func(handler)
}

/// When using `mist.serve`, the body is not automatically read. You can
/// inspect content headers to determine whether to read the body or not.
/// This function will pull the content from the sender. It gives back a
/// `Request(BitString)` containing the body. This return value should be
/// treated as replacing the initial request. Do not attempt to call this
/// method multiple times on the same request.
pub fn read_body(
  req: Request(Body),
) -> Result(Request(BitString), http.DecodeError) {
  http.read_body(req)
}

/// A websocket handler is created using the `websocket.with_handler`
/// method. This function enables the mist HTTP layer to build the properly
/// formatted websocket upgrade response.
pub fn upgrade(websocket_handler: WebsocketHandler) -> Response {
  Upgrade(websocket_handler)
}

/// `mist.serve` expects the mist Response type, rather than the `gleam/http`
/// Response. When returning a response with no body, this will convert the
/// type. Note that any previously set response body will be removed before
/// sending.
pub fn empty_response(resp: HttpResponse(a)) -> Response {
  resp
  |> response.set_body(BitBuilderBody(bit_builder.new()))
  |> MistResponse
}

/// The mist runtime only supports sending BitBuilder types, or files (see
/// below). This method will erase any pre-existing response body.
pub fn bit_builder_response(resp: HttpResponse(a), data: BitBuilder) -> Response {
  resp
  |> response.set_body(BitBuilderBody(data))
  |> MistResponse
}

/// This is a more generally optimized method for returning files to a client.
/// It's a light wrapper around Erlang's `file:sendfile/5` method. The error
/// can be matched on with `mist/file.{FileError}` if custom behavior is desired
/// for various cases. The size of the file will be added to the
/// `content-length` header field.
pub fn file_response(
  resp: HttpResponse(a),
  path: String,
  content_type: String,
) -> Result(Response, FileError) {
  let file_path = bit_string.from_string(path)
  let size = file.size(file_path)
  use fd <- result.map(file.open(file_path))
  resp
  |> response.set_body(FileBody(fd, content_type, 0, size))
  |> MistResponse
}

/// You can send chunks of responses from an iterator. The iterator must
/// complete.
pub fn chunked_response(
  resp: HttpResponse(a),
  iter: Iterator(BitBuilder),
) -> Response {
  resp
  |> response.set_body(Chunked(iter))
  |> MistResponse
}
