import gleam/string
import gleam/list
import gleam/map
import gleam/result
import gleam/option.{None, Option, Some}
import gleam/uri
import gleam/bit_builder
import gleam/otp/actor
import gleam/erlang/process.{Selector, Subject}
import gleam/http.{Get}
import gleam/http/request.{Request}
import gleam/http/response.{Response as HttpResponse}
import mist.{Body}
import mist/websocket
import glisten
import glisten/handler.{HandlerMessage}
import nakai
import nakai/html
import nakai/html/attrs
import gliew/internal/event.{Event, LiveMount}
import gliew/internal/worker.{LiveUpdate, WorkerMessage}
import gliew/internal/manager.{Message as ManagerMessage}
import gliew/internal/util.{random_hex_string}

const elem_id_prefix = "g-"

// Response is the final response that should be returned from a
// handler.
//
pub opaque type Response {
  View(status: Int, headers: List(#(String, String)), node: html.Node(Event))
  Response(status: Int, headers: List(#(String, String)), body: Option(String))
}

/// Creates a view response.
///
pub fn view(node: html.Node(Event), status: Int) {
  View(status, [], node)
}

/// Creates an empty response without a body.
///
pub fn response(status: Int) {
  Response(status, [], None)
}

/// Adds a header to a response.
///
pub fn with_header(response: Response, key key: String, value value: String) {
  case response {
    View(status, headers, node) ->
      headers
      |> list.prepend(#(key, value))
      |> View(status, _, node)
    Response(status, headers, body) ->
      headers
      |> list.prepend(#(key, value))
      |> Response(status, _, body)
  }
}

/// Sets body of a response.
///
pub fn with_body(response: Response, body: String) {
  case response {
    Response(status, headers, _) -> Response(status, headers, Some(body))
    _ -> response
  }
}

/// Creates a HTML node tree that will be `mounted`
/// and receive live updates with data on `Subject(a)`
/// returned by the mount function.
///
pub fn live_mount(
  mount mount: fn(b) -> Subject(a),
  with context: b,
  render render: fn(Option(a)) -> html.Node(Event),
) {
  // Render initial node tree
  let tree =
    render(None)
    |> process_tree(None, False)

  // Get id from root element
  let id = extract_id(tree)

  // Add mount event to root of tree
  fn(selector: Selector(WorkerMessage)) {
    // Mount component to get subject
    let subject = mount(context)

    // Select and map subject
    selector
    |> process.selecting(
      subject,
      fn(val) {
        render(Some(val))
        |> process_tree(Some(id), True)
        |> nakai.to_inline_string
        |> LiveUpdate
      },
    )
  }
  |> LiveMount
  |> insert_event(tree)
}

/// Attaches an on-click handler to a node by specifying
/// the HTTP method and path to make a request to when
/// clicked.
///
pub fn on_click(node: html.Node(Event), do method: http.Method, to path: String) {
  case method {
    http.Get ->
      node
      |> add_attr(attrs.Attr("hx-get", path))
      |> add_attr(attrs.Attr("hx-swap", "none"))
    http.Post ->
      node
      |> add_attr(attrs.Attr("hx-post", path))
      |> add_attr(attrs.Attr("hx-swap", "none"))
    http.Put ->
      node
      |> add_attr(attrs.Attr("hx-put", path))
      |> add_attr(attrs.Attr("hx-swap", "none"))
    http.Patch ->
      node
      |> add_attr(attrs.Attr("hx-patch", path))
      |> add_attr(attrs.Attr("hx-swap", "none"))
    http.Delete ->
      node
      |> add_attr(attrs.Attr("hx-delete", path))
      |> add_attr(attrs.Attr("hx-swap", "none"))
    _ -> node
  }
}

/// Makes the live mount morph the diff of the innerHTML
/// instead of replacing the whole thing.
///
pub fn morph() {
  attrs.Event("gliew-event", event.Morph)
}

/// Append the innerHTML of the live mount instead of
/// replacing it.
///
pub fn append() {
  attrs.Event("gliew-event", event.Append)
}

// Add an attribute to the provided node.
//
fn add_attr(node: html.Node(Event), attr: attrs.Attr(Event)) {
  case node {
    html.Element(tag, attrs, children) ->
      attrs
      |> list.prepend(attr)
      |> html.Element(tag, _, children)
    html.LeafElement(tag, attrs) ->
      attrs
      |> list.prepend(attr)
      |> html.LeafElement(tag, _)
    html.Html(attrs, children) ->
      attrs
      |> list.prepend(attr)
      |> html.Html(children)
    html.Body(attrs, children) ->
      attrs
      |> list.prepend(attr)
      |> html.Body(children)
    // Other nodes don't accept attributes
    // we return them unchanged
    other -> other
  }
}

// Insert the event to the node.
//
fn insert_event(event: Event, node: html.Node(Event)) {
  node
  |> add_attr(attrs.Event("gliew-event", event))
}

// Adds the id attribute to the node if provided, otherwise
// generates it.
//
fn process_tree(node: html.Node(Event), id: Option(String), render_events: Bool) {
  case node {
    html.Element(tag, attrs, children) ->
      attrs
      |> ensure_id(id)
      |> map_events(render_events)
      |> html.Element(tag, _, children)
    html.LeafElement(tag, attrs) ->
      attrs
      |> ensure_id(id)
      |> map_events(render_events)
      |> html.LeafElement(tag, _)
    other -> other
  }
}

// Transforms event attributes into actual htmx attributes.
//
fn map_events(attrs: List(attrs.Attr(Event)), render_events: Bool) {
  case render_events {
    True ->
      list.map(
        attrs,
        fn(attr) {
          case attr {
            attrs.Event("gliew-event", event.Morph) ->
              attrs.Attr("hx-swap-oob", "morph")
            attrs.Event("gliew-event", event.Append) ->
              attrs.Attr("hx-swap-oob", "beforeend")
            other -> other
          }
        },
      )
    False -> attrs
  }
}

// Make sure there is an id attribute in the list of attributes.
// If id is `None` it will generate one if there isn't one.
// If id is `Some(id)` it will replace any id if there is one or
// otherwise add it.
//
fn ensure_id(attrs: List(attrs.Attr(Event)), id: Option(String)) {
  case has_id(attrs) {
    True ->
      case id {
        Some(id) ->
          list.map(
            attrs,
            fn(attr) {
              case attr {
                attrs.Attr("id", _) -> attrs.id(id)
                other -> other
              }
            },
          )
        None -> attrs
      }
    False ->
      attrs
      |> list.prepend(attrs.id(
        id
        |> option.unwrap(random_id()),
      ))
  }
}

// Extract the ID value from a HTML node.
//
fn extract_id(node: html.Node(Event)) {
  case node {
    html.Element(_, attrs, _) -> find_id(attrs)
    html.LeafElement(_, attrs) -> find_id(attrs)
    _ -> Error(Nil)
  }
  |> result.unwrap(random_id())
}

// Find id attribute in a list of attrs.
//
fn find_id(attrs: List(attrs.Attr(Event))) {
  attrs
  |> list.find_map(fn(attr) {
    case attr {
      attrs.Attr("id", id) -> Ok(id)
      _ -> Error(Nil)
    }
  })
}

// Generates a random ID for a HTML id attribute.
//
fn random_id() {
  elem_id_prefix <> random_hex_string(3)
}

// Check if list of attrs has id.
//
fn has_id(attrs: List(attrs.Attr(a))) {
  attrs
  |> list.any(fn(a) {
    case a {
      attrs.Attr("id", _) -> True
      _ -> False
    }
  })
}

// Server ------------------------------------------------

/// Returns script tags to put into your own layout in order
/// for live mounts to work.
///
pub fn script() {
  html.Fragment([
    html.Element(
      tag: "script",
      attrs: [attrs.src("https://unpkg.com/htmx.org@1.9.2/dist/htmx.min.js")],
      children: [],
    ),
    html.Element(
      tag: "script",
      attrs: [attrs.src("https://unpkg.com/htmx.org@1.9.2/dist/ext/ws.js")],
      children: [],
    ),
    html.Element(
      tag: "script",
      attrs: [
        attrs.src("https://unpkg.com/idiomorph@0.0.8/dist/idiomorph-ext.min.js"),
      ],
      children: [],
    ),
  ])
}

fn default_layout(content: html.Node(Event)) {
  html.Html(
    [],
    [html.Head([script()]), html.Body(attrs: [], children: [content])],
  )
}

/// Configuration for a server.
///
pub type Server {
  Server(
    port: Int,
    layout: fn(html.Node(Event)) -> html.Node(Event),
    handler: fn(Request(Body)) -> Response,
  )
}

/// Creates a configuration for a server with default
/// layout.
///
pub fn new(port: Int, handler: fn(Request(Body)) -> Response) {
  Server(port, default_layout, handler)
}

/// Start server.
///
pub fn serve(server: Server) {
  use manager <- result.try(
    manager.start_manager()
    |> result.map_error(fn(err) {
      case err {
        actor.InitTimeout -> glisten.AcceptorTimeout
        actor.InitFailed(reason) -> glisten.AcceptorFailed(reason)
        actor.InitCrashed(any) -> glisten.AcceptorCrashed(any)
      }
    }),
  )

  mist.serve(server.port, handler_func(manager, server.layout, server.handler))
}

fn handler_func(
  manager: Subject(ManagerMessage),
  layout: fn(html.Node(Event)) -> html.Node(Event),
  handler: fn(Request(Body)) -> Response,
) {
  // Return actual handler func
  fn(req: Request(Body)) {
    case req.method, req.path {
      Get, "/connect" -> handle_ws_connect(manager, req)
      _, _ ->
        case handler(req) {
          Response(status, headers, body) ->
            response.new(status)
            |> add_headers(headers)
            |> to_mist_response(body)
          View(status, headers, node) ->
            response.new(status)
            |> add_headers(headers)
            |> mist.bit_builder_response(
              manager.process_tree(manager, req, node)
              |> layout
              |> nakai.to_string_builder
              |> bit_builder.from_string_builder,
            )
        }
    }
  }
  |> mist.handler_func
}

fn add_headers(response: HttpResponse(a), headers: List(#(String, String))) {
  headers
  |> list.fold(
    response,
    fn(res, pair) {
      res
      |> response.prepend_header(pair.0, pair.1)
    },
  )
}

fn to_mist_response(response, body: Option(String)) {
  case body {
    Some(body) ->
      response
      |> mist.bit_builder_response(bit_builder.from_string(body))
    None ->
      response
      |> mist.empty_response
  }
}

fn handle_ws_connect(manager: Subject(ManagerMessage), req: Request(Body)) {
  case parse_params(req) {
    Ok(#(id, csrf)) -> upgrade_connection(manager, id, csrf)
    Error(Nil) ->
      response.new(401)
      |> mist.empty_response
  }
}

fn upgrade_connection(
  manager: Subject(ManagerMessage),
  session: String,
  csrf: String,
) {
  case manager.get_worker(manager, session, csrf) {
    Ok(worker) ->
      fn(_msg, _subject: Subject(HandlerMessage)) { Ok(Nil) }
      |> websocket.with_handler
      |> websocket.on_init(fn(socket: Subject(HandlerMessage)) {
        worker.connect(worker, socket)
      })
      |> websocket.on_close(fn(socket: Subject(HandlerMessage)) {
        worker.disconnect(worker, socket)
      })
      |> mist.upgrade
    Error(Nil) ->
      response.new(401)
      |> mist.empty_response
  }
}

fn parse_params(req: Request(Body)) {
  case req.query {
    Some(params) ->
      case uri.parse_query(params) {
        Ok(params) ->
          list.map(
            params,
            fn(p) {
              #(
                p.0,
                p.1
                |> string.replace(" ", "+"),
              )
            },
          )
          |> get_params
        Error(Nil) -> Error(Nil)
      }
    None -> Error(Nil)
  }
}

fn get_params(params: List(#(String, String))) {
  let pmap = map.from_list(params)

  use session <- result.then(map.get(pmap, "session"))
  use csrf <- result.then(map.get(pmap, "csrf"))

  Ok(#(session, csrf))
}
