import map:Map

pub enum Method =
  | Get
  | Head
  | Post
  | Put
  | Delete
  | Connect
  | Options
  | Trace
  | Patch;

pub type Header =
  Tuple(String, String)

pub type Request(assigns) =
  {
    method: Method,
    path: List(String),
    headers: List(Header),
    body: Result(Unit, String)
    assigns: assigns
  };

pub type Response =
  {Int, List(Header), String};

// A Coin application is a module with these functions:
//
// - handle: Takes a Request record and returns a Response record
//
pub type App =
  module {
    fn handle(Request({})) -> Response
  };

// We have a concept of middleware which can be chained to preprocess a
// request.
//
// A middleware can return `Next` or `Send`. `Next` allows the request to
// continue through the middleware stack, while `Send` causes the server to
// immediately send a response, skipping any following middleware.
//
pub enum MiddlewareAction(assigns) =
  | Next(Request(assigns))
  | Send(Response);

pub type Middleware(assigns_in, assigns_out) =
  fn(Request(assigns_in)) -> MiddlewareAction(assigns_out);

// Wrap a Request so that it can be used by middleware.
//
pub fn into_middleware(request) {
  Next(request)
}

pub fn finally(action, handler) {
  case action {
    Next(request) -> handler(request)
    Send(response) -> response
  }
}

pub fn use(action, middleware) {
  case action {
    Next(request) -> middleware(request)
    Send(response) -> action
  }
}
