import coin:[Send, Next, Get, respond, body]
import string

pub fn handle(request) {
  request
    |> coin:into_middleware
    |> coin:use(_, reject_favicon_requests)
    |> coin:finally(_, router)
}

fn router(request) {
  case {request.method, request.path} {
  | {Get, []} ->
      home_page()

  | {Get, ["profile", name]} ->
      profile_page(name)

  | _ ->
      not_found()
  }
}

fn reject_favicon_requests(request) {
  case request.path {
  | ["favicon.ico"] -> Send({404, [], ""})
  | _ -> Next
  }
}

fn home_page() {
  {200, [], "Welcome!"}
}

fn profile_page(name) {
  {200, [], string:concat([name, "'s profile"])}
}

fn not_found() {
  {404, [], "Page not found"}
}
