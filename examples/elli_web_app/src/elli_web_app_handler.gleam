pub external type Request;

pub external fn request_path(Request) -> List(String) = "elli_request" "path";

pub fn handle(request, _args) {
  case request_path(request) {
  | [] ->
      {200, [], "Home page"}

  | _ ->
      {404, [], "Page not found"}
  }
}


pub enum HandleEvent =
  | Ok

pub fn handle_event(_event, _data, _args) {
  Ok
}
