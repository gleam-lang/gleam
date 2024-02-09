import gleam/erlang/charlist.{Charlist}

external fn log_error(format: Charlist, data: any) -> Nil =
  "logger" "error"

pub fn error(data: any) -> Nil {
  log_error(charlist.from_string("~tp"), [data])
}
