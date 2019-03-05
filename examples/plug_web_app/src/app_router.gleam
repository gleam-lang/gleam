import plug

external fn concat(List(String)) -> String = "erlang" "list_to_binary"

pub fn init(_options) {
  {}
}

pub fn call(conn, _options) {
  let method = plug:method(conn)
  let path = plug:path_info(conn)

  case {method, path} {
  | {plug:Get, []} ->
      plug:send_resp(conn, 200, "Welcome home!")

  | {plug:Get, ["profile", name]} ->
      let body = concat([name, "'s profile page"])
      plug:send_resp(conn, 200, body)

  | {_, _} ->
      plug:send_resp(conn, 404, "Page not found")
  }
}
