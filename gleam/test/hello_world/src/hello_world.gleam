import plug
import string

pub fn init(opts) {
  opts
}

pub fn call(conn, opts) {
  case plug:path_info(conn) {
  | [] ->
      plug:send_resp(conn, 200, "Home page")

  | ["user", name] ->
      let body = string:concat([name, "'s profile page"])
      plug:send_resp(conn, 200, body)

  | any ->
      plug:send_resp(conn, 404, "Page not found")
  }
}
