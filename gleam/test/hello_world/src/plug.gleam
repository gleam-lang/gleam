pub external type Conn

pub external type Done

pub external fn path_info(Conn) -> List(String)
  = "Elixir.Plug.Conn" "path_info"

pub external fn send_resp(Conn, Int, String) -> Done
  = "Elixir.Plug.Conn" "send_resp"
