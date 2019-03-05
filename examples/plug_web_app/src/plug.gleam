pub external type Conn
pub external type SentConn

pub enum Method =
  | Post
  | Patch
  | Put
  | Get
  | Delete
  | Options
  | Other(String)

pub external fn method(Conn) -> Method = "Elixir.GleamPlugFFI" "method"

pub external fn path_info(Conn) -> List(String) = "Elixir.GleamPlugFFI" "path_info"

pub external fn send_resp(Conn, Int, String) -> SentConn = "Elixir.Plug.Conn" "send_resp"
