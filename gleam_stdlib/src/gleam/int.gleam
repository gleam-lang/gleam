import gleam/order

pub enum NotAnInt =
  | NotAnInt

pub external fn parse(String) -> Result(Int, NotAnInt) = "gleam__stdlib" "parse_int";

pub external fn to_string(Int) -> String = "erlang" "integer_to_binary"

pub external fn to_base_string(Int, Int) -> String = "erlang" "integer_to_binary"

pub fn compare(a, b) {
  case a == b {
  | True -> order.Eq
  | False ->
    case a < b {
    | True -> order.Lt
    | False -> order.Gt
    }
  }
}
