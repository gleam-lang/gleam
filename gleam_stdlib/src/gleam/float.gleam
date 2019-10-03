import gleam/iodata
import gleam/order

pub external fn parse(String) -> Result(Float, Nil) =
  "gleam_stdlib" "parse_float";

pub fn to_string(f) {
  f
  |> iodata.from_float
  |> iodata.to_string
}

pub fn compare(a, b) {
  case a == b {
  | True -> order.Eq
  | False ->
    case a <. b {
    | True -> order.Lt
    | False -> order.Gt
    }
  }
}

pub fn min(a, b) {
  case a <. b {
  | True -> a
  | False -> b
  }
}

pub fn max(a, b) {
  case a >. b {
  | True -> a
  | False -> b
  }
}

pub external fn ceiling(Float) -> Float = "math" "ceil";

pub external fn floor(Float) -> Float = "math" "floor";

pub external fn round(Float) -> Int = "erlang" "round";

pub external fn truncate(Float) -> Int = "erlang" "trunc";
