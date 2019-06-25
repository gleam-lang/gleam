import gleam/iodata

pub enum NotAFloat =
  | NotAFloat

pub external fn parse(String) -> Result(Float, NotAFloat) =
  "gleam__stdlib" "parse_float";

pub fn to_string(f) {
  f
  |> iodata:from_float
  |> iodata:to_string
}

pub external fn ceiling(Float) -> Float = "math" "ceil";

pub external fn floor(Float) -> Float = "math" "floor";

pub external fn round(Float) -> Int = "erlang" "round";

pub external fn truncate(Float) -> Int = "erlang" "trunc";
