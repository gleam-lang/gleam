import iodata

pub enum NotAFloat =
  | NotAFloat

pub external fn parse(String) -> Result(Float, NotAFloat) =
  "gleam__stdlib" "parse_float";

pub fn to_string(f) {
  f
  |> iodata:from_float
  |> iodata:to_string
}
