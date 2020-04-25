external fn erl_filter(
  fn(key, value) ->
  Bool,
  Map(key, value),
) -> Map(key, value) =
  "maps" "filter"

fn do() {
  case list {
    [
      x,
      ..xs
    ] -> {
      let x = 1
      x
    }
  }
}
