fn go(x) {
  case x {
  | 1 -> `One
  | 2 -> `Two
  | 3 -> `Three
  | _ -> `UnknownInt(x)
  }
}
