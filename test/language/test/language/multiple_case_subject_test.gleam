pub fn wildcard_test() {
  let x = true()
  let y = false()
  let result = case x, y {
    _, _ -> 0
  }
  assert result == 0
}

pub fn no_match_test() {
  let x = true()
  let y = false()
  let result = case x, y {
    False, True -> 1
    _, _ -> 0
  }
  assert result == 0
}

pub fn match_test() {
  let x = true()
  let y = false()
  let result = case x, y {
    False, True -> 1
    _, _ -> 0
  }
  assert result == 0
}

pub fn alternative_test() {
  let x = true()
  let y = false()
  let result = case x, y {
    False, True | True, False -> 1
    _, _ -> 0
  }
  assert result == 1
}

pub fn guard_test() {
  let x = true()
  let y = true()
  let result = case x, y {
    x, y if x == y -> 1
    _, _ -> 0
  }
  assert result == 1
}

fn true() -> Bool {
  True
}

fn false() -> Bool {
  False
}
