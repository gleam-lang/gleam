import importable.{type MovieAlias, type Pair, Movie, make_pair}

// Type aliases are transparent at runtime: a value created through the
// underlying type is equal to the same value viewed through its alias.

pub fn alias_is_transparent_test() {
  let via_alias: MovieAlias = Movie("Gleam")
  assert via_alias == Movie("Gleam")
}

pub fn alias_value_equals_underlying_test() {
  let m = importable.war_games
  let a: MovieAlias = m
  assert a == m
}

pub fn parametric_alias_construction_test() {
  let p: Pair(Int, String) = make_pair(42, "hello")
  assert p == #(42, "hello")
}

pub fn parametric_alias_destructure_test() {
  let p: Pair(String, Int) = make_pair("gleam", 1)
  let #(lang, version) = p
  assert lang == "gleam"
  assert version == 1
}

pub fn parametric_alias_equality_test() {
  let a: Pair(Int, Int) = make_pair(1, 2)
  let b: Pair(Int, Int) = #(1, 2)
  assert a == b
}
