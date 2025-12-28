@external(erlang, "thing", "new")
pub fn thing() -> Nil

// https://github.com/gleam-lang/gleam/issues/4507
@external(erlang, "the.thing", "make.new")
pub fn escaped_thing() -> Nil
