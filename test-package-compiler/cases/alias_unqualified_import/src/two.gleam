// https://github.com/gleam-lang/gleam/issues/303
import one.{Empty as E, id as i}

pub fn make() {
  i(E)
}
