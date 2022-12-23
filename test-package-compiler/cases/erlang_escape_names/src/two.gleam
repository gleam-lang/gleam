// https://github.com/gleam-lang/gleam/issues/340
import one.{receive}

pub fn qualified_call() {
  one.receive(1)
}

pub fn qualified_value() {
  one.receive
}

pub fn unqualified_call() {
  receive(1)
}

pub fn unqualified_value() {
  receive
}
