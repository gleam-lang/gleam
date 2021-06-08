import gleam
import should

pub fn nil_test() {
  gleam.Nil
  |> should.equal(Nil)
}
