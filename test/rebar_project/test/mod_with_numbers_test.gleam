import mod_with_numbers_0123456789
import should

pub fn import_test() {
  mod_with_numbers_0123456789.hello()
  |> should.equal("world")
}
