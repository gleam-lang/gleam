import one
import one/nested

pub fn main() {
  one.hello()
  |> nested.id
}
