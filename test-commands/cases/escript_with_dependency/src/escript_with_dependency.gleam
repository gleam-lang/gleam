import argv

pub fn main() -> Nil {
  // Using a dependency package
  assert argv.load().arguments == []

  Nil
}
