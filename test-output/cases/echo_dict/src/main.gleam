import gleam/dict

pub fn main() {
  echo dict.new()
  echo dict.from_list([#(1, "hello"), #(2, "world!")])
}
