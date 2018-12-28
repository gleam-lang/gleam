fn greet(name) {
  ["Hello, ", name, "!"]
}

fn add_name(record, name) {
  record = { record | name = name }
  record
}

fn its_tim(record) {
  add_name(record, "Tim")
}
