pub fn greet(name) {
  ["Hello, ", name, "!"]
}

pub fn curry(f) {
  fn(x) {
    fn(y) {
      f(x, y)
    }
  }
}

fn add(x, y) {
  x + y
}

fn adder() {
  curry(add)
}

fn incrementor() {
  adder()(1)
}

fn increment(x) {
  incrementor()(x)
}
