import build0

pub fn hello_world_test() {
  assert "Hello, from build0!" = build0.hello_world()
  Nil
}

pub fn one_two() {
  assert 3 = build0.add(build0.one(), build0.two())
  Nil
}
