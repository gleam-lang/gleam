import two.{Box} as _some_two

pub fn unbox(x) {
  let Box(i) = x
  i
}
