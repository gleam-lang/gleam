import two.{Box} as some_two

pub fn unbox(x) {
  let Box(i) = x
  i
}
