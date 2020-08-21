import should

const hex_int = 0xF

pub fn bases_test() {
  let octal_int = 0o17
  let binary_int = 0b00001111

  let tuple(x, hex_int) = tuple(octal_int, binary_int)

  should.equal(x, 15)
}
