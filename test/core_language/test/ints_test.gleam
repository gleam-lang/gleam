import should

const hex_int = 0xF

pub fn bases_test() {
  let octal_int = 0o17
  let binary_int = 0b00001111

  let tuple(x, y) = tuple(octal_int, binary_int)

  should.equal(x, 15)
  should.equal(y, 15)
}

pub fn minus_lexing_test() {
  // 1-1 should lex as 1 - 1
  should.equal({1-1}, 0)
  // a-1 should lex as a - 1
  let a = 1
  should.equal({a-1}, 0)
  // 1- 1 should lex as 1 - 1
  should.equal({1- 1}, 0)
}
