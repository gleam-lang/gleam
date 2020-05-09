import should

pub fn precedence_test() {
  should.equal(1 + 2 * 3, 7)
  should.equal(3 * 1 + 2, 5)
  should.equal({ 1 + 2 } * 3, 9)
  should.equal(1 + 2 * 3, 7)
  should.equal(3 * { 1 + 2 }, 9)
  should.equal(1 + 2 * 3 + 4, 11)
  should.equal(2 * 3 + 4 * 5, 26)
  should.equal(2 * {3 + 1} / 2, 4)
  should.equal(5 + 3 / 3 * 2 - 6 * 4, -17)
}
