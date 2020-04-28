import should

pub fn precedence_test() {
  should.equal(1 + 2 * 3, 7)
  should.equal(3 * 1 + 2, 5)
  should.equal({ 1 + 2 } * 3, 9)
  should.equal(3 * { 1 + 2 }, 9)
}
