import should

pub fn division_by_zero_test() {
  should.equal(2.0 /. 2.0, 1.0)
  should.equal(2.0 /. 0.0, 0.0)
}
