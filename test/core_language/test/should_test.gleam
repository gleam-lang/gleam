import should

pub fn equal_test() {
    should.equal(1, 1)
    should.equal(True, True)
}

pub fn not_equal_test() {
    should.not_equal(1, 2)
    should.not_equal(True, False)
}

