import should

// Helpers

fn integer_fn() {
  1
}

// Valid values

pub fn function_as_value_test() {
  let <<a>> = <<integer_fn():integer>>

  should.equal(a, 1)
}

// Strings

pub fn string_test() {
  let a = "test"
  let <<b:2-binary, "st":utf8>> = a

  should.equal(b, <<"te":utf8>>)
}

pub fn explicit_utf8_test() {
  let a = <<"test":utf8>>
  let <<b:2-binary, "st":utf8>> = a

  should.equal(b, <<"te":utf8>>)
}

pub fn emoji_test() {
  let a = <<"😁😀":utf8>>
  let <<b:4-binary, "😀":utf8>> = a

  should.equal(b, <<"😁":utf8>>)
}

