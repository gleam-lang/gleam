import should

// Helpers
fn integer_fn() {
  1
}

// Valid values
pub fn function_as_value_test() {
  let <<a>> = <<integer_fn():int>>

  should.equal(a, 1)
}

pub fn integer_to_binary_test() {
  let <<a, rest:binary>> = <<1, 17, 42:16>>

  should.equal(a, 1)
  should.equal(rest, <<17, 0, 42>>)
}

// Sizes
pub fn size_variable_from_match_test() {
  let <<
    name_size:8,
    name:binary-size(name_size),
    " the ":utf8,
    species:binary,
  >> = <<5, "Frank the Walrus":utf8>>

  should.equal(name, <<"Frank":utf8>>)
  should.equal(species, <<"Walrus":utf8>>)
}

pub fn sizes_with_expressions_test() {
  let a = 1
  let b = <<a:size(a * 2), a:size(3 + integer_fn())>>

  should.equal(b, <<1:2, 1:4>>)
}

// Units
pub fn units_test() {
  let a = <<1:size(1)-unit(8), 2:size(1)-unit(16)>>

  should.equal(a, <<1, 0, 2>>)
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

pub fn codepoint_conversion_test() {
  let <<snake:utf8>> = <<"🐍":utf8>>
  let <<snake_int:32>> = <<snake:utf32_codepoint>>

  should.equal(snake_int, 128013)
}

pub fn string_variable_test() {
  let x = "x"
  let y = <<x:utf8, "ß↑e̊":utf8>>
  let <<z:8, _:binary>> = y

  should.equal(z, 120) // x is unicode codepoint 120
}
