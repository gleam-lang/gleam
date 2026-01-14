pub fn empty_strings_equal_test() {
  assert "" == ""
}

pub fn newlines_equal_test() {
  assert "
" == "\n"
}

pub fn let_assert_string_prefix_test() {
  let assert "ab" <> rest = "abcdef"
  assert "cdef" == rest
}
