pub fn case_string_prefix_match_test() {
  let string = "12345"
  let string_2 = case string {
    "0" <> rest -> rest
    "123" <> rest -> rest
    _ -> ""
  }
  assert "45" == string_2
}

pub fn match_emoji_test() {
  let string = "🫥 is neutral dotted"
  let string_2 = case string {
    "🫥" <> rest -> rest
    _ -> panic
  }
  assert " is neutral dotted" == string_2
}

pub fn match_theta_test() {
  let string = "Θ wibble wobble"
  let string_2 = case string {
    "Θ" <> rest -> rest
    _ -> panic
  }
  assert " wibble wobble" == string_2
}

pub fn match_flag_emoji_test() {
  let string = "🇺🇸 is a cluster"
  let string_2 = case string {
    "🇺🇸" <> rest -> rest
    _ -> panic
  }
  assert " is a cluster" == string_2
}

pub fn match_backslash_test() {
  let string = "\" is a backslash"
  let string_2 = case string {
    "\"" <> rest -> rest
    _ -> panic
  }
  assert " is a backslash" == string_2
}

pub fn match_newline_test() {
  let string = "\n is a newline"
  let string_2 = case string {
    "\n" <> rest -> rest
    _ -> panic
  }
  assert " is a newline" == string_2
}

pub fn match_escaped_newline_test() {
  let string = "\\n is a newline that escaped"
  let string_2 = case string {
    "\\n" <> rest -> rest
    _ -> panic
  }
  assert " is a newline that escaped" == string_2
}
