// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

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

pub fn multiple_single_character_matches() {
  let string = "1234"
  let string_2 = case string {
    "0" <> _ -> panic
    "1" <> rest -> rest
    "2" <> _ -> panic
    _ -> panic
  }
  assert "234" == string_2
}

// https://github.com/gleam-lang/gleam/issues/5856
pub fn overlapping_string_prefixes_with_guard_test() {
  let classify = fn(input: String, flag: Bool) -> String {
    case input {
      "aa" <> _rest if flag -> "wibble"
      "a" <> _rest if flag -> "wobble"
      "a" <> _rest -> "woo"
      _ -> "other"
    }
  }

  assert classify("aabc", True) == "wibble"
  assert classify("aabc", False) == "woo"
  assert classify("abc", True) == "wobble"
  assert classify("abc", False) == "woo"
  assert classify("xyz", True) == "other"
}

// https://github.com/gleam-lang/gleam/issues/5856
pub fn string_prefix_binding_rest_with_guard_test() {
  let classify = fn(input: String, flag: Bool) -> String {
    case input {
      "aa" <> rest if flag -> rest
      "a" <> rest -> rest
      _ -> "other"
    }
  }

  assert classify("aax", True) == "x"
  assert classify("aax", False) == "ax"
  assert classify("abc", True) == "bc"
  assert classify("z", True) == "other"
}

// https://github.com/gleam-lang/gleam/issues/5856
pub fn non_overlapping_sibling_string_prefix_test() {
  let classify = fn(input: String, a: Bool, b: Bool) -> String {
    case input {
      "aa" <> _ if a -> "aa"
      "a" <> _ if b -> "a-guard"
      "ac" <> _ -> "ac"
      _ -> "other"
    }
  }

  assert classify("aaz", True, False) == "aa"
  assert classify("aaz", False, True) == "a-guard"
  assert classify("aaz", False, False) == "other"
  assert classify("acz", False, False) == "ac"
  assert classify("acz", False, True) == "a-guard"
  assert classify("xyz", False, False) == "other"
}

// https://github.com/gleam-lang/gleam/issues/5856
pub fn exact_string_after_longer_prefix_test() {
  let classify = fn(input: String, a: Bool, b: Bool) -> String {
    case input {
      "aa" <> _ if a -> "aa-prefix"
      "a" <> _ if b -> "a-prefix"
      "a" -> "exact-a"
      _ -> "other"
    }
  }

  assert classify("aa", True, False) == "aa-prefix"
  assert classify("a", False, True) == "a-prefix"
  assert classify("a", False, False) == "exact-a"
  assert classify("ab", False, False) == "other"
  assert classify("x", False, False) == "other"
}

// https://github.com/gleam-lang/gleam/issues/5856
pub fn string_prefix_name_binding_after_longer_prefix_test() {
  let classify = fn(input: String, flag: Bool) -> String {
    case input {
      "aa" <> _ if flag -> "aa"
      "a" as first <> rest -> rest <> first
      _ -> "other"
    }
  }

  assert classify("aabc", True) == "aa"
  assert classify("aabc", False) == "abca"
  assert classify("axyz", True) == "xyza"
  assert classify("z", True) == "other"
}

// https://github.com/gleam-lang/gleam/issues/5856
pub fn string_prefix_guard_on_bound_variable_falls_through_test() {
  let classify = fn(input: String) -> String {
    case input {
      "aa" <> rest if rest == "x" -> "aa-x"
      "a" <> rest -> rest
      _ -> "other"
    }
  }

  assert classify("aax") == "aa-x"
  assert classify("aay") == "ay"
  assert classify("abc") == "bc"
  assert classify("z") == "other"
}

// https://github.com/gleam-lang/gleam/issues/5856
pub fn string_prefix_nested_guard_falls_through_to_shorter_test() {
  let classify = fn(input: String, flag: Bool) -> String {
    case input {
      "w" <> _ if flag -> "w"
      "wibble" <> rest if rest == "x" -> rest
      "wib" <> rest -> rest
      _ -> "other"
    }
  }

  assert classify("wabc", True) == "w"
  assert classify("wibblex", False) == "x"
  assert classify("wibbley", False) == "bley"
  assert classify("wibz", False) == "z"
  assert classify("xyz", False) == "other"
}

// https://github.com/gleam-lang/gleam/issues/5856
pub fn string_prefix_name_binding_falls_through_to_shorter_test() {
  let classify = fn(input: String) -> String {
    case input {
      "aa" <> rest if rest == "x" -> rest
      "a" as first <> rest -> rest <> first
      _ -> "other"
    }
  }

  assert classify("aax") == "x"
  assert classify("aay") == "aya"
  assert classify("abc") == "bca"
  assert classify("z") == "other"
}

// https://github.com/gleam-lang/gleam/issues/5856
pub fn string_prefix_discard_falls_through_to_shorter_test() {
  let classify = fn(input: String) -> String {
    case input {
      "aa" <> rest if rest == "x" -> rest
      "a" <> _ -> "short"
      _ -> "other"
    }
  }

  assert classify("aax") == "x"
  assert classify("aay") == "short"
  assert classify("ab") == "short"
  assert classify("z") == "other"
}

// https://github.com/gleam-lang/gleam/issues/5856
pub fn string_prefix_multibyte_falls_through_to_shorter_test() {
  let classify = fn(input: String) -> String {
    case input {
      "🫥a" <> rest if rest == "x" -> rest
      "🫥" <> rest -> rest
      _ -> "other"
    }
  }

  assert classify("🫥ax") == "x"
  assert classify("🫥ay") == "ay"
  assert classify("🫥z") == "z"
  assert classify("abc") == "other"
}

// https://github.com/gleam-lang/gleam/issues/5856
pub fn disjoint_string_prefixes_with_guard_test() {
  let classify = fn(input: String) -> String {
    case input {
      "aa" <> rest if rest == "x" -> rest
      "b" <> _ -> "b-start"
      _ -> "other"
    }
  }

  assert classify("aax") == "x"
  assert classify("aay") == "other"
  assert classify("bcd") == "b-start"
  assert classify("zzz") == "other"
}

// https://github.com/gleam-lang/gleam/issues/6168
pub fn string_literal_guard_falls_through_to_prefix_test() {
  let go = fn(input: String, x: Int) -> Int {
    case input, x {
      "ab", y if y > 4 -> 1
      "a" <> _, _ -> 400
      _, _ -> 999
    }
  }

  assert go("ab", 1) == 400
  assert go("ab", 9) == 1
  assert go("az", 1) == 400
  assert go("zz", 9) == 999
}

// https://github.com/gleam-lang/gleam/issues/6168
pub fn string_prefix_falls_through_to_shorter_prefix_without_a_guard_test() {
  let go = fn(wrap: Result(Result(Int, Int), Nil), input: String) -> String {
    case wrap, input {
      _, "ab" -> "exact"
      Ok(Ok(0)), "ab" <> _ -> "zero"
      Ok(_), "a" <> rest -> rest
      _, _ -> "other"
    }
  }

  assert go(Ok(Error(1)), "abc") == "bc"
  assert go(Ok(Ok(0)), "abc") == "zero"
  assert go(Ok(Ok(0)), "ab") == "exact"
  assert go(Error(Nil), "zz") == "other"
}

// https://github.com/gleam-lang/gleam/issues/6168
pub fn string_literal_falls_through_to_prefix_binding_rest_test() {
  let go = fn(input: String, x: Int) -> String {
    case input, x {
      "ab", y if y > 4 -> "exact"
      "a" <> rest, _ -> rest
      _, _ -> "other"
    }
  }

  assert go("ab", 1) == "b"
  assert go("ab", 9) == "exact"
  assert go("axy", 1) == "xy"
  assert go("zz", 9) == "other"
}

// https://github.com/gleam-lang/gleam/issues/6168
pub fn string_literal_dependent_bit_array_falls_through_to_prefix_test() {
  let go = fn(input: String, bits: BitArray) -> Int {
    case input, bits {
      "ab", <<length, _:bytes-size(length)>> -> 1
      "a" <> _, _ -> 2
      _, _ -> 3
    }
  }

  assert go("ab", <<2, 1>>) == 2
  assert go("ab", <<>>) == 2
  assert go("ab", <<2, 1, 2>>) == 1
  assert go("zz", <<2, 1>>) == 3
}

// https://github.com/gleam-lang/gleam/issues/6168
pub fn string_prefix_chain_falls_through_to_each_shorter_prefix_test() {
  let go = fn(wrap: Result(Result(Int, Int), Nil), input: String) -> String {
    case wrap, input {
      _, "abc" -> "exact"
      Ok(Ok(0)), "abc" <> _ -> "zero"
      Ok(Error(1)), "ab" <> _ -> "one"
      _, "a" <> rest -> rest
      _, _ -> "other"
    }
  }

  assert go(Ok(Ok(0)), "abc") == "exact"
  assert go(Ok(Ok(0)), "abcd") == "zero"
  assert go(Ok(Error(1)), "abcd") == "one"
  assert go(Ok(Error(9)), "abcd") == "bcd"
  assert go(Error(Nil), "zz") == "other"
}

// https://github.com/gleam-lang/gleam/issues/6168
pub fn guarded_string_prefix_falls_through_to_shorter_prefix_test() {
  let go = fn(input: String, y: Int) -> Int {
    case input, y {
      "abc" <> _, _ if y > 4 -> 1
      "a" <> _, _ if y > 3 -> 2
      _, 0 -> 7
      "ab" <> _, _ -> 3
      _, _ -> 9
    }
  }

  assert go("abcd", 5) == 1
  assert go("abcd", 4) == 2
  assert go("abcd", 0) == 7
  assert go("abz", 1) == 3
  assert go("zz", 1) == 9
}

// https://github.com/gleam-lang/gleam/issues/6168
pub fn string_prefix_falls_through_past_a_second_guard_test() {
  let go = fn(input: String, x: Bool, y: Bool) -> Int {
    case input {
      "wibble" <> _ if x -> 1
      "wibble" <> _ if y -> 2
      "wib" <> _ -> 3
      _ -> 4
    }
  }

  assert go("wibble", True, False) == 1
  assert go("wibble", False, True) == 2
  assert go("wibble", False, False) == 3
  assert go("wib", False, False) == 3
  assert go("zz", False, False) == 4
}

// https://github.com/gleam-lang/gleam/issues/6168
pub fn string_prefix_falls_through_when_the_fallback_binds_test() {
  let go = fn(a: String, b: String, c: Int) -> Int {
    case a, b, c {
      "ab" <> _, _, _ if c > 4 -> 1
      "a" <> _, _, _ -> 2
      _, "z" <> _, _ -> 7
      _, _, _ -> 9
    }
  }

  assert go("ab", "q", 5) == 1
  assert go("ab", "q", 1) == 2
  assert go("zz", "zq", 1) == 7
  assert go("zz", "q", 1) == 9
}

// https://github.com/gleam-lang/gleam/issues/6168
pub fn string_prefix_falls_through_to_shorter_prefix_matched_later_test() {
  let go = fn(s: String, m: Int, n: Int) -> Int {
    case s, m {
      "ab" <> _, _ if n > 4 -> 1
      "a" <> _, 1 -> 2
      "ab" <> _, 2 -> 3
      _, _ -> 4
    }
  }

  assert go("abz", 1, 5) == 1
  assert go("abz", 1, 1) == 2
  assert go("abz", 2, 1) == 3
  assert go("zz", 1, 1) == 4
}

pub fn string_prefix_assignment_inside_a_nested_pattern_after_a_guard_test() {
  let go = fn(b: Bool, r: Result(String, Nil)) -> String {
    case b, r {
      True, _ if b -> "guard"
      _, Ok("ab" as p <> rest) -> p <> rest
      _, _ -> "other"
    }
  }

  assert go(True, Ok("abc")) == "guard"
  assert go(False, Ok("abc")) == "abc"
  assert go(False, Ok("zz")) == "other"
  assert go(False, Error(Nil)) == "other"
}
