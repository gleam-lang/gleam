use crate::assert_js;

#[test]
fn string_literals() {
    assert_js!(
        r#"
fn go() {
  "Hello, Gleam!"
}
"#,
    );
}

#[test]
fn string_patterns() {
    assert_js!(
        r#"
fn go(x) {
  let "Hello" = x
}
"#,
    );
}

#[test]
fn equality() {
    assert_js!(
        r#"
fn go(a) {
  a == "ok"
  a != "ok"
  a == a
}
"#,
    );
}

#[test]
fn case() {
    assert_js!(
        r#"
fn go(a) {
  case a {
    "" -> 0
    "one" -> 1
    "two" -> 2
    _ -> 3
  }
}
"#,
    );
}

#[test]
fn string_concat() {
    assert_js!(
        r#"
fn go() {
  "Hello, " <> "Joe"
}
"#,
    );
}

#[test]
fn string_prefix() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    "Hello, " <> name -> name
    _ -> "Unknown"
  }
}
"#,
    );
}

#[test]
fn string_prefix_utf16() {
    assert_js!(
        r#"
pub fn go(x) {
  case "Θ foo bar" {
    "Θ" <> rest -> rest
    _ -> ""
  }
  case "🫥 is neutral dotted" {
    "🫥" <> rest -> rest
    _ -> ""
  }
  case "🇺🇸 is a cluster" {
    "🇺🇸" <> rest -> rest
    _ -> ""
  }
  case "\" is a an escaped quote" {
    "\"" <> rest -> rest
    _ -> ""
  }
  case "\\ is a an escaped backslash" {
    "\\" <> rest -> rest
    _ -> ""
  }
}
"#,
    );
}

#[test]
fn discard_concat_rest_pattern() {
    // We can discard the right hand side, it parses and type checks ok
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    "Hello, " <> _ -> Nil
    _ -> Nil
  }
}
"#,
    );
}

#[test]
fn string_prefix_assignment() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    "Hello, " as greeting <> name -> greeting
    _ -> "Unknown"
  }
}
"#,
    )
}

#[test]
fn string_prefix_shadowing() {
    assert_js!(
        r#"
pub fn go(x) {
  case x {
    "Hello, " as x <> name -> x
    _ -> "Unknown"
  }
}
"#,
    )
}
