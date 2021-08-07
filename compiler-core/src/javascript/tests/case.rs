use crate::assert_js;

// https://github.com/gleam-lang/gleam/issues/1187
#[test]
fn pointless() {
    assert_js!(
        r#"
fn go(x) {
  case x {
    _ -> x
  }
}
"#,
        r#"function go(x) {
  return x;
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/1188
#[test]
fn following_todo() {
    assert_js!(
        r#"
fn go(x) {
  case x {
    True -> todo
    _ -> 1
  }
}
"#,
        r#"function go(x) {
  if (x) {
    throw Object.assign(
      new Error("This has not yet been implemented"),
      { gleam_error: "todo", module: "my/mod", function: "go", line: 4 }
    );
  } else {
    return 1;
  }
}
"#
    )
}

#[test]
fn multi_subject_catch_all() {
    assert_js!(
        r#"
fn go(x, y) {
  case x, y {
    True, True -> 1
    _, _ -> 0
  }
}
"#,
        r#"function go(x, y) {
  if (x && y) {
    return 1;
  } else {
    return 0;
  }
}
"#
    )
}

#[test]
fn multi_subject_or() {
    assert_js!(
        r#"
fn go(x, y) {
  case x, y {
    True, _ | _, True -> 1
    _, _ -> 0
  }
}
"#,
        r#"function go(x, y) {
  if (x) {
    return 1;
  } else if (y) {
    return 1;
  } else {
    return 0;
  }
}
"#
    )
}

#[test]
fn multi_subject_no_catch_all() {
    assert_js!(
        r#"
fn go(x, y) {
  case x, y {
    True, _ -> 1
    _, True -> 2
    False, False -> 0
  }
}
"#,
        r#"function go(x, y) {
  if (x) {
    return 1;
  } else if (y) {
    return 2;
  } else if (!x && !y) {
    return 0;
  } else {
    throw new Error("Bad match");
  }
}
"#
    )
}

#[test]
fn multi_subject_subject_assignments() {
    assert_js!(
        r#"
fn go() {
  case True, False {
    True, True -> 1
    _, _ -> 0
  }
}
"#,
        r#"function go() {
  let $ = true;
  let $1 = false;
  if ($ && $1) {
    return 1;
  } else {
    return 0;
  }
}
"#
    )
}

#[test]
fn assignment() {
    assert_js!(
        r#"
fn go(x) {
  let y = case x {
    True -> 1
    _ -> 0
  }
  y
}
"#,
        r#"function go(x) {
  let y = (() => {
    if (x) {
      return 1;
    } else {
      return 0;
    }
  })();
  return y;
}
"#
    )
}

#[test]
fn preassign_assignment() {
    assert_js!(
        r#"
fn go(x) {
  let y = case x() {
    True -> 1
    _ -> 0
  }
  y
}
"#,
        r#"function go(x) {
  let y = (() => {
    let $ = x();
    if ($) {
      return 1;
    } else {
      return 0;
    }
  })();
  return y;
}
"#
    )
}
