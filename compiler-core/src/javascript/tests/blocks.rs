use crate::assert_js;

#[test]
fn block() {
    assert_js!(
        r#"
fn go() {
  let x = {
    1
    2
  }
  x
}
"#,
        r#"function go() {
  let x = (() => {
    1;
    return 2;
  })();
  return x;
}
"#
    );
}

#[test]
fn sequences() {
    assert_js!(
        r#"
fn go() {
  "one"
  "two"
  "three"
}
"#,
        r#"function go() {
  "one";
  "two";
  return "three";
}
"#
    );
}

#[test]
fn left_operator_sequence() {
    assert_js!(
        r#"
fn go() {
  1 == {
    1
    2
  }
}
"#,
        r#"function go() {
  return 1 === (() => {
    1;
    return 2;
  })();
}
"#
    );
}

#[test]
fn right_operator_sequence() {
    assert_js!(
        r#"
fn go() {
  {
    1
    2
  } == 1
}
"#,
        r#"function go() {
  return (() => {
    1;
    return 2;
  })() === 1;
}
"#
    );
}
