use crate::assert_js;

#[test]
fn list_literals() {
    assert_js!(
        r#"
fn go(x) {
    []
    [1]
    [1, 2]
    [1, 2, ..x]
}
"#,
    );
}

#[test]
fn long_list_literals() {
    assert_js!(
        r#"
fn go() {
  [111111111111111111111111111111111111111111111111111111111111111111111111]
  [11111111111111111111111111111111111111111111, 1111111111111111111111111111111111111111111]
}
"#,
    );
}

#[test]
fn multi_line_list_literals() {
    assert_js!(
        r#"
fn go(x) {
    [{True; 1}]
}
"#,
    );
}

#[test]
fn list_constants() {
    assert_js!(
        r#"
const a = []
const b = [1, 2, 3]
"#,
    );
}

#[test]
fn list_destructuring() {
    assert_js!(
        r#"
fn go(x, y) {
  let [] = x
  let [a] = x
  let [1, 2] = x
  let [_, #(3, b)] = y
  let [head, ..tail] = y
}
"#,
    );
}

#[test]
fn equality() {
    assert_js!(
        r#"
fn go() {
  [] == [1]
  [] != [1]
}
"#,
    );
}

#[test]
fn case() {
    assert_js!(
        r#"
fn go(xs) {
  case xs {
    [] -> 0
    [_] -> 1
    [_, _] -> 2
    _ -> 9999
  }
}
"#,
    );
}
