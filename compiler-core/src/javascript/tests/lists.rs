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
        r#"function go(x) {
  [];
  [1, []];
  [1, [2, []]];
  return [1, [2, x]];
}
"#
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
        r#"function go() {
  [111111111111111111111111111111111111111111111111111111111111111111111111, []];
  return [
    11111111111111111111111111111111111111111111,
    [1111111111111111111111111111111111111111111,
    []],
  ];
}
"#
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
        r#"function go(x) {
  return [
    (() => {
      true;
      return 1;
    })(),
    [],
  ];
}
"#
    );
}

#[test]
fn list_constants() {
    assert_js!(
        r#"
const a = []
const b = [1, 2, 3]
"#,
        r#"const a = [];

const b = [1, [2, [3, []]]];
"#
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
        r#"function go(x, y) {
  if (x?.length !== 0) throw new Error("Bad match");
  if (x?.[1]?.length !== 0) throw new Error("Bad match");
  let a = x[0];
  if (
    x?.[1]?.[1]?.length !== 0 ||
    x[0] !== 1 ||
    x[1][0] !== 2
  ) throw new Error("Bad match");
  if (
    y?.[1]?.[1]?.length !== 0 ||
    y[1][0][0] !== 3
  ) throw new Error("Bad match");
  let b = y[1][0][1];
  if (y?.[1]?.length === undefined) throw new Error("Bad match");
  let head = y[0];
  let tail = y[1];
  return y;
}
"#
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
        r#"function go() {
  $equal([], [1, []]);
  return !$equal([], [1, []]);
}

function $equal(x, y) {
  let values = [x, y];
  while (values.length !== 0) {
    let a = values.pop();
    let b = values.pop();
    if (
      a === b ||
      (a instanceof Uint8Array &&
        b instanceof Uint8Array &&
        a.byteLength === b.byteLength &&
        a.every((x, i) => x === b[i]))
    )
      continue;
    if (!$is_object(a) || !$is_object(b) || a.length !== b.length) return false;
    for (let k of Object.keys(a)) values.push(a[k], b[k]);
  }
  return true;
}

function $is_object(object) {
  return object !== null && typeof object === 'object';
}
"#
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
        r#"function go(xs) {
  if (xs?.length === 0) {
    return 0;
  } else if (xs?.[1]?.length === 0) {
    return 1;
  } else if (xs?.[1]?.[1]?.length === 0) {
    return 2;
  } else {
    return 9999;
  }
}
"#
    );
}
