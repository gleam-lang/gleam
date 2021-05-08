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
        r#""use strict";

function go(x) {
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
    [111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111]
    [11111111111111111111111111111111111111111111, 11111111111111111111111111111111111111111111111111111111111111111111111111111111]
}
"#,
        r#""use strict";

function go() {
  [111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111,
    []];
  return [11111111111111111111111111111111111111111111,
    [11111111111111111111111111111111111111111111111111111111111111111111111111111111,
      []]];
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
        r#""use strict";

function go(x) {
  return [(() => {
      true;
      return 1;
    })(),
    []];
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
        r#""use strict";

const a = [];

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
        r#""use strict";

function go(x, y) {
  let gleam$tmp = x;
  if (!(gleam$tmp?.length === 0)) throw new Error("Bad match")
  
  let gleam$tmp = x;
  if (!(gleam$tmp?.[1]?.length === 0)) throw new Error("Bad match")
  let a = gleam$tmp[0];

  let gleam$tmp = x;
  if (!(gleam$tmp?.[1]?.[1]?.length === 0 && gleam$tmp[0] === 1 && gleam$tmp[1][0] === 2)) throw new Error("Bad match")
  
  let gleam$tmp = y;
  if (!(gleam$tmp?.[1]?.[1]?.length === 0 && gleam$tmp[1][0][0] === 3)) throw new Error("Bad match")
  let b = gleam$tmp[1][0][1];

  let gleam$tmp = y;
  if (!(gleam$tmp?.[1]?.length !== undefined)) throw new Error("Bad match")
  let head = gleam$tmp[0];
  let tail = gleam$tmp[1];
}
"#
    );
}