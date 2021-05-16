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
  let $ = x;
  if (!($?.length === 0)) throw new Error("Bad match");
  
  let $1 = x;
  if (!($1?.[1]?.length === 0)) throw new Error("Bad match");
  let a = $1[0];
  
  let $2 = x;
  if (!(
    $2?.[1]?.[1]?.length === 0 &&
    $2[0] === 1 &&
    $2[1][0] === 2
  )) throw new Error("Bad match");
  
  let $3 = y;
  if (!(
    $3?.[1]?.[1]?.length === 0 &&
    $3[1][0][0] === 3
  )) throw new Error("Bad match");
  let b = $3[1][0][1];
  
  let $4 = y;
  if (!($4?.[1]?.length !== undefined)) throw new Error("Bad match");
  let head = $4[0];
  let tail = $4[1];
}
"#
    );
}
