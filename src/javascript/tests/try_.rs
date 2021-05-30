use crate::assert_js;

#[test]
fn top() {
    assert_js!(
        r#"pub fn main(x) {
  try y = x
  try z = y
  Ok(z)
}"#,
        r#""use strict";

export function main(x) {
  if (x.type === "Error") return x;
  let y = x[0];

  if (y.type === "Error") return y;
  let z = y[0];

  return { type: "Ok", 0: z };
}
"#
    )
}

#[test]
fn rebinding() {
    assert_js!(
        r#"pub fn main(x) {
  try x = x
  try x = x
  Ok(x)
}"#,
        r#""use strict";

export function main(x) {
  if (x.type === "Error") return x;
  let x$1 = x[0];

  if (x$1.type === "Error") return x$1;
  let x$2 = x$1[0];

  return { type: "Ok", 0: x$2 };
}
"#
    )
}

#[test]
fn discard() {
    assert_js!(
        r#"pub fn main(x, y) {
  try _ = x
  try _ = y
  x
}"#,
        r#""use strict";

export function main(x, y) {
  if (x.type === "Error") return x;
  if (y.type === "Error") return y;
  return x;
}
"#
    )
}
