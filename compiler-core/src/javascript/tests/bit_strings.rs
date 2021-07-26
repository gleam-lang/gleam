use crate::assert_js;

#[test]
fn empty() {
    assert_js!(
        r#"
fn go() {
  <<>>
}
"#,
        r#""use strict";

function go() {
  return new ArrayBuffer(0);
}
"#
    );
}

#[test]
fn one() {
    assert_js!(
        r#"
fn go() {
  <<256>>
}
"#,
        r#""use strict";

function go() {
  return (() => {
    let _bits = new DataView(new ArrayBuffer(1));
    _bits.setInt8(0, 256);
    return _bits.buffer;
  })();
}
"#
    );
}

#[test]
fn two() {
    assert_js!(
        r#"
fn go() {
  <<256, 4>>
}
"#,
        r#""use strict";

function go() {
  return (() => {
    let _bits = new DataView(new ArrayBuffer(2));
    _bits.setInt8(0, 256);
    _bits.setInt8(1, 4);
    return _bits.buffer;
  })();
}
"#
    );
}

#[test]
fn variable() {
    assert_js!(
        r#"
fn go(x) {
  <<256, 4, x>>
}
"#,
        r#""use strict";

function go(x) {
  return (() => {
    let _bits = new DataView(new ArrayBuffer(3));
    _bits.setInt8(0, 256);
    _bits.setInt8(1, 4);
    _bits.setInt8(2, x);
    return _bits.buffer;
  })();
}
"#
    );
}
