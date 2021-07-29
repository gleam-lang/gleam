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
    let _segments = [256];
    let _bits = new DataView(new ArrayBuffer(_segments.reduce((size, segment) =>
      size + (segment instanceof Uint8Array ? segment.byteLength : 1),
    0)));
    let _cursor = 0;
    for (let segment of _segments) {
      if (segment instanceof Uint8Array) {
        new Uint8Array(_bits.buffer).set(segment, _cursor);
        _cursor += segment.byteLength;
      } else {
        _bits.setInt8(_cursor, segment);
        _cursor++;
      }
    }
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
    let _segments = [256, 4];
    let _bits = new DataView(new ArrayBuffer(_segments.reduce((size, segment) =>
      size + (segment instanceof Uint8Array ? segment.byteLength : 1),
    0)));
    let _cursor = 0;
    for (let segment of _segments) {
      if (segment instanceof Uint8Array) {
        new Uint8Array(_bits.buffer).set(segment, _cursor);
        _cursor += segment.byteLength;
      } else {
        _bits.setInt8(_cursor, segment);
        _cursor++;
      }
    }
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
    let _segments = [256, 4, x];
    let _bits = new DataView(new ArrayBuffer(_segments.reduce((size, segment) =>
      size + (segment instanceof Uint8Array ? segment.byteLength : 1),
    0)));
    let _cursor = 0;
    for (let segment of _segments) {
      if (segment instanceof Uint8Array) {
        new Uint8Array(_bits.buffer).set(segment, _cursor);
        _cursor += segment.byteLength;
      } else {
        _bits.setInt8(_cursor, segment);
        _cursor++;
      }
    }
    return _bits.buffer;
  })();
}
"#
    );
}

#[test]
fn utf8() {
    assert_js!(
        r#"
fn go(x) {
  <<256, 4, x, "Gleam":utf8>>
}
"#,
        r#""use strict";

function go(x) {
  return (() => {
    let _segments = [256, 4, x, new TextEncoder().encode("Gleam")];
    let _bits = new DataView(new ArrayBuffer(_segments.reduce((size, segment) =>
      size + (segment instanceof Uint8Array ? segment.byteLength : 1),
    0)));
    let _cursor = 0;
    for (let segment of _segments) {
      if (segment instanceof Uint8Array) {
        new Uint8Array(_bits.buffer).set(segment, _cursor);
        _cursor += segment.byteLength;
      } else {
        _bits.setInt8(_cursor, segment);
        _cursor++;
      }
    }
    return _bits.buffer;
  })();
}
"#
    );
}

#[test]
fn utf8_codepoint() {
    assert_js!(
        r#"
fn go(x) {
  <<x:utf8_codepoint, "Gleam":utf8>>
}
"#,
        r#""use strict";

function go(x) {
  return (() => {
    let _segments = [
      new TextEncoder().encode(x),
      new TextEncoder().encode("Gleam"),
    ];
    let _bits = new DataView(new ArrayBuffer(_segments.reduce((size, segment) =>
      size + (segment instanceof Uint8Array ? segment.byteLength : 1),
    0)));
    let _cursor = 0;
    for (let segment of _segments) {
      if (segment instanceof Uint8Array) {
        new Uint8Array(_bits.buffer).set(segment, _cursor);
        _cursor += segment.byteLength;
      } else {
        _bits.setInt8(_cursor, segment);
        _cursor++;
      }
    }
    return _bits.buffer;
  })();
}
"#
    );
}
