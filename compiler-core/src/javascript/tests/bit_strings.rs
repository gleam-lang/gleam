use crate::assert_js;

#[test]
fn empty() {
    assert_js!(
        r#"
fn go() {
  <<>>
}
"#,
        r#"function go() {
  return new Uint8Array();
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
        r#"function go() {
  return $bit_string([256]);
}

function $bit_string(segments) {
  let size = segment => segment instanceof Uint8Array ? segment.byteLength : 1;
  let bytes = segments.reduce((acc, segment) => acc + size(segment), 0);
  let view = new DataView(new ArrayBuffer(bytes));
  let cursor = 0;
  for (let segment of segments) {
    if (segment instanceof Uint8Array) {
      new Uint8Array(view.buffer).set(segment, cursor);
      cursor += segment.byteLength;
    } else {
      view.setInt8(cursor, segment);
      cursor++;
    }
  }
  return new Uint8Array(view.buffer);
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
        r#"function go() {
  return $bit_string([256, 4]);
}

function $bit_string(segments) {
  let size = segment => segment instanceof Uint8Array ? segment.byteLength : 1;
  let bytes = segments.reduce((acc, segment) => acc + size(segment), 0);
  let view = new DataView(new ArrayBuffer(bytes));
  let cursor = 0;
  for (let segment of segments) {
    if (segment instanceof Uint8Array) {
      new Uint8Array(view.buffer).set(segment, cursor);
      cursor += segment.byteLength;
    } else {
      view.setInt8(cursor, segment);
      cursor++;
    }
  }
  return new Uint8Array(view.buffer);
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
        r#"function go(x) {
  return $bit_string([256, 4, x]);
}

function $bit_string(segments) {
  let size = segment => segment instanceof Uint8Array ? segment.byteLength : 1;
  let bytes = segments.reduce((acc, segment) => acc + size(segment), 0);
  let view = new DataView(new ArrayBuffer(bytes));
  let cursor = 0;
  for (let segment of segments) {
    if (segment instanceof Uint8Array) {
      new Uint8Array(view.buffer).set(segment, cursor);
      cursor += segment.byteLength;
    } else {
      view.setInt8(cursor, segment);
      cursor++;
    }
  }
  return new Uint8Array(view.buffer);
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
        r#"function go(x) {
  return $bit_string([256, 4, x, new TextEncoder().encode("Gleam")]);
}

function $bit_string(segments) {
  let size = segment => segment instanceof Uint8Array ? segment.byteLength : 1;
  let bytes = segments.reduce((acc, segment) => acc + size(segment), 0);
  let view = new DataView(new ArrayBuffer(bytes));
  let cursor = 0;
  for (let segment of segments) {
    if (segment instanceof Uint8Array) {
      new Uint8Array(view.buffer).set(segment, cursor);
      cursor += segment.byteLength;
    } else {
      view.setInt8(cursor, segment);
      cursor++;
    }
  }
  return new Uint8Array(view.buffer);
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
        r#"function go(x) {
  return $bit_string([
    new TextEncoder().encode(String.fromCodePoint(x)),
    new TextEncoder().encode("Gleam"),
  ]);
}

function $bit_string(segments) {
  let size = segment => segment instanceof Uint8Array ? segment.byteLength : 1;
  let bytes = segments.reduce((acc, segment) => acc + size(segment), 0);
  let view = new DataView(new ArrayBuffer(bytes));
  let cursor = 0;
  for (let segment of segments) {
    if (segment instanceof Uint8Array) {
      new Uint8Array(view.buffer).set(segment, cursor);
      cursor += segment.byteLength;
    } else {
      view.setInt8(cursor, segment);
      cursor++;
    }
  }
  return new Uint8Array(view.buffer);
}
"#
    );
}

#[test]
fn bit_string() {
    assert_js!(
        r#"
fn go(x) {
  <<x:bit_string, "Gleam":utf8>>
}
"#,
        r#"function go(x) {
  return $bit_string([x, new TextEncoder().encode("Gleam")]);
}

function $bit_string(segments) {
  let size = segment => segment instanceof Uint8Array ? segment.byteLength : 1;
  let bytes = segments.reduce((acc, segment) => acc + size(segment), 0);
  let view = new DataView(new ArrayBuffer(bytes));
  let cursor = 0;
  for (let segment of segments) {
    if (segment instanceof Uint8Array) {
      new Uint8Array(view.buffer).set(segment, cursor);
      cursor += segment.byteLength;
    } else {
      view.setInt8(cursor, segment);
      cursor++;
    }
  }
  return new Uint8Array(view.buffer);
}
"#
    );
}
