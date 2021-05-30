use crate::assert_js;

#[test]
fn zero_arity_literal() {
    assert_js!(
        r#"
type Mine{
    This
    ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant
}

fn go() {
    This
    ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant
}
"#,
        r#""use strict";

function go() {
  { type: "This" };
  return {
    type: "ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant"
  };
}
"#
    );
}

#[test]
fn zero_arity_const() {
    assert_js!(
        r#"
type Mine{
    This
    ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant
}

const this = This;
const that = ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant;
"#,
        r#""use strict";

const this = { type: "This" };

const that = {
  type: "ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant"
};
"#
    );
}

#[test]
fn zero_arity_imported() {
    assert_js!(
        (vec!["other".to_string()], r#"pub type One { Two }"#),
        r#"import other
pub fn main() {
  other.Two
}"#,
        r#""use strict";

import * as other from "./other.js";

export function main() {
  return { type: "Two" };
}
"#
    );
}

#[test]
fn zero_arity_imported_unqualified() {
    assert_js!(
        (vec!["other".to_string()], r#"pub type One { Two }"#),
        r#"import other.{Two}
pub fn main() {
  Two
}"#,
        r#""use strict";

import * as other from "./other.js";

export function main() {
  return { type: "Two" };
}
"#
    );
}

// TODO
// #[test]
// fn zero_arity_imported_unqualified_aliased() {
//     assert_js!(
//         (vec!["other".to_string()], r#"pub type One { Two }"#),
//         r#"import other.{Two as Three}
// pub fn main() {
//   Three
// }"#,
//         r#""use strict";
//
// import * as other from "./other.js";
// const { Two as Three } = other;
//
// export function main() {
//   return { type: "Two" };
// }
// "#
//     );
// }

#[test]
fn const_zero_arity_imported() {
    assert_js!(
        (vec!["other".to_string()], r#"pub type One { Two }"#),
        r#"import other
const x = other.Two
"#,
        r#""use strict";

const x = { type: "Two" };

import * as other from "./other.js";
"#
    );
}

#[test]
fn const_zero_arity_imported_unqualified() {
    assert_js!(
        (vec!["other".to_string()], r#"pub type One { Two }"#),
        r#"import other.{Two}
const a = Two
"#,
        r#""use strict";

const a = { type: "Two" };

import * as other from "./other.js";
"#
    );
}

// TODO
// #[test]
// fn const_zero_arity_imported_unqualified_aliased() {
//     assert_js!(
//         (vec!["other".to_string()], r#"pub type One { Two }"#),
//         r#"import other.{Two as Three}
// const a = Three
// "#,
//         r#""use strict";

// const a = { type: "Two" };

// import * as other from "./other.js";
// const { Two as Three } = other;
// "#
//     );
// }

#[test]
fn const_with_fields() {
    assert_js!(
        r#"
type Mine {
  Mine(a: Int, b: Int)
}

const labels = Mine(b: 2, a: 1)
const no_labels = Mine(3, 4)
"#,
        r#""use strict";

const labels = { type: "Mine", a: 1, b: 2 };

const no_labels = { type: "Mine", a: 3, b: 4 };
"#
    );
}

#[test]
fn unnamed_fields() {
    assert_js!(
        r#"
type Ip{
    Ip(String)
}

const local = Ip("0.0.0.0")

fn build(x) {
    x("1.2.3.4")
}

fn go() {
    build(Ip)
    Ip("5.6.7.8")
}

fn destructure(x) {
  let Ip(raw) = x
  raw
}
"#,
        r#""use strict";

const local = { type: "Ip", 0: "0.0.0.0" };

function build(x) {
  return x("1.2.3.4");
}

function go() {
  build((var0) => { return { type: "Ip", 0: var0 }; });
  return { type: "Ip", 0: "5.6.7.8" };
}

function destructure(x) {
  if (x.type !== "Ip") throw new Error("Bad match");
  let raw = x[0];
  
  return raw;
}
"#
    );

    assert_js!(
        r#"
type TypeWithALongNameAndSeveralArguments{
    TypeWithALongNameAndSeveralArguments(String, String, String, String, String)
}


fn go() {
    TypeWithALongNameAndSeveralArguments
}
"#,
        r#""use strict";

function go() {
  return (var0, var1, var2, var3, var4) => {
    return {
      type: "TypeWithALongNameAndSeveralArguments",
      0: var0,
      1: var1,
      2: var2,
      3: var3,
      4: var4
    };
  };
}
"#
    );
}

#[test]
fn custom_type_with_named_fields() {
    assert_js!(
        r#"
type Cat{
    Cat(name: String, cuteness: Int)
}

const felix = Cat("Felix", 12)
const tom = Cat(cuteness: 1, name: "Tom")

fn go() {
    Cat("Nubi", 1)
    Cat(2, name: "Nubi")
    Cat(cuteness: 3, name: "Nubi")
}

fn update(cat) {
    Cat(..cat, name: "Sid")
    Cat(..cat, name: "Bartholemew Wonder Puss the Fourth !!!!!!!!!!!!!!!!")
}

fn access(cat: Cat) {
    cat.cuteness
}
"#,
        r#""use strict";

const felix = { type: "Cat", name: "Felix", cuteness: 12 };

const tom = { type: "Cat", name: "Tom", cuteness: 1 };

function go() {
  { type: "Cat", name: "Nubi", cuteness: 1 };
  { type: "Cat", name: "Nubi", cuteness: 2 };
  return { type: "Cat", name: "Nubi", cuteness: 3 };
}

function update(cat) {
  Object.assign({}, cat, { name: "Sid" });
  return Object.assign(
    {},
    cat,
    { name: "Bartholemew Wonder Puss the Fourth !!!!!!!!!!!!!!!!" }
  );
}

function access(cat) {
  return cat.cuteness;
}
"#
    );
}

#[test]
fn destructure_custom_type_with_named_fields() {
    assert_js!(
        r#"
type Cat{
  Cat(name: String, cuteness: Int)
}

fn go(cat) {
  let Cat(x, y) = cat
  let Cat(name: x, ..) = cat
  let Cat(cuteness: 4, name: x) = cat
  x
}

"#,
        r#""use strict";

function go(cat) {
  if (cat.type !== "Cat") throw new Error("Bad match");
  let x = cat.name;
  let y = cat.cuteness;
  
  if (cat.type !== "Cat") throw new Error("Bad match");
  let x$1 = cat.name;
  
  if (cat.type !== "Cat" || cat.cuteness !== 4) throw new Error("Bad match");
  let x$2 = cat.name;
  
  return x$2;
}
"#
    )
}

#[test]
fn nested_pattern_with_labels() {
    assert_js!(
        r#"pub type Box(x) { Box(a: Int, b: x) }
fn go(x) {
  case x {
    Box(a: _, b: Box(a: a, b: b)) -> a + b
    _ -> 1
  }
}
"#,
        r#""use strict";

function go(x) {
  if (x.type === "Box" && x.b.type === "Box") {
    let a = x.b.a;
    let b = x.b.b;
    return a + b;
  } else {
    return 1;
  }
}
"#
    );
}

#[test]
fn imported_no_label() {
    assert_js!(
        (vec!["other".to_string()], r#"pub type One { Two(Int) }"#),
        r#"import other
pub fn main() {
  other.Two(1)
}"#,
        r#""use strict";

import * as other from "./other.js";

export function main() {
  return { type: "Two", 0: 1 };
}
"#
    );
}

#[test]
fn imported_ignoring_label() {
    assert_js!(
        (
            vec!["other".to_string()],
            r#"pub type One { Two(field: Int) }"#
        ),
        r#"import other
pub fn main() {
  other.Two(1)
}"#,
        r#""use strict";

import * as other from "./other.js";

export function main() {
  return { type: "Two", field: 1 };
}
"#
    );
}

#[test]
fn imported_using_label() {
    assert_js!(
        (
            vec!["other".to_string()],
            r#"pub type One { Two(field: Int) }"#
        ),
        r#"import other
pub fn main() {
  other.Two(field: 1)
}"#,
        r#""use strict";

import * as other from "./other.js";

export function main() {
  return { type: "Two", field: 1 };
}
"#
    );
}

#[test]
fn imported_multiple_fields() {
    assert_js!(
        (
            vec!["other".to_string()],
            r#"pub type One { Two(a: Int, b: Int, c: Int) }"#
        ),
        r#"import other
pub fn main() {
  other.Two(b: 2, c: 3, a: 1)
}"#,
        r#""use strict";

import * as other from "./other.js";

export function main() {
  return { type: "Two", a: 1, b: 2, c: 3 };
}
"#
    );
}

#[test]
fn unqualified_imported_no_label() {
    assert_js!(
        (vec!["other".to_string()], r#"pub type One { Two(Int) }"#),
        r#"import other.{Two}
pub fn main() {
  Two(1)
}"#,
        r#""use strict";

import * as other from "./other.js";

export function main() {
  return { type: "Two", 0: 1 };
}
"#
    );
}

#[test]
fn unqualified_imported_ignoring_label() {
    assert_js!(
        (
            vec!["other".to_string()],
            r#"pub type One { Two(field: Int) }"#
        ),
        r#"import other.{Two}
pub fn main() {
  Two(1)
}"#,
        r#""use strict";

import * as other from "./other.js";

export function main() {
  return { type: "Two", field: 1 };
}
"#
    );
}

#[test]
fn unqualified_imported_using_label() {
    assert_js!(
        (
            vec!["other".to_string()],
            r#"pub type One { Two(field: Int) }"#
        ),
        r#"import other.{Two}
pub fn main() {
  Two(field: 1)
}"#,
        r#""use strict";

import * as other from "./other.js";

export function main() {
  return { type: "Two", field: 1 };
}
"#
    );
}

#[test]
fn unqualified_imported_multiple_fields() {
    assert_js!(
        (
            vec!["other".to_string()],
            r#"pub type One { Two(a: Int, b: Int, c: Int) }"#
        ),
        r#"import other.{Two}
pub fn main() {
  Two(b: 2, c: 3, a: 1)
}"#,
        r#""use strict";

import * as other from "./other.js";

export function main() {
  return { type: "Two", a: 1, b: 2, c: 3 };
}
"#
    );
}

#[test]
fn constructor_as_value() {
    assert_js!(
        (
            vec!["other".to_string()],
            r#"pub type One { Two(a: Int, b: Int, c: Int) }"#
        ),
        r#"import other
pub fn main() {
  other.Two
}"#,
        r#""use strict";

import * as other from "./other.js";

export function main() {
  return (var0, var1, var2) => {
    return { type: "Two", a: var0, b: var1, c: var2 };
  };
}
"#
    );
}

#[test]
fn unqualified_constructor_as_value() {
    assert_js!(
        (
            vec!["other".to_string()],
            r#"pub type One { Two(a: Int, b: Int, c: Int) }"#
        ),
        r#"import other.{Two}
pub fn main() {
  Two
}"#,
        r#""use strict";

import * as other from "./other.js";

export function main() {
  return (var0, var1, var2) => {
    return { type: "Two", a: var0, b: var1, c: var2 };
  };
}
"#
    );
}

#[test]
fn const_imported_no_label() {
    assert_js!(
        (vec!["other".to_string()], r#"pub type One { Two(Int) }"#),
        r#"import other
pub const main = other.Two(1)
"#,
        r#""use strict";

export const main = { type: "Two", 0: 1 };

import * as other from "./other.js";
"#
    );
}

#[test]
fn const_imported_ignoring_label() {
    assert_js!(
        (
            vec!["other".to_string()],
            r#"pub type One { Two(field: Int) }"#
        ),
        r#"import other
pub const main = other.Two(1)
"#,
        r#""use strict";

export const main = { type: "Two", field: 1 };

import * as other from "./other.js";
"#
    );
}

#[test]
fn const_imported_using_label() {
    assert_js!(
        (
            vec!["other".to_string()],
            r#"pub type One { Two(field: Int) }"#
        ),
        r#"import other
pub const main = other.Two(field: 1)
"#,
        r#""use strict";

export const main = { type: "Two", field: 1 };

import * as other from "./other.js";
"#
    );
}

#[test]
fn const_imported_multiple_fields() {
    assert_js!(
        (
            vec!["other".to_string()],
            r#"pub type One { Two(a: Int, b: Int, c: Int) }"#
        ),
        r#"import other
pub const main = other.Two(b: 2, c: 3, a: 1)
"#,
        r#""use strict";

export const main = { type: "Two", a: 1, b: 2, c: 3 };

import * as other from "./other.js";
"#
    );
}

#[test]
fn const_unqualified_imported_no_label() {
    assert_js!(
        (vec!["other".to_string()], r#"pub type One { Two(Int) }"#),
        r#"import other.{Two}
pub const main = Two(1)
"#,
        r#""use strict";

export const main = { type: "Two", 0: 1 };

import * as other from "./other.js";
"#
    );
}

#[test]
fn const_unqualified_imported_ignoring_label() {
    assert_js!(
        (
            vec!["other".to_string()],
            r#"pub type One { Two(field: Int) }"#
        ),
        r#"import other.{Two}
pub const main = Two(1)
"#,
        r#""use strict";

export const main = { type: "Two", field: 1 };

import * as other from "./other.js";
"#
    );
}

#[test]
fn const_unqualified_imported_using_label() {
    assert_js!(
        (
            vec!["other".to_string()],
            r#"pub type One { Two(field: Int) }"#
        ),
        r#"import other.{Two}
pub const main = Two(field: 1)
"#,
        r#""use strict";

export const main = { type: "Two", field: 1 };

import * as other from "./other.js";
"#
    );
}

#[test]
fn const_unqualified_imported_multiple_fields() {
    assert_js!(
        (
            vec!["other".to_string()],
            r#"pub type One { Two(a: Int, b: Int, c: Int) }"#
        ),
        r#"import other.{Two}
pub const main = Two(b: 2, c: 3, a: 1)
"#,
        r#""use strict";

export const main = { type: "Two", a: 1, b: 2, c: 3 };

import * as other from "./other.js";
"#
    );
}
