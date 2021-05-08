use crate::assert_js;

#[test]
fn zero_arity_custom_type() {
    assert_js!(
        r#"
type Mine{
    This
    ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant
}

const this = This;
const that = ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant;

fn go() {
    This
    ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant
}
"#,
        r#""use strict";

const this = { type: "This" };

const that = {
  type: "ThatOneIsAMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchMuchLongerVariant"
};

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
fn custom_type_with_unnamed_fields() {
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
  let gleam$tmp = x;
  if (!(gleam$tmp["type"] === "Ip")) throw new Error("Bad match")
  let raw = gleam$tmp[0];
  ;
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

// fn destructure(cat) {
//   let Cat(x, y) = cat
//   let Cat(name: x, ..) = cat
//   let Cat(cuteness: 4, name: x) = cat
//   x
// }
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
