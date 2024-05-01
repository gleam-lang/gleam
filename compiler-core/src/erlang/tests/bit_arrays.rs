use crate::assert_erl;

#[test]
fn bit_array() {
    assert_erl!(
        r#"pub fn main() {
  let a = 1
  let simple = <<1, a>>
  let complex = <<4:int-big, 5.0:little-float, 6:native-int>>
  let assert <<7:2, 8:size(3), b:bytes-size(4)>> = <<1>>
  let assert <<c:8-unit(1), d:bytes-size(2)-unit(2)>> = <<1>>

  simple
}
"#
    );
}

#[test]
fn bit_array_float() {
    assert_erl!(
        r#"pub fn main() {
  let b = 16
  let floats = <<1.0:16-float, 5.0:float-32, 6.0:float-64-little, 1.0:float-size(b)>>
  let assert <<1.0:16-float, 5.0:float-32, 6.0:float-64-little, 1.0:float-size(b)>> = floats 
}"#
    );
}

#[test]
fn bit_array1() {
    assert_erl!(
        r#"pub fn x() { 2 }
fn main() {
  let a = -1
  let b = <<a:unit(2)-size(a * 2), a:size(3 + x())-unit(1)>>

  b
}
"#
    );
}

#[test]
fn bit_array2() {
    assert_erl!(
        r#"pub fn main() {
  let a = 1
  let assert <<b, 1>> = <<1, a>>
  b
}
"#
    );
}

#[test]
fn bit_array3() {
    assert_erl!(
        r#"pub fn main() {
  let a = <<"test":utf8>>
  let assert <<b:utf8_codepoint, "st":utf8>> = a
  b
}
"#
    );
}

#[test]
fn bit_array4() {
    assert_erl!(
        r#"fn x() { 1 }
pub fn main() {
  let a = <<x():int>>
  a
}
"#
    );
}

#[test]
fn bit_array5() {
    assert_erl!(
        r#"const bit_size = 8
pub fn main() {
  let a = <<10:size(bit_size)>>
  a
}
"#
    );
}

#[test]
fn bit_array_discard() {
    // https://github.com/gleam-lang/gleam/issues/704

    assert_erl!(
        r#"
pub fn bit_array_discard(x) -> Bool {
 case x {
  <<_:utf8, rest:bytes>> -> True
   _ -> False
 }
}
                    "#
    );
}

#[test]
fn bit_array_discard1() {
    assert_erl!(
        r#"
pub fn bit_array_discard(x) -> Bool {
 case x {
  <<_discardme:utf8, rest:bytes>> -> True
   _ -> False
 }
}
"#
    );
}

#[test]
fn bit_array_declare_and_use_var() {
    assert_erl!(
        r#"pub fn go(x) {
  let assert <<name_size:8, name:bytes-size(name_size)>> = x
  name
}"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3050
#[test]
fn unicode_bit_array_1() {
    assert_erl!(
        r#"
    pub fn main() {
        let emoji = "\u{1F600}"
        let arr = <<emoji:utf8>>
}"#
    );
}

#[test]
fn unicode_bit_array_2() {
    assert_erl!(
        r#"
    pub fn main() {
        let arr = <<"\u{1F600}":utf8>>
}"#
    );
}
