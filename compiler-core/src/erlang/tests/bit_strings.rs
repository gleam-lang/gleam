use crate::assert_erl;

#[test]
fn bit_strings() {
    assert_erl!(
        r#"pub fn main() {
  let a = 1
  let simple = <<1, a>>
  let complex = <<4:int-big, 5.0:little-float, 6:native-int>>
  let <<7:2, 8:size(3), b:binary-size(4)>> = <<1>>
  let <<c:8-unit(1), d:binary-size(2)-unit(2)>> = <<1>>

  simple
}
"#
    );

    assert_erl!(
        r#"pub fn x() { 2 }
fn main() {
  let a = -1
  let b = <<a:unit(2)-size(a * 2), a:size(3 + x())-unit(1)>>

  b
}
"#
    );

    assert_erl!(
        r#"pub fn main() {
  let a = 1
  let <<b, 1>> = <<1, a>>
  b
}
"#
    );

    assert_erl!(
        r#"pub fn main() {
  let a = <<"test":utf8>>
  let <<b:utf8_codepoint, "st":utf8>> = a
  b
}
"#
    );

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
fn bit_string_discard() {
    // https://github.com/gleam-lang/gleam/issues/704

    assert_erl!(
        r#"
pub fn bitstring_discard(x) -> Bool {
 case x {
  <<_:utf8, rest:binary>> -> True
   _ -> False
 }
}
                    "#
    );

    assert_erl!(
        r#"
pub fn bitstring_discard(x) -> Bool {
 case x {
  <<_discardme:utf8, rest:binary>> -> True
   _ -> False
 }
}
                    "#
    );
}

#[test]
fn bit_string_declare_and_use_var() {
    assert_erl!(
        r#"pub fn go(x) {
  let <<name_size:8, name:binary-size(name_size)>> = x
  name
}"#
    );
}
