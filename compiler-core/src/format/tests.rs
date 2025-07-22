use pretty_assertions::assert_eq;

mod asignments;
mod binary_operators;
mod bit_array;
mod blocks;
mod cases;
mod conditional_compilation;
mod custom_type;
mod echo;
mod external_fn;
mod external_types;
mod function;
mod guards;
mod imports;
mod lists;
mod pipeline;
mod record_update;
mod tuple;
mod use_;

#[macro_export]
macro_rules! assert_format {
    ($src:expr $(,)?) => {
        let mut writer = String::new();
        $crate::format::pretty(&mut writer, &$src.into(), camino::Utf8Path::new("<stdin>"))
            .unwrap();
        assert_eq!($src, writer);
    };
}

#[macro_export]
macro_rules! assert_format_rewrite {
    ($src:expr, $expected:expr  $(,)?) => {
        let mut writer = String::new();
        $crate::format::pretty(&mut writer, &$src.into(), camino::Utf8Path::new("<stdin>"))
            .unwrap();
        assert_eq!(writer, $expected);
    };
}

#[test]
fn imports() {
    assert_format!("\n");
    assert_format!("import one\n");
    assert_format!("import one\nimport two\n");
    assert_format!("import one/two/three\n");
    assert_format!("import four/five\nimport one/two/three\n");
    assert_format!("import one.{fun, fun2, fun3}\n");
    assert_format!("import one.{One, Two, fun1, fun2}\n");
    assert_format!("import one.{main as entrypoint}\n");
    assert_format!("import one/two/three as free\n");
    assert_format!("import one/two/three.{thunk} as free\n");
    assert_format!("import one/two/three.{thunk as funky} as free\n");
    assert_format!(
        "import my/cool/module.{
  Ane, Bwo, Chree, Dour, Eive, Fix, Geven, Hight, Iine, Jen, Kleven, Lwelve,
  Mhirteen, Nifteen, Oixteen,
}
"
    );
    assert_format!(
        "import gleam/result.{
  Aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, Abcde,
  End,
}
"
    );
}

#[test]
fn multiple_statements_test() {
    assert_format!(
        r#"import one
import three
import two

pub type One

pub type Two

pub type Three

pub type Four
"#
    );
}

#[test]
fn type_alias() {
    assert_format!(
        "type Option(a) =
  Result(a, Nil)
"
    );

    assert_format!(
        "pub type Option(a) =
  Result(a, Nil)
"
    );

    assert_format!(
        "pub type Pair(a, b) =
  #(a, b)
"
    );

    assert_format!(
        "pub type Sixteen(element) =
  #(
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
  )
"
    );

    assert_format!(
        "pub type Sixteen(element) =
  fn(
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
    element,
  ) ->
    #(
      element,
      element,
      element,
      element,
      element,
      element,
      element,
      element,
      element,
      element,
      element,
      element,
      element,
      element,
      element,
      element,
    )
"
    );

    //    assert_format!(
    //        "pub type Curried(element) =
    //  fn() ->
    //  elementttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt
    //"
    //    );

    //    assert_format!(
    //        "pub type Sixteen(element) =
    //  fn(element) ->
    //  #(
    //    element,
    //    element,
    //    element,
    //    element,
    //    element,
    //    element,
    //    element,
    //    element,
    //    element,
    //    element,
    //    element,
    //    element,
    //    element,
    //    element,
    //    element,
    //    element,
    //  )
    //"
    //    );

    assert_format!(
        "pub type Curried(element) =
  fn(element) -> fn(element) -> element
"
    );

    //    assert_format!(
    //        "pub type Curried(element) =
    //  fn(element)
    //  -> fn(element)
    //  -> fn(element)
    //  -> fn(element)
    //  -> fn(element)
    //  -> element
    //"
    //    );

    assert_format!(
        "type WowThisTypeHasJustTheLongestName =
  WowThisTypeHasAnEvenLongerNameHowIsThatPossible
"
    );

    assert_format!(
        "type WowThisTypeHasJustTheLongestName =
  Container(
    Int,
    String,
    List(a),
    SomethingElse,
    WowThisTypeHasJustTheLongestName,
  )
"
    );

    assert_format!(
        "type WowThisTypeHasJustTheLongestName(
  some_long_type_variable,
  and_another,
  and_another_again,
) =
  Container(
    Int,
    String,
    List(a),
    SomethingElse,
    WowThisTypeHasJustTheLongestName,
  )
"
    );

    assert_format!(
        "///
type Many(a) =
  List(a)
"
    );
}

#[test]
fn expr_fn() {
    assert_format!(
        r#"fn main() {
  fn(x) { x }
}
"#
    );

    assert_format!(
        r#"fn main() {
  fn(_) { x }
}
"#
    );

    assert_format!(
        r#"fn main() {
  fn(_discarded) { x }
}
"#
    );

    assert_format!(
        r#"fn main() {
  fn() {
    1
    2
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  fn() {
    let y = x
    y
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  fn() {
    let x: Int = 1
    x
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  fn() {
    let x: Box(_) = call()
    x
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  fn() {
    let x: Box(_whatever) = call()
    x
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  fn(_) {
    1
    2
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  fn(_) -> Int {
    1
    2
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  fn(_: Int) -> Int { 2 }
}
"#
    );

    assert_format!(
        r#"fn main() {
  fn(x) {
    case x {
      Ok(i) -> i + 1
      Error(_) -> 0
    }
  }
}
"#
    );
}

#[test]
fn expr_call() {
    assert_format!(
        r#"fn main() {
  run()
}
"#
    );

    assert_format!(
        r#"fn main() {
  run(1)
}
"#
    );

    assert_format!(
        r#"fn main() {
  run(with: 1)
}
"#
    );

    assert_format!(
        r#"fn main() {
  run(
    with: 1,
    loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong: 1,
  )
}
"#
    );

    assert_format!(
        r#"fn main() {
  run(
    with: something(1, 2, 3),
    loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong: 1,
  )
}
"#
    );

    assert_format!(
        r#"fn main() {
  run(
    with: something(
      loooooooooooooooooooooooooooooooooooooooong: 1,
      looooooooooooooooooooooooooooooooooooooooong: 2,
    ),
    loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong: 1,
  )
}
"#
    );

    assert_format!(
        "fn main() {
  succ(1)
}
"
    );

    assert_format!(
        "fn main() {
  add(1)(2)(3)
}
"
    );

    assert_format!(
        "fn main() {
  Ok(1)
}
"
    );

    assert_format!(
        "fn main() {
  Ok(1, {
    1
    2
  })
}
"
    );

    assert_format!(
        r#"fn main() {
  Person("Al", is_cool: VeryTrue)
}
"#
    );

    assert_format!(
        r#"fn main() {
  Person(name: "Al", is_cool: VeryTrue)
}
"#
    );
}

#[test]
fn compact_single_argument_call() {
    assert_format!(
        r#"fn main() {
  thingy(fn(x) {
    1
    2
  })
}
"#
    );

    assert_format!(
        r#"fn main() {
  thingy([
    // ok!
    one(),
    two(),
  ])
}
"#
    );

    assert_format!(
        r#"fn main() {
  thingy(<<
    // ok!
    one(),
    two(),
  >>)
}
"#
    );

    assert_format!(
        r#"fn main() {
  thingy(#(
    // ok!
    one(),
    two(),
  ))
}
"#
    );

    assert_format!(
        r#"fn main() {
  thingy(
    wiggle(my_function(
      // ok!
      one(),
      two(),
    )),
  )
}
"#
    );

    assert_format!(
        r#"fn main() {
  thingy(case x {
    1 -> 1
    _ -> 0
  })
}
"#
    );

    assert_format!(
        r#"fn main() {
  thingy({
    1
    2
    3
  })
}
"#
    );

    assert_format!(
        r#"fn main() {
  thingy({
    let x = 1
    x
  })
}
"#
    );
}

#[test]
fn expr_tuple() {
    assert_format!(
        r#"fn main(one, two, three) {
  #(1, {
    1
    2
  })
}
"#
    );

    assert_format!(
        r#"fn main() {
  #(
    atom.create_from_string("module"),
    atom.create_from_string("gleam@otp@actor"),
  )
}
"#
    );

    assert_format!(
        r#"fn main() {
  #()
}
"#
    );

    assert_format!(
        r#"fn main() {
  #(1)
}
"#
    );

    assert_format!(
        r#"fn main() {
  #(1, 2)
}
"#
    );

    assert_format!(
        r#"fn main() {
  #(1, 2, 3)
}
"#
    );

    assert_format!(
        r#"fn main() {
  #(
    really_long_variable_name,
    really_long_variable_name,
    really_long_variable_name,
    really_long_variable_name,
    really_long_variable_name,
  )
}
"#
    );
}

#[test]
fn statement_fn() {
    assert_format!(
        r#"fn main(one, two, three) {
  Nil
}
"#
    );
}

#[test]
fn statement_fn1() {
    assert_format!(
        r#"fn main(label_one one, label_two two, label_three three) {
  Nil
}
"#
    );
}

#[test]
fn statement_fn2() {
    assert_format!(
        r#"fn main(label_one one: One, label_two two: Two) {
  Nil
}
"#
    );
}

#[test]
fn statement_fn3() {
    assert_format!(
        r#"fn main(
  label_one one: One,
  label_two two: Two,
  label_three three: Three,
  label_four four: Four,
) {
  Nil
}
"#
    );

    assert_format!(
        r#"fn main(_discarded) {
  Nil
}
"#
    );
}

#[test]
fn statement_fn4() {
    assert_format!(
        r#"fn main(label _discarded) {
  Nil
}
"#
    );
}

#[test]
fn statement_fn5() {
    // https://github.com/gleam-lang/gleam/issues/613
    assert_format!(
        r#"fn main() {
  Nil
  // Done
}
"#
    );
}

#[test]
fn statement_fn6() {
    //
    // Module function return annotations
    //

    assert_format!(
        r#"fn main() -> Nil {
  Nil
}
"#
    );
}

#[test]
fn statement_fn7() {
    assert_format!(
        r#"fn main() -> Loooooooooooooooooooong(
  Looooooooooooooong,
  Looooooooooooooooooong,
  Loooooooooooooooooooooong,
  Looooooooooooooooooooooooong,
) {
  Nil
}
"#
    );
}

#[test]
fn statement_fn8() {
    assert_format!(
        r#"fn main() -> Loooooooooooooooooooong(
  Loooooooooooooooooooooooooooooooooooooooooong,
) {
  Nil
}
"#
    );
}

#[test]
fn statement_fn9() {
    assert_format!(
        r#"fn main() -> program.Exit {
  Nil
}
"#
    );
}

#[test]
fn statement_fn10() {
    assert_format!(
        "fn order(
  first: Set(member),
  second: Set(member),
) -> #(Set(member), Set(member), a) {
  Nil
}
"
    );
}

#[test]
fn statement_fn11() {
    assert_format!(
        "///
pub fn try_map(
  over list: List(a),
  with fun: fn(a) -> Result(b, e),
) -> Result(List(b), e) {
  Nil
}
"
    );
}

#[test]
fn binary_operators() {
    assert_format!(
        r#"fn main() {
  True && False
}
"#
    );

    assert_format!(
        r#"fn main() {
  True || False
}
"#
    );

    assert_format!(
        r#"fn main() {
  1 < 1
}
"#
    );

    assert_format!(
        r#"fn main() {
  1 <= 1
}
"#
    );

    assert_format!(
        r#"fn main() {
  1.0 <. 1.0
}
"#
    );

    assert_format!(
        r#"fn main() {
  1.0 <=. 1.0
}
"#
    );

    assert_format!(
        r#"fn main() {
  1 == 1
}
"#
    );

    assert_format!(
        r#"fn main() {
  1 != 1
}
"#
    );

    assert_format!(
        r#"fn main() {
  1 >= 1
}
"#
    );

    assert_format!(
        r#"fn main() {
  1 > 1
}
"#
    );

    assert_format!(
        r#"fn main() {
  1.0 >=. 1.0
}
"#
    );

    assert_format!(
        r#"fn main() {
  1.0 >. 1.0
}
"#
    );

    assert_format!(
        r#"fn main() {
  1 + 1
}
"#
    );

    assert_format!(
        r#"fn main() {
  1.0 +. 1.0
}
"#
    );

    assert_format!(
        r#"fn main() {
  1 - 1
}
"#
    );

    assert_format!(
        r#"fn main() {
  1.0 -. 1.0
}
"#
    );

    assert_format!(
        r#"fn main() {
  1 * 1
}
"#
    );

    assert_format!(
        r#"fn main() {
  1.0 *. 1.0
}
"#
    );

    assert_format!(
        r#"fn main() {
  1 / 1
}
"#
    );

    assert_format!(
        r#"fn main() {
  1.0 /. 1.0
}
"#
    );

    assert_format!(
        r#"fn main() {
  1 % 1
}
"#
    );
}

#[test]
fn expr_int() {
    assert_format!(
        r#"fn i() {
  1
}
"#
    );

    assert_format!(
        r#"fn i() {
  121_234_345_989_000
}
"#
    );

    assert_format!(
        r#"fn i() {
  -12_928_347_925
}
"#
    );

    assert_format!(
        r#"fn i() {
  1_234_567_890
}
"#
    );

    assert_format!(
        r#"fn i() {
  123_456_789
}
"#
    );

    assert_format!(
        r#"fn i() {
  12_345_678
}
"#
    );

    assert_format!(
        r#"fn i() {
  1_234_567
}
"#
    );

    assert_format!(
        r#"fn i() {
  123_456
}
"#
    );

    assert_format!(
        r#"fn i() {
  12_345
}
"#
    );

    assert_format!(
        r#"fn i() {
  1234
}
"#
    );

    assert_format!(
        r#"fn i() {
  123
}
"#
    );

    assert_format!(
        r#"fn i() {
  12
}
"#
    );

    assert_format!(
        r#"fn i() {
  1
}
"#
    );

    assert_format!(
        r#"fn i() {
  -1_234_567_890
}
"#
    );

    assert_format!(
        r#"fn i() {
  -123_456_789
}
"#
    );

    assert_format!(
        r#"fn i() {
  -12_345_678
}
"#
    );

    assert_format!(
        r#"fn i() {
  -1_234_567
}
"#
    );

    assert_format!(
        r#"fn i() {
  -123_456
}
"#
    );

    assert_format!(
        r#"fn i() {
  -12_345
}
"#
    );

    assert_format!(
        r#"fn i() {
  -1234
}
"#
    );

    assert_format!(
        r#"fn i() {
  -123
}
"#
    );

    assert_format!(
        r#"fn i() {
  -12
}
"#
    );

    assert_format!(
        r#"fn i() {
  -1
}
"#
    );

    assert_format_rewrite!(
        r#"fn i() {
  1_234
}
"#,
        r#"fn i() {
  1234
}
"#
    );

    assert_format_rewrite!(
        r#"fn i() {
  12_34
}
"#,
        r#"fn i() {
  1234
}
"#
    );

    assert_format_rewrite!(
        r#"fn i() {
  123_4
}
"#,
        r#"fn i() {
  1234
}
"#
    );

    assert_format_rewrite!(
        r#"fn i() {
  1234_5
}
"#,
        r#"fn i() {
  12_345
}
"#
    );

    assert_format_rewrite!(
        r#"fn i() {
  12345_6
}
"#,
        r#"fn i() {
  123_456
}
"#
    );

    assert_format_rewrite!(
        r#"fn i() {
  123456_7
}
"#,
        r#"fn i() {
  1_234_567
}
"#
    );

    assert_format_rewrite!(
        r#"fn i() {
  1234567_8
}
"#,
        r#"fn i() {
  12_345_678
}
"#
    );

    assert_format_rewrite!(
        r#"fn i() {
  -1_234
}
"#,
        r#"fn i() {
  -1234
}
"#
    );

    assert_format_rewrite!(
        r#"fn i() {
  -12_34
}
"#,
        r#"fn i() {
  -1234
}
"#
    );

    assert_format_rewrite!(
        r#"fn i() {
  -123_4
}
"#,
        r#"fn i() {
  -1234
}
"#
    );

    assert_format_rewrite!(
        r#"fn i() {
  -1234_5
}
"#,
        r#"fn i() {
  -12_345
}
"#
    );

    assert_format_rewrite!(
        r#"fn i() {
  -12345_6
}
"#,
        r#"fn i() {
  -123_456
}
"#
    );

    assert_format_rewrite!(
        r#"fn i() {
  -123456_7
}
"#,
        r#"fn i() {
  -1_234_567
}
"#
    );

    assert_format_rewrite!(
        r#"fn i() {
  -1234567_8
}
"#,
        r#"fn i() {
  -12_345_678
}
"#
    );

    assert_format_rewrite!(
        r#"fn i() {
  let #(1_234, _) = #(1_234, Nil)
}
"#,
        r#"fn i() {
  let #(1234, _) = #(1234, Nil)
}
"#
    );
    assert_format_rewrite!(
        r#"fn i() {
  let #(12_34, _) = #(12_34, Nil)
}
"#,
        r#"fn i() {
  let #(1234, _) = #(1234, Nil)
}
"#
    );
    assert_format_rewrite!(
        r#"fn i() {
  let #(1234567_8, _) = #(1234567_8, Nil)
}
"#,
        r#"fn i() {
  let #(12_345_678, _) = #(12_345_678, Nil)
}
"#
    );
    assert_format_rewrite!(
        r#"fn i() {
  let #(-1_234, _) = #(-1_234, Nil)
}
"#,
        r#"fn i() {
  let #(-1234, _) = #(-1234, Nil)
}
"#
    );
    assert_format_rewrite!(
        r#"fn i() {
  let #(-12_34, _) = #(-12_34, Nil)
}
"#,
        r#"fn i() {
  let #(-1234, _) = #(-1234, Nil)
}
"#
    );
    assert_format_rewrite!(
        r#"fn i() {
  let #(-1234567_8, _) = #(-1234567_8, Nil)
}
"#,
        r#"fn i() {
  let #(-12_345_678, _) = #(-12_345_678, Nil)
}
"#
    );

    assert_format_rewrite!(
        r#"const an_int = 1_234
"#,
        r#"const an_int = 1234
"#
    );
    assert_format_rewrite!(
        r#"const an_int = 12_34
"#,
        r#"const an_int = 1234
"#
    );
    assert_format_rewrite!(
        r#"const an_int = 1234567_8
"#,
        r#"const an_int = 12_345_678
"#
    );
    assert_format_rewrite!(
        r#"const an_int = -1_234
"#,
        r#"const an_int = -1234
"#
    );
    assert_format_rewrite!(
        r#"const an_int = -12_34
"#,
        r#"const an_int = -1234
"#
    );
    assert_format_rewrite!(
        r#"const an_int = -1234567_8
"#,
        r#"const an_int = -12_345_678
"#
    );

    assert_format!("fn n() {\n  1_234_567\n}\n");
    assert_format!("fn h() {\n  0xCAB005E\n}\n");
    assert_format!("fn h() {\n  0xC_AB_00_5E\n}\n");
    assert_format!("fn h() {\n  0xCA_B0_05_E\n}\n");
    assert_format!("fn b() {\n  0b10100001\n}\n");
    assert_format!("fn b() {\n  0b_1010_0001\n}\n");
    assert_format!("fn o() {\n  0o1234567\n}\n");
    assert_format!("fn o() {\n  0o1_234_567\n}\n");
    assert_format!("fn o() {\n  0o_123_456_7\n}\n");
}

#[test]
fn expr_float() {
    assert_format_rewrite!(
        r#"fn f() {
  1.
}
"#,
        r#"fn f() {
  1.0
}
"#
    );

    assert_format_rewrite!(
        r#"fn f() {
  1.00
}
"#,
        r#"fn f() {
  1.0
}
"#
    );

    assert_format_rewrite!(
        r#"fn f() {
  1.00100
}
"#,
        r#"fn f() {
  1.001
}
"#
    );

    assert_format_rewrite!(
        r#"fn f() {
  1.001001
}
"#,
        r#"fn f() {
  1.001001
}
"#
    );

    assert_format_rewrite!(
        r#"fn f() {
  1.00e100_100
}
"#,
        r#"fn f() {
  1.0e100_100
}
"#
    );

    assert_format_rewrite!(
        r#"fn f() {
  1.00100e100_100
}
"#,
        r#"fn f() {
  1.001e100_100
}
"#
    );

    assert_format_rewrite!(
        r#"fn f() {
  1.001001e100_100
}
"#,
        r#"fn f() {
  1.001001e100_100
}
"#
    );

    assert_format!(
        r#"fn f() {
  1.0
}
"#
    );

    assert_format!(
        r#"fn f() {
  -1.0
}
"#
    );

    assert_format!(
        r#"fn f() {
  9999.6666
}
"#
    );

    assert_format!(
        r#"fn f() {
  -1_234_567_890.0
}
"#
    );
    assert_format!(
        r#"fn f() {
  -1_234_567_890.0
}
"#
    );
    assert_format!(
        r#"fn f() {
  -123_456_789.0
}
"#
    );
    assert_format!(
        r#"fn f() {
  -12_345_678.0
}
"#
    );
    assert_format!(
        r#"fn f() {
  -1_234_567.0
}
"#
    );
    assert_format!(
        r#"fn f() {
  -123_456.0
}
"#
    );
    assert_format!(
        r#"fn f() {
  -12_345.0
}
"#
    );
    assert_format!(
        r#"fn f() {
  -1234.0
}
"#
    );
    assert_format!(
        r#"fn f() {
  -123.0
}
"#
    );
    assert_format!(
        r#"fn f() {
  -12.0
}
"#
    );
    assert_format!(
        r#"fn f() {
  -1.0
}
"#
    );
    assert_format!(
        r#"fn f() {
  -0.0
}
"#
    );
    assert_format!(
        r#"fn f() {
  -1_234_567_890.1
}
"#
    );
    assert_format!(
        r#"fn f() {
  -123_456_789.1
}
"#
    );
    assert_format!(
        r#"fn f() {
  -12_345_678.1
}
"#
    );
    assert_format!(
        r#"fn f() {
  -1_234_567.1
}
"#
    );
    assert_format!(
        r#"fn f() {
  -123_456.1
}
"#
    );
    assert_format!(
        r#"fn f() {
  -12_345.1
}
"#
    );
    assert_format!(
        r#"fn f() {
  -1234.1
}
"#
    );
    assert_format!(
        r#"fn f() {
  -123.1
}
"#
    );
    assert_format!(
        r#"fn f() {
  -12.1
}
"#
    );
    assert_format!(
        r#"fn f() {
  -1.1
}
"#
    );
    assert_format!(
        r#"fn f() {
  -0.1
}
"#
    );
    assert_format!(
        r#"fn f() {
  -1_234_567_890.123456
}
"#
    );
    assert_format!(
        r#"fn f() {
  -123_456_789.123456
}
"#
    );
    assert_format!(
        r#"fn f() {
  -12_345_678.123456
}
"#
    );
    assert_format!(
        r#"fn f() {
  -1_234_567.123456
}
"#
    );
    assert_format!(
        r#"fn f() {
  -123_456.123456
}
"#
    );
    assert_format!(
        r#"fn f() {
  -12_345.123456
}
"#
    );
    assert_format!(
        r#"fn f() {
  -1234.123456
}
"#
    );
    assert_format!(
        r#"fn f() {
  -123.123456
}
"#
    );
    assert_format!(
        r#"fn f() {
  -12.123456
}
"#
    );
    assert_format!(
        r#"fn f() {
  -1.123456
}
"#
    );
    assert_format!(
        r#"fn f() {
  -0.123456
}
"#
    );

    assert_format_rewrite!(
        r#"fn f() {
  1_234.0
}
"#,
        r#"fn f() {
  1234.0
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  12_34.0
}
"#,
        r#"fn f() {
  1234.0
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  123_4.0
}
"#,
        r#"fn f() {
  1234.0
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  1234_5.0
}
"#,
        r#"fn f() {
  12_345.0
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  12345_6.0
}
"#,
        r#"fn f() {
  123_456.0
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  123456_7.0
}
"#,
        r#"fn f() {
  1_234_567.0
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  1234567_8.0
}
"#,
        r#"fn f() {
  12_345_678.0
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  -1_234.0
}
"#,
        r#"fn f() {
  -1234.0
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  -12_34.0
}
"#,
        r#"fn f() {
  -1234.0
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  -123_4.0
}
"#,
        r#"fn f() {
  -1234.0
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  -1234_5.0
}
"#,
        r#"fn f() {
  -12_345.0
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  -12345_6.0
}
"#,
        r#"fn f() {
  -123_456.0
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  -123456_7.0
}
"#,
        r#"fn f() {
  -1_234_567.0
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  -1234567_8.0
}
"#,
        r#"fn f() {
  -12_345_678.0
}
"#
    );

    assert_format_rewrite!(
        r#"fn f() {
  let #(1_234.0, _) = #(1_234.0, Nil)
}
"#,
        r#"fn f() {
  let #(1234.0, _) = #(1234.0, Nil)
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  let #(12_34.0, _) = #(12_34.0, Nil)
}
"#,
        r#"fn f() {
  let #(1234.0, _) = #(1234.0, Nil)
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  let #(1234567_8.0, _) = #(1234567_8.0, Nil)
}
"#,
        r#"fn f() {
  let #(12_345_678.0, _) = #(12_345_678.0, Nil)
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  let #(-1_234.0, _) = #(-1_234.0, Nil)
}
"#,
        r#"fn f() {
  let #(-1234.0, _) = #(-1234.0, Nil)
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  let #(-12_34.0, _) = #(-12_34.0, Nil)
}
"#,
        r#"fn f() {
  let #(-1234.0, _) = #(-1234.0, Nil)
}
"#
    );
    assert_format_rewrite!(
        r#"fn f() {
  let #(-1234567_8.0, _) = #(-1234567_8.0, Nil)
}
"#,
        r#"fn f() {
  let #(-12_345_678.0, _) = #(-12_345_678.0, Nil)
}
"#
    );
    assert_format_rewrite!(
        r#"const a_float = 1_234.0
"#,
        r#"const a_float = 1234.0
"#
    );
    assert_format_rewrite!(
        r#"const a_float = 12_34.0
"#,
        r#"const a_float = 1234.0
"#
    );
    assert_format_rewrite!(
        r#"const a_float = 1234567_8.0
"#,
        r#"const a_float = 12_345_678.0
"#
    );
    assert_format_rewrite!(
        r#"const a_float = -1_234.0
"#,
        r#"const a_float = -1234.0
"#
    );
    assert_format_rewrite!(
        r#"const a_float = -12_34.0
"#,
        r#"const a_float = -1234.0
"#
    );
    assert_format_rewrite!(
        r#"const a_float = -1234567_8.0
"#,
        r#"const a_float = -12_345_678.0
"#
    );

    assert_format_rewrite!(
        r#"const a_float = 1234.00
"#,
        r#"const a_float = 1234.0
"#
    );
    assert_format_rewrite!(
        r#"const a_float = 1234.00100
"#,
        r#"const a_float = 1234.001
"#
    );
    assert_format_rewrite!(
        r#"const a_float = 1234.001001
"#,
        r#"const a_float = 1234.001001
"#
    );

    assert_format!(
        r#"fn f() {
  1.0e1
}
"#
    );
    assert_format!(
        r#"fn f() {
  1.0e-1
}
"#
    );
    assert_format!(
        r#"fn f() {
  -1.0e1
}
"#
    );
    assert_format!(
        r#"fn f() {
  -1.0e-1
}
"#
    );
    assert_format!(
        r#"fn f() {
  1.0e10
}
"#
    );
    assert_format!(
        r#"fn f() {
  1.0e-10
}
"#
    );
    assert_format!(
        r#"fn f() {
  -1.0e10
}
"#
    );
    assert_format!(
        r#"fn f() {
  -11.0e-10
}
"#
    );
    assert_format!(
        r#"fn f() {
  1.0e100
}
"#
    );
    assert_format!(
        r#"fn f() {
  1.0e-100
}
"#
    );
    assert_format!(
        r#"fn f() {
  -1.0e100
}
"#
    );
    assert_format!(
        r#"fn f() {
  -11.0e-100
}
"#
    );
    assert_format!(
        r#"fn f() {
  1.0e100
}
"#
    );
    assert_format!(
        r#"fn f() {
  1.0e100_100
}
"#
    );
    assert_format!(
        r#"fn f() {
  1.0e100_100
}
"#
    );
    assert_format!(
        r#"fn f() {
  1.001e100_100
}
"#
    );
}

#[test]
fn expr_string() {
    assert_format!(
        r#"fn main() {
  "Hello"
}
"#
    );

    assert_format!(
        r#"fn main() {
  "Hello

World"
}
"#
    );

    assert_format!(
        r#"fn main() {
  "\\n\\t"
}
"#
    );
}
#[test]
fn expr_seq() {
    assert_format!(
        r#"fn main() {
  1
  2
  3
}
"#
    );

    assert_format!(
        r#"fn main() {
  first(1)
  1
}
"#
    );
}
#[test]
fn expr_lists() {
    assert_format!(
        "fn main() {
  []
}
"
    );

    assert_format!(
        "fn main() {
  [1]
}
"
    );

    assert_format!(
        "fn main() {
  [
    {
      1
      2
    },
  ]
}
"
    );

    assert_format!(
        "fn main() {
  [1, 2, 3]
}
"
    );

    assert_format!(
        "fn main() {
  [
    1,
    {
      2
      3
    },
    3,
  ]
}
"
    );

    assert_format!(
        "fn main() {
  [
    really_long_variable_name,
    really_long_variable_name,
    really_long_variable_name,
    [1, 2, 3],
    really_long_variable_name,
  ]
}
"
    );

    assert_format!(
        "fn main() {
  [
    really_long_variable_name,
    really_long_variable_name,
    really_long_variable_name,
    [
      really_long_variable_name,
      really_long_variable_name,
      really_long_variable_name,
      2,
      3,
      [1, 2, 3, 4],
    ],
    really_long_variable_name,
  ]
}
"
    );

    assert_format!(
        "fn main() {
  [1, 2, 3, ..x]
}
"
    );

    assert_format!(
        "fn main() {
  [
    really_long_variable_name,
    really_long_variable_name,
    really_long_variable_name,
    ..tail
  ]
}
"
    );

    assert_format!(
        "fn main() {
  [
    really_long_variable_name,
    really_long_variable_name,
    really_long_variable_name,
    [
      really_long_variable_name,
      really_long_variable_name,
      really_long_variable_name,
      2,
      3,
      [1, 2, 3, 4],
      ..tail
    ],
    really_long_variable_name,
  ]
}
"
    );
}

#[test]
fn expr_pipe() {
    assert_format!(
        r#"fn main() {
  1
  |> really_long_variable_name
  |> really_long_variable_name
  |> really_long_variable_name
  |> really_long_variable_name
  |> really_long_variable_name
  |> really_long_variable_name
}
"#
    );

    assert_format!(
        r#"fn main() {
  #(
    1
      |> succ
      |> succ,
    2,
    3,
  )
}
"#
    );

    assert_format!(
        r#"fn main() {
  some_call(
    1
      |> succ
      |> succ,
    2,
    3,
  )
}
"#
    );

    assert_format!(
        r#"fn main() {
  [
    1
      |> succ
      |> succ,
    2,
    3,
  ]
}
"#
    );

    assert_format!(
        r#"fn main() {
  let x =
    1
    |> succ
    |> succ
  x
}
"#
    );

    assert_format!(
        r#"fn main() {
  #(1, 2)
  |> pair.first
  |> should.equal(1)
}
"#
    );

    assert_format!(
        r#"fn main() {
  #(1, 2)
  |> pair.first(1, 2, 4)
  |> should.equal(1)
}
"#
    );

    assert_format!(
        r#"fn main() {
  1
  // 1
  |> func1
  // 2
  |> func2
}
"#
    );

    // https://github.com/gleam-lang/gleam/issues/618

    assert_format!(
        r#"fn main() {
  {
    1
    2
  }
  |> func
}
"#
    );

    assert_format!(
        r#"fn main() {
  1
  |> {
    1
    2
  }
}
"#
    );

    // https://github.com/gleam-lang/gleam/issues/658
    assert_format!(
        r#"fn main() {
  { os.system_time(os.Millisecond) < june_12_2020 * 1_000_000 }
  |> should.equal(True)
}
"#
    );

    assert_format!(
        r#"fn main() {
  { os.system_time(os.Millisecond) < june_12_2020 * 1_000_000 }
  |> transform
  |> should.equal(True)
}
"#
    );
}

#[test]
fn expr_let() {
    assert_format!(
        r#"fn main() {
  let x = 1
  Nil
}
"#
    );
}

#[test]
fn expr_let1() {
    assert_format!(
        r#"fn main() {
  let assert x = 1
  Nil
}
"#
    );
}

#[test]
fn expr_let2() {
    assert_format!(
        r#"fn main() {
  let x = {
    let y = 1
    y
  }
  Nil
}
"#
    );
}

#[test]
fn expr_let3() {
    assert_format!(
        r#"fn main() {
  let x = {
    1
    2
  }
  Nil
}
"#
    );
}

#[test]
fn expr_let4() {
    assert_format!(
        r#"fn main() {
  let y = case x {
    1 -> 1
    _ -> 0
  }
  y
}
"#
    );
}

#[test]
fn expr_let5() {
    assert_format!(
        r#"fn main() {
  let y = case x {
    1 -> 1

    _ -> 0
  }
  y
}
"#
    );
}

#[test]
fn expr_let6() {
    assert_format!(
        r#"fn main() {
  let x = fn(x) { x }
  x
}
"#
    );
}

#[test]
fn expr_let7() {
    assert_format!(
        r#"fn main() {
  let x = fn() {
    1
    2
  }
  x
}
"#
    );
}

#[test]
fn expr_let8() {
    assert_format!(
        r#"fn main() {
  let x = fn(
    state: state,
    acc: visitor_acc,
    visitor: fn(visitor_acc, Pid(a)) -> new_visitor_acc,
  ) {
    1
    2
  }
  x
}
"#
    );
}

#[test]
fn expr_let9() {
    assert_format!(
        r#"fn main() {
  let x = fn(
    state: state,
    acc: visitor_acc,
    visitor: fn(visitor_acc, Pid(a)) -> new_visitor_acc,
  ) {
    2
  }
  x
}
"#
    );
}

#[test]
fn expr_let10() {
    assert_format!(
        r#"fn main() {
  let dict = map.from_list([#("a", 0), #("b", 1), #("c", 2), #("d", 3)])
  1
}
"#
    );
}

#[test]
fn pattern_simple() {
    // Pattern::Float
    assert_format!(
        r#"fn main() {
  let 1 = 1
  Nil
}
"#
    );

    // Pattern::String
    assert_format!(
        r#"fn main() {
  let 1.0 = 1
  Nil
}
"#
    );

    // Pattern::Var
    assert_format!(
        r#"fn main() {
  let x = 1
  let y = 1
  Nil
}
"#
    );
}

#[test]
fn breakable_pattern() {
    assert_format!(
        r#"fn main() {
  let Ok(Thingybob(
    one: _one,
    two: _two,
    three: _three,
    four: _four,
    five: _five,
    six: _six,
  )) = 1
  Nil
}
"#
    );
}

#[test]
fn pattern_let() {
    assert_format!(
        r#"fn main() {
  let x as y = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let #(x, y, 123 as z) = 1
  Nil
}
"#
    );
}

#[test]
fn pattern_discard() {
    assert_format!(
        r#"fn main() {
  let _ = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let _wibble = 1
  Nil
}
"#
    );
}

#[test]
fn pattern_lists() {
    assert_format!(
        r#"fn main() {
  let [] = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let [1] = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let [1, 2, 3, 4] = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let [1, 2, 3, 4, ..x] = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let [
    really_long_variable_name,
    really_long_variable_name,
    really_long_variable_name,
    [1, 2, 3, 4, xyz],
    ..thingy
  ] = 1
  Nil
}
"#
    );
}

#[test]
fn pattern_constructor() {
    assert_format!(
        r#"fn main() {
  let True = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let False = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let Ok(1) = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let Person(name, age: the_age) = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let Person(name: the_name, age: the_age) = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let Person(age: age, name: name) = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let Person(age: really_long_variable_name, name: really_long_variable_name) =
    1
  Nil
}
"#
    );
}

#[test]
fn pattern_tuple() {
    assert_format!(
        r#"fn main() {
  let #() = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let #(x) = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let #(x, y) = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let #(x, y, z) = 1
  Nil
}
"#
    );
}

#[test]
fn expr_case() {
    assert_format!(
        r#"fn main() {
  case 1 {
    1 -> {
      1
      2
    }
    1 -> 1
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  case 1 {
    1 -> {
      let x = 1
      x
    }
    1 -> 1
  }
}
"#
    );

    assert_format!(
        r#"fn do() {
  case list {
    [x, ..xs] -> {
      let x = 1
      x
    }
  }
}
"#
    );

    assert_format!(
        r#"fn do() {
  case list {
    [x, ..xs] -> {
      let x = 1
      x
      1
      2
      3
      4
    }
  }
}
"#
    );

    assert_format!(
        "fn main() {
  case x {
    1 -> 2

    2 -> 3

    _ -> 0
  }
}
"
    );

    assert_format!(
        r#"fn main() {
  case bool {
    True -> {
      "Wibble"
      |> io.println

      "Wobble"
      |> io.println

      Nil
    }
    False -> Nil
  }
}
"#
    );
}

#[test]
fn expr_case_nested() {
    assert_format!(
        r#"fn main() {
  case 1 {
    1 ->
      case x {
        1 -> 1
        _ -> 0
      }
    1 -> 1
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  case list {
    [x] ->
      case x {
        _ -> 1
      }
  }
}
"#
    );
}

#[test]
fn expr_case_then_fn() {
    assert_format!(
        r#"fn main() {
  case 1 {
    1 -> fn(x) { x }
    1 -> 1
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  case 1 {
    1 -> fn() {
      1
      2
    }
    1 -> 1
  }
}
"#
    );
}

#[test]
fn expr_case_multiple_subjects() {
    assert_format!(
        r#"fn main() {
  case 1 {
    1 -> 1
    1 -> 1
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  case 1, 2, 3, 4 {
    1, 2, 3, 4 -> 1
    1, 2, 3, 4 -> 1
  }
}
"#
    );
}

#[test]
fn expr_case_alternative_patterns() {
    assert_format!(
        r#"fn main() {
  case 1 {
    1 | 2 | 3 -> Nil
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  case 1, 2 {
    1, 1 | 2, 2 | 3, 3 -> Nil
    1, 1 | 2, 2 | 3, 3 -> Nil
    1, 1 | 2, 2 | 3, 3 -> Nil
    1, 1 | 2, 2 | 3, 3 -> Nil
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  case pat {
    pat.Typeof("Boolean", pat)
    | pat.Typeof("Number", pat)
    | pat.Typeof("String", pat) -> Nil
  }
}
"#
    );
}

#[test]
fn expr_case_clause_guards() {
    assert_format!(
        r#"fn main() {
  case 1 {
    _ if x == y -> Nil
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  case "x" {
    _ if x == "x" -> Nil
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  case #(1, 2, 3) {
    _ if x == #(1, 2, 3) -> Nil
  }
}
"#
    );

    assert_format!(
        r#"type Test {
  Test(x: Int, y: Float)
}

pub fn main() {
  let x = Test(1, 3.0)
  case x {
    _ if x == Test(1, 1.0) -> 1
    _ if x == Test(y: 2.0, x: 2) -> 2
    _ if x != Test(2, 3.0) -> 2
    _ -> 0
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  case 1 {
    _ if x != y -> Nil
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  case 1 {
    _ if x || y -> Nil
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  case 1 {
    _ if x && y -> Nil
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  case 1 {
    _ if x != y && x == z -> Nil
  }
}
"#
    );
}

#[test]
fn expr_case_clause_comments() {
    assert_format!(
        r#"fn main() {
  case 1 {
    // Hello Louis!
    1 | 2 | 3 -> Nil
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  case 1 {
    // Hello José!
    1 | 2 -> Nil
    // Hello Louis!
    n -> Nil
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  case 1 {
    // Hello Joe!
    1 | 2 -> Nil

    // Hello Louis!
    n -> Nil
  }
}
"#
    );

    assert_format!(
        r#"fn main() {
  case pat {
    // Hello Ada
    pat.Typeof("Boolean", pat) | pat.Typeof("Number", pat) -> True

    // Hello Alan
    pat.Typeof("Boolean", pat)
    | pat.Typeof("Number", pat)
    | pat.Typeof("String", pat) -> False
  }
}
"#
    );
}

#[test]
fn field_access() {
    assert_format!(
        r#"fn main() {
  one.two
}
"#
    );

    assert_format!(
        r#"fn main() {
  one.two.three.four
}
"#
    );
}

#[test]
fn tuple_access() {
    assert_format!(
        r#"fn main() {
  tup.0
}
"#
    );
}

#[test]
fn tuple_access1() {
    assert_format!(
        r#"fn main() {
  tup.1
}
"#
    );
}

#[test]
fn tuple_access2() {
    assert_format!(
        r#"fn main() {
  tup.777
}
"#
    );
}

#[test]
fn tuple_access3() {
    assert_format!(
        r#"fn main() {
  tup.1.2
}
"#
    );
}

#[test]
fn expr_panic() {
    assert_format!(
        "fn main() {
  panic
}
"
    );
}

#[test]
fn expr_panic_as() {
    assert_format!(
        r#"fn main() {
  panic as "panicking"
}
"#
    );
}

#[test]
fn expr_panic_as_value() {
    assert_format!(
        r#"fn main() {
  let x = "panicking" <> "with a value"
  panic as x
}
"#
    );
}

#[test]
fn expr_todo_as_value() {
    assert_format!(
        r#"fn main() {
  let x = "Need to" <> "do this"
  todo as x
}
"#
    );
}

#[test]
fn expr_todo() {
    assert_format!(
        "fn main() {
  todo
}
"
    );
}

#[test]
fn expr_todo_with_label() {
    assert_format!(
        r#"fn main() {
  todo as "todo with a label"
}
"#
    );
}

#[test]
fn expr_todo1() {
    assert_format_rewrite!(
        r#"fn main() {
  fn() {}
}
"#,
        r#"fn main() {
  fn() { todo }
}
"#
    );
}

#[test]
fn doc_comments_test() {
    assert_format!(
        "/// one
fn main() {
  Nil
}
"
    );
}

#[test]
fn doc_comments_1_test() {
    assert_format!(
        "/// one
///two
fn main() {
  Nil
}
"
    );
}

#[test]
fn doc_comments_2_test() {
    assert_format!(
        r#"/// one
///two
@external(javascript, "", "")
fn whatever() -> Nil
"#
    );
}

#[test]
fn doc_comments_3_test() {
    assert_format!(
        r#"/// one
///two
type Thingy
"#
    );
}

#[test]
fn doc_comments_4_test() {
    assert_format!(
        r#"/// one
///two
type Thingy
"#
    );
}

#[test]
fn doc_comments_5_test() {
    assert_format!(
        r#"/// one
///two
type Whatever {
  Whatever
}
"#
    );
}

#[test]
fn doc_comments_6_test() {
    assert_format!(
        r#"/// one
///two
type Whatever =
  Int
"#
    );
}

#[test]
fn comments3() {
    assert_format!(
        "// one
fn main() {
  Nil
}
"
    );
}

#[test]
fn comments4() {
    assert_format!(
        "// one
//two
fn main() {
  Nil
}
"
    );
}

#[test]
fn comments5() {
    assert_format!(
        r#"// one
//two
@external(javascript, "", "")
fn whatever() -> Nil
"#
    );
}

#[test]
fn comments9() {
    assert_format!(
        r#"// one
//two
type Whatever =
  Int
"#
    );
}

#[test]
fn comment23() {
    assert_format!(
        "fn main() {
  // Hello
  // world
  1
}
"
    );
}

#[test]
fn comment24() {
    assert_format!(
        "fn main() {
  // Hello
  // world
  1.0
}
"
    );
}

#[test]
fn comment25() {
    assert_format!(
        "fn main() {
  // Hello
  // world
  Nil
}
"
    );
}

#[test]
fn comment14() {
    assert_format!(
        "fn main() {
  // Hello
  // world
  []
}
"
    );
}

#[test]
fn comment15() {
    assert_format!(
        "fn main() {
  // Hello
  // world
  [
    // One
    1,
    // Two
    2,
  ]
}
"
    );
}

#[test]
fn comment16() {
    assert_format!(
        "fn main() {
  // Hello
  // world
  [
    // One
    1,
    // Two
    2,
    [
      // Five
      3,
      [
        // Four
        4,
      ],
    ],
  ]
}
"
    );
}

#[test]
fn comment17() {
    assert_format!(
        "fn main() {
  // Hello
  // world
  one(
    // One
    1,
    // Two
    2,
    two(
      // Five
      3,
      three(
        // Four
        4,
      ),
    ),
  )
}
"
    );
}

#[test]
fn comment18() {
    assert_format!(
        "fn main() {
  // Hello
  1
  // world
  2
}
"
    );
}

#[test]
fn comment19() {
    assert_format!(
        "fn main() {
  let // hello
  x = 1
  x
}
"
    );
}

#[test]
fn comment20() {
    assert_format!(
        "fn main() {
  let [
    // 1
    1,
    // 2
    2,
  ] = xs
  x
}
"
    );
}

#[test]
fn comment21() {
    assert_format!(
        "pub type Spec {
  Spec(
    // Hello
    hello: Int,
    // World
    world: Int,
  )
}
"
    );
}

#[test]
fn comment22() {
    assert_format!(
        "/// ß↑e̊
///
pub fn one() {
  1
}

pub fn two() {
  2
}
",
    );
}

#[test]
fn trailing_comments() {
    assert_format!(
        "fn main() {
  x
}
// Hello world
// ok!
"
    );

    assert_format!(
        "fn main() {
  x
}
/// Hello world
/// ok!
"
    );
    assert_format!(
        "fn main() {
  x
}
/// Hello world
/// ok!
// Hello world
// ok!
"
    );
}

#[test]
fn commented_fn_arguments() {
    assert_format!(
        "fn main(
  // comment
  label argument: Type,
) {
  x
}
"
    );
}

#[test]
fn commented_fn_arguments1() {
    assert_format!(
        "fn main(
  // comment1
  label argument1: Type,
  // comment2
  label argument2: Type,
) {
  x
}
"
    );
}

#[test]
fn commented_fn_arguments2() {
    assert_format!(
        "@external(erlang, \"\", \"\")
pub fn main(
  // comment1
  argument1: Type,
  // comment2
  argument2: Type,
) -> Int
"
    );
}

#[test]
fn commented_binop() {
    assert_format!(
        "fn main() {
  1
  // hello
  + 2
}
"
    );

    assert_format!(
        "fn main() {
  // one
  1
  // two
  + 2
  // three
  + 3
}
"
    );
}

#[test]
fn commented_constructors() {
    assert_format!(
        "pub type Number {
  // 1
  One
  // 2
  Two
  // 3
  Three
  // ???
  More
}
"
    );

    assert_format!(
        "pub type Number {
  /// 1
  One
  /// 2
  Two
  /// 3
  Three
  /// ???
  More
}
"
    );

    assert_format!(
        "pub type Number {
  // a
  /// 1
  One
  // b
  /// 2
  Two
  // c
  /// 3
  Three
  // defg
  /// ???
  More
}
"
    );

    assert_format!(
        "pub type Number {
  /// 1
  One(value: Int)
  /// > 1
  Many(value: Int)
}
"
    );
}

#[test]
fn function_captures_test() {
    assert_format_rewrite!(
        "pub fn main() {
  run(_)
}
",
        "pub fn main() {
  run
}
"
    );

    assert_format!(
        "pub fn main() {
  run(1, 2, _, 4, 5)
}
"
    );

    assert_format_rewrite!(
        "pub fn main() {
  run(1, 2, _, 4, 5)(_)
}
",
        "pub fn main() {
  run(1, 2, _, 4, 5)
}
"
    );
}

#[test]
fn pattern_record_spread() {
    assert_format!(
        "type Triple {
  Triple(a: Int, b: Int, c: Int)
}

fn main() {
  let triple = Triple(1, 2, 3)
  let Triple(the_a, c: the_c, ..) = triple
  the_c
}
"
    );

    // Formats the operator spread syntax with long names
    assert_format!(
        "type Triple {
  Triple(a: Int, b: Int, c: Int)
}

fn main() {
  let triple = Triple(1, 2, 3)
  let Triple(
    really_really_long_variable_name_a,
    c: really_really_long_variable_name_c,
    ..,
  ) = triple
  really_really_long_variable_name_c
}
"
    );

    // https://github.com/gleam-lang/gleam/issues/776
    assert_format!(
        "fn main() {
  let Triple(..) = triple()
  1
}
"
    );
}

#[test]
fn empty_lines() {
    assert_format!(
        "pub fn main() {
  1

  2
}
"
    );

    assert_format!(
        "pub fn main() {
  // one
  1

  // two
  2
}
"
    );

    assert_format!(
        "pub fn main() {
  // one
  1

  // two
  2

  // three
  3
}
"
    );

    assert_format!(
        "pub type Number {
  One

  Two

  Three
}
"
    );

    assert_format!(
        "pub fn main() {
  let x = 1

  x
}
"
    );

    // Lines with only spaces are treated as empty
    assert_format_rewrite!(
        "pub fn main() {
  let x = 1\n    \n  x
}
",
        "pub fn main() {
  let x = 1

  x
}
"
    );

    assert_format!(
        "pub fn main() {
  let inc = fn(a) { a + 1 }

  pair.map_first(#(1, 2), inc)
  |> should.equal(#(2, 2))

  pair.map_first(#(1, 2), inc)
  |> should.equal(#(2, 2))
}
"
    );
}

#[test]
fn modules_docs() {
    assert_format!(
        "//// One
//// Two
//// Three

pub fn main() {
  let x = 1

  x
}
"
    );

    assert_format!(
        "////
////
////
////
////

type X {
  X
}
// Hello
"
    );
}

#[test]
fn binary_operator_precedence() {
    assert_format!(
        "fn main() {
  { 1 + 2 } * 3
}
"
    );

    assert_format!(
        "fn main() {
  3 * { 1 + 2 }
}
"
    );

    assert_format!(
        "fn main() {
  3
  * {
    1
    |> inc
  }
}
"
    );

    assert_format!(
        "fn main() {
  {
    1
    |> inc
  }
  * 3
}
"
    );

    assert_format!(
        "fn main() {
  1
  |> { a || b }
}
"
    );

    assert_format!(
        "fn main() {
  { a || b }
  |> go
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/868
#[test]
fn precedence_rhs() {
    assert_format!(
        "fn main() {
  True != { a == b }
}
"
    );

    assert_format!(
        "fn main() {
  True != { a == { b != c } }
}
"
    );
}

#[test]
fn module_constants() {
    assert_format!(
        "pub const str = \"a string\"

const my_constant: String = \"Hello\"

pub const int = 4

pub const float = 3.14
"
    );
}

#[test]
fn concise_wrapping_of_simple_lists() {
    assert_format!(
        "pub fn main() {
  [
    100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400,
    1500, 1600, 1700, 1800, 1900, 2000,
  ]
}
"
    );

    assert_format!(
        "pub fn main() {
  [
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 1.0, 11.0, 12.0, 13.0, 14.0,
    15.0, 16.0, 17.0, 18.0, 19.0, 2.0,
  ]
}
"
    );

    assert_format!(
        r#"pub fn main() {
  [
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    "ten", "eleven", "twelve",
  ]
}
"#
    );

    assert_format!(
        "const values = [
  100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400,
  1500, 1600, 1700, 1800, 1900, 2000,
]
"
    );

    assert_format!(
        "const values = [
  1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 1.0, 11.0, 12.0, 13.0, 14.0, 15.0,
  16.0, 17.0, 18.0, 19.0, 2.0,
]
"
    );

    assert_format!(
        r#"const values = [
  "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
  "eleven", "twelve",
]
"#
    );
}

#[test]
fn commented_labelled_arguments() {
    assert_format!(
        "fn main() {
  Emulator(
    // one
    one: 1,
    // two
    two: 1,
  )
}
"
    );

    assert_format!(
        "fn main() {
  my_func(
    // one
    one: 1,
    // two
    two: 1,
  )
}
"
    );
}

#[test]
fn module_rewrites_test() {
    // Module comments are moved to the top
    assert_format_rewrite!(
        "//// One

//// Two

fn main() {
  1
}
//// Three

//// Four
",
        "//// One
//// Two
//// Three
//// Four

fn main() {
  1
}
",
    );

    // Superfluous function captures are removed from pipe expressions
    assert_format_rewrite!(
        "fn main() {
  1
  |> run(_, 1)
}
",
        "fn main() {
  1
  |> run(1)
}
",
    );

    assert_format_rewrite!(
        "fn main() {
  1
  |> run(_)
}
",
        "fn main() {
  1
  |> run
}
",
    );

    assert_format_rewrite!(
        "fn main() {
  let some_really_long_variable_name_to_force_wrapping = 1
  let bits = <<
      some_really_long_variable_name_to_force_wrapping,
      some_really_long_variable_name_to_force_wrapping,
    >>
  bits
}
",
        "fn main() {
  let some_really_long_variable_name_to_force_wrapping = 1
  let bits = <<
    some_really_long_variable_name_to_force_wrapping,
    some_really_long_variable_name_to_force_wrapping,
  >>
  bits
}
",
    );
}

// TODO: improve. This is too wide
#[test]
// https://github.com/gleam-lang/gleam/issues/748
fn assignments_break_value_first_test() {
    assert_format!(
        r#"fn main() {
  let assert Ok(1) = [
    10_000_000_000_000_000_000_000_000_001,
    20_000_000_000_000_000_000_000_000_001,
    30_000_000_000_000_000_000_000_000_001,
    40_000_000_000_000_000_000_000_000_001,
  ]
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let assert Ok(1) = [
    1_000_000_000_000_000_000_000_000_000, 2_000_000_000_000_000_000_000_000_000,
    3_000_000_000_000_000_000_000_000_000, 4_000_000_000_000_000_000_000_000_000,
  ]
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let assert <<11, 2, 4, 5, 6>> = [
    10_000_000_000_000_000_000_000_000_001,
    20_000_000_000_000_000_000_000_000_001,
    30_000_000_000_000_000_000_000_000_001,
    40_000_000_000_000_000_000_000_000_001,
  ]
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let assert <<11, 2, 4, 5, 6>> = [
    1_000_000_000_000_000_000_000_000_000, 2_000_000_000_000_000_000_000_000_000,
    3_000_000_000_000_000_000_000_000_000, 4_000_000_000_000_000_000_000_000_000,
  ]
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let assert [11, 2, 4, 5, 6] = [
    10_000_000_000_000_000_000_000_000_001,
    20_000_000_000_000_000_000_000_000_001,
    30_000_000_000_000_000_000_000_000_001,
    40_000_000_000_000_000_000_000_000_001,
  ]
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let assert [11, 2, 4, 5, 6] = [
    1_000_000_000_000_000_000_000_000_000, 2_000_000_000_000_000_000_000_000_000,
    3_000_000_000_000_000_000_000_000_000, 4_000_000_000_000_000_000_000_000_000,
  ]
  Nil
}
"#
    );
}

#[test]
fn function_type_type() {
    assert_format!(
        "type F =
  fn(some, really, long, set, of, arguments) ->
    #(some, really, long, set, of, arguments)
"
    );
}

#[test]
fn tuple_constant() {
    assert_format!(
        "const x: #(Int, Int) = #(1, 2)
"
    );
}

#[test]
fn var_constant() {
    assert_format!(
        r#"const x = 1

const x_alias = x

fn f(i: Int) -> Int {
  i
}

const f_alias: fn(Int) -> Int = f
"#
    );
}

#[test]
fn let_as_expression() {
    assert_format!(
        "pub fn main() {
  let x = 1
}
"
    );

    assert_format!(
        "pub fn main() {
  let x = {
    let y = 1
  }
}
"
    );
}

#[test]
fn assert_as_expression() {
    assert_format!(
        "pub fn main() {
  let assert x = 1
}
"
    );

    assert_format!(
        "pub fn main() {
  let assert x = {
    let assert y = 1
  }
}
"
    );
}

#[test]
fn case_in_call() {
    assert_format!(
        "fn clause_guard_tests(_fns) -> List(Test) {
  example(fn() {
    assert_equal(0, case Nil {
      _ if yes -> 0
      _ -> 1
    })
  })
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/1390
#[test]
fn list_spread_pattern() {
    assert_format!(
        "pub fn main(x) {
  case x {
    [y, ..] -> y
    _ -> 0
  }
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/1431
#[test]
fn first_argument_capture_special_case_list() {
    assert_format!(
        r#"pub fn main(x) {
  wibble(_, [
    "one argument that is both breakable and long enough to cause it to wrap",
  ])
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1431
#[test]
fn first_argument_capture_special_case_fn() {
    assert_format!(
        r#"pub fn main(x) {
  wibble(_, fn() {
    "one argument that is both breakable and long enough to cause it to wrap"
  })
}
"#
    );
}

#[test]
fn negation() {
    assert_format!(
        "pub fn negate(x) {
  !x
}
"
    );
}

#[test]
fn negation_block() {
    assert_format!(
        "pub fn negate(x) {
  !{
    123
    x
  }
}
"
    );
}

#[test]
fn empty_lines_work_with_trailing_space() {
    let src = "pub fn main() {
  let inc = fn(a) { a + 1 }


  pair.map_first(#(1, 2), inc)
  |> should.equal(#(2, 2))

  // Comment

  1


  // Comment


  2
}
";
    let expected = "pub fn main() {
  let inc = fn(a) { a + 1 }

  pair.map_first(#(1, 2), inc)
  |> should.equal(#(2, 2))

  // Comment

  1

  // Comment

  2
}
";
    // We first make extra sure we've not messed up the expected output and
    // check it's well formatted.
    assert_format!(expected);

    assert_format_rewrite!(src, expected);
}

#[test]
fn empty_lines_work_with_eol_normalisation() {
    let src = "pub fn main() {
  let inc = fn(a) { a + 1 }


  pair.map_first(#(1, 2), inc)
  |> should.equal(#(2, 2))

  // Comment

  1


  // Comment


  2
}
";
    let expected = "pub fn main() {
  let inc = fn(a) { a + 1 }

  pair.map_first(#(1, 2), inc)
  |> should.equal(#(2, 2))

  // Comment

  1

  // Comment

  2
}
";

    // We first make extra sure we've not messed up the expected output and
    // check it's well formatted.
    assert_format!(expected);

    assert_format_rewrite!(&src.replace('\n', "\r\n"), expected);
    assert_format_rewrite!(&src.replace('\n', "\r"), expected);
}

#[test]
fn empty_lines_work_with_trailing_space_and_eol_normalisation() {
    let src = "pub fn main() {
  let inc = fn(a) { a + 1 }


  pair.map_first(#(1, 2), inc)
  |> should.equal(#(2, 2))

  // Comment

  1


  // Comment


  2
}
";
    let expected = "pub fn main() {
  let inc = fn(a) { a + 1 }

  pair.map_first(#(1, 2), inc)
  |> should.equal(#(2, 2))

  // Comment

  1

  // Comment

  2
}
";

    // We first make extra sure we've not messed up the expected output and
    // check it's well formatted.
    assert_format!(expected);

    assert_format_rewrite!(src.replace('\n', "\r\n"), expected);
    assert_format_rewrite!(&src.replace('\n', "\r"), expected);
}
#[test]
fn single_empty_line_between_comments() {
    // empty line isn't added if it's not already present
    assert_format!(
        "pub fn wibble() {
  // wibble
  // wobble
  123
}
"
    );
}

#[test]
fn single_empty_line_between_comments1() {
    // single empty line between comments/statement preserved
    assert_format!(
        "pub fn wibble() {
  // wibble

  // wobble

  123
}
"
    );
}

#[test]
fn single_empty_line_between_comments2() {
    // multiple consecutive empty lines condensed into one
    assert_format_rewrite!(
        "pub fn wibble() {
  // wibble


  // wobble


  123
}
",
        "pub fn wibble() {
  // wibble

  // wobble

  123
}
"
    );
}

#[test]
fn single_empty_line_between_comments3() {
    // freestanding comments keep empty lines
    assert_format!(
        "// wibble

// wobble
"
    );
}

#[test]
fn single_empty_line_between_comments4() {
    // freestanding comments condense consecutive empty lines
    assert_format_rewrite!(
        "// wibble


// wobble
",
        "// wibble

// wobble
",
    );
}

// https://github.com/gleam-lang/gleam/issues/1640
#[test]
fn no_newline_before_comments() {
    assert_format!(
        "// wibble
// wobble
"
    );
}

// https://github.com/gleam-lang/gleam/issues/1647
#[test]
fn list_at_end_of_long_expr_line() {
    assert_format!(
        "pub fn example() {
  Ok(
    RecordConstructorWithALongName(
      a_field: RecordConstructorWithALongName(a_field: Record(a_field: [])),
    ),
  )
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/1647
#[test]
fn list_at_end_of_long_pattern_line() {
    assert_format!(
        "pub fn example() {
  let assert LongLongLongLongLongLongLongLongLongLongLongLongLongLongLongLongLongLongLongLongLong([]) =
    1
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/1647
#[test]
fn list_at_end_of_long_constant_line() {
    assert_format!(
        "const longlonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglonglong = []
"
    );
}

// https://github.com/gleam-lang/gleam/issues/1649
#[test]
fn dont_remove_braces_when_accessing_tuple() {
    assert_format!(
        r#"fn main() {
  { typed.0 }.type_
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1681
#[test]
fn wrap_case_subjects() {
    assert_format!(
        r#"fn main() {
  case
    "This is a really really long string to force wrapping",
    "This is a really really long string to force wrapping",
    "This is a really really long string to force wrapping",
    "This is a really really long string to force wrapping"
  {
    _, _, _, _ -> Nil
  }
}
"#
    );
}

// A bug reported on Discord. This would cause a compiler crash.
#[test]
fn multiple_empty_line_collapse_bug() {
    assert_format_rewrite!(
        r#"// Comment



const x = 1
"#,
        r#"// Comment

const x = 1
"#
    );
}

#[test]
fn do_not_remove_required_braces_case_guard() {
    assert_format!(
        "fn main() {
  let is_enabled = False
  let is_confirmed = False
  let is_admin = True
  case is_enabled, is_confirmed, is_admin {
    is_enabled, is_confirmed, is_admin
      if is_enabled && { is_confirmed || is_admin }
    -> Nil
    _, _, _ -> Nil
  }
}
"
    );

    assert_format!(
        "fn main() {
  let wibble = True
  case wibble {
    wibble if True != { 1 == 2 } -> Nil
    _ -> Nil
  }
}
"
    );

    assert_format!(
        "fn main() {
  let wibble = True
  let wobble = False
  case wibble {
    wibble if True != { 1 == { wobble == wibble } } -> Nil
    _ -> Nil
  }
}
"
    );

    assert_format!(
        "fn main() {
  let wibble = #(10, [0])
  case wibble {
    wibble if True && { wibble.0 == 10 || wibble.0 == 1 } -> Nil
    _ -> Nil
  }
}
"
    );
}

#[test]
fn remove_braces_case_guard() {
    assert_format_rewrite!(
        "fn main() {
  let is_enabled = False
  let is_confirmed = False
  let is_admin = True
  case is_enabled, is_confirmed, is_admin {
    is_enabled, is_confirmed, is_admin if { is_enabled && is_confirmed } || is_admin ->
      Nil
    _, _, _ -> Nil
  }
}
",
        "fn main() {
  let is_enabled = False
  let is_confirmed = False
  let is_admin = True
  case is_enabled, is_confirmed, is_admin {
    is_enabled, is_confirmed, is_admin
      if is_enabled && is_confirmed || is_admin
    -> Nil
    _, _, _ -> Nil
  }
}
"
    );
}

#[test]
fn remove_braces_case_guard_2() {
    assert_format_rewrite!(
        "fn main() {
  let wibble = #(10, [0])
  case wibble {
    wibble if True && { wibble.0 == 10 } -> Nil
    _ -> Nil
  }
}
",
        "fn main() {
  let wibble = #(10, [0])
  case wibble {
    wibble if True && wibble.0 == 10 -> Nil
    _ -> Nil
  }
}
"
    );
}

#[test]
fn const_multi_line_string_breaks() {
    assert_format!(
        r#"const string = [
  "hello
world",
]
"#
    );
}

#[test]
fn expr_multi_line_string_breaks() {
    assert_format!(
        r#"pub fn main() {
  let string = [
    "hello
world",
  ]
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1724
#[test]
fn case_subject_block() {
    assert_format!(
        r#"pub fn main() {
  case
    {
      let assert Ok(x) = thing()
      let assert Ok(y) = thing()
      x + y
    }
  {
    _ -> Nil
  }
}
"#
    );
}

#[test]
fn qualified_const_fn() {
    assert_format!(
        r#"import other

const x = other.function
"#
    );
}

#[test]
fn qualified_const_fn_fn_after() {
    assert_format!(
        r#"import other

const x = other.function

pub fn main() {
  io.println("Hello, Joe!")
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1872
#[test]
fn multiple_line_spread_list_comments() {
    assert_format!(
        r#"fn main() {
  [
    // First!
    // First?
    1,
    // Spread!
    // Spread?
    ..[2, 3]
  ]
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1872
#[test]
fn list_spread_comment_pattern() {
    assert_format!(
        r#"fn main() {
  let assert [
    1,
    // Spread!
    // Spread?
    ..rest
  ] = x
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1872
#[test]
fn list_spread_discard_comment_pattern() {
    assert_format!(
        r#"fn main() {
  let assert [
    1,
    // Spread!
    // Spread?
    ..
  ] = x
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1786
#[test]
fn multiple_line_documentation_comment_statement_grouping() {
    assert_format!(
        r#"/// This is the first line of the documentation comment.
/// This is the second line of the documentation comment.
/// This is the third line of the documentation comment.
pub type Map(key, value)
"#
    );
}

#[test]
fn not_and() {
    assert_format!(
        r#"pub fn main() {
  !{ True && False }
}
"#
    );
}

#[test]
fn not_or() {
    assert_format!(
        r#"pub fn main() {
  !{ True || False }
}
"#
    );
}

#[test]
fn not_add() {
    assert_format!(
        r#"pub fn main() {
  !{ 1 + 3 }
}
"#
    );
}

#[test]
fn deprecated_assert() {
    assert_format_rewrite!(
        r#"fn main(x) {
  let assert True = x
}
"#,
        r#"fn main(x) {
  let assert True = x
}
"#
    );
}

#[test]
fn negate() {
    assert_format_rewrite!(
        r#"pub fn main() {
  let a = 3
  let b = -        a
}
"#,
        r#"pub fn main() {
  let a = 3
  let b = -a
}
"#
    );
}

#[test]
fn double_negate() {
    assert_format_rewrite!(
        r#"pub fn main() {
  let a = 3
  let b = --a
}
"#,
        r#"pub fn main() {
  let a = 3
  let b = - -a
}
"#
    );
}

#[test]
fn triple_negate() {
    assert_format_rewrite!(
        r#"pub fn main() {
  let a = 3
  let b = -  -   - a
}
"#,
        r#"pub fn main() {
  let a = 3
  let b = - - -a
}
"#
    );
}

#[test]
fn binary_negate() {
    assert_format_rewrite!(
        r#"pub fn main() {
  let a = 3
  let b = -{a+3}
}
"#,
        r#"pub fn main() {
  let a = 3
  let b = -{ a + 3 }
}
"#
    );
}

#[test]
fn binary_double_negate() {
    assert_format_rewrite!(
        r#"pub fn main() {
  let a = 3
  let b = --{a + 3}
}
"#,
        r#"pub fn main() {
  let a = 3
  let b = - -{ a + 3 }
}
"#
    );
}

#[test]
fn repeated_negate_after_subtract() {
    assert_format_rewrite!(
        r#"pub fn main() {
  let a = 3
  let b = 4
  let c = a--------b
}
"#,
        r#"pub fn main() {
  let a = 3
  let b = 4
  let c = a - - - - - - - -b
}
"#
    );
}

#[test]
fn wrap_long_line_with_int_negation() {
    assert_format_rewrite!(
        r#"pub fn main() {
  let a = 3
  let b = a * a * a * a * a * a * a * a * a * a * a * a * a *   { a * a * a * a * a * a * a * a * a * a }
  let c = c * c * c * c * c * c * c * c * c * c * c * c * c * - { c * c * c * c * c * c * c * c * c * c }
}
"#,
        r#"pub fn main() {
  let a = 3
  let b =
    a
    * a
    * a
    * a
    * a
    * a
    * a
    * a
    * a
    * a
    * a
    * a
    * a
    * { a * a * a * a * a * a * a * a * a * a }
  let c =
    c
    * c
    * c
    * c
    * c
    * c
    * c
    * c
    * c
    * c
    * c
    * c
    * c
    * -{ c * c * c * c * c * c * c * c * c * c }
}
"#
    );
}

#[test]
fn wrap_long_line_with_bool_negation() {
    assert_format_rewrite!(
        r#"pub fn main() {
  let a = True
  let b = a || a || a || a || a || a || a || a || a || a || a || a || a ||   { a || a || a || a || a || a || a || a || a || a }
  let c = c || c || c || c || c || c || c || c || c || c || c || c || c || ! { c || c || c || c || c || c || c || c || c || c }
}
"#,
        r#"pub fn main() {
  let a = True
  let b =
    a
    || a
    || a
    || a
    || a
    || a
    || a
    || a
    || a
    || a
    || a
    || a
    || a
    || { a || a || a || a || a || a || a || a || a || a }
  let c =
    c
    || c
    || c
    || c
    || c
    || c
    || c
    || c
    || c
    || c
    || c
    || c
    || c
    || !{ c || c || c || c || c || c || c || c || c || c }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/1977
#[test]
fn preserve_single_expression_blocks() {
    assert_format!(
        r#"pub fn main(x) {
  case x {
    1 -> {
      1
    }
    _ -> 2
  }
}
"#
    );
}

#[test]
fn calling_pipeline0() {
    assert_format!(
        r#"pub fn main() {
  {
    one
    |> two
  }()
}
"#
    );
}

#[test]
fn calling_pipeline1() {
    assert_format!(
        r#"pub fn main() {
  {
    one
    |> two
  }(1)
}
"#
    );
}

#[test]
fn calling_pipeline2() {
    assert_format!(
        r#"pub fn main() {
  {
    one
    |> two
  }(1, 2)
}
"#
    );
}

#[test]
fn calling_pipeline_1_list() {
    assert_format!(
        r#"pub fn main() {
  {
    one
    |> two
  }([1, 2, 3])
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2119
#[test]
fn empty_line_after_fn_with_return_annotation() {
    assert_format!(
        r#"fn main() {
  fn() -> String { "" }

  0
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2174
#[test]
fn empty_line_after_crash() {
    assert_format_rewrite!(
        r#"pub type One {
  One // Comment

}

"#,
        r#"pub type One {
  One
  // Comment
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2196
#[test]
fn comment_at_end_of_type() {
    assert_format!(
        r#"pub type X {
  X
  // Afterwards
}
"#
    );
}

#[test]
fn deprecated_type_alias() {
    assert_format!(
        r#"@deprecated("Deprecated type")
pub type Tiger =
  Nil
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2423
#[test]
fn prefix_as() {
    assert_format!(
        r#"pub fn main(x) {
  case x {
    "0" as digit <> rest | "1" as digit <> rest -> rest
  }
}
"#
    );
}

#[test]
fn case_splits_function_on_newline() {
    assert_format!(
        r#"pub fn main() {
  case x {
    1 ->
      some_module.some_long_name_function([
        some_module.some_long_name_function(),
      ])
    _ -> todo
  }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2442
#[test]
fn single_argument_list() {
    assert_format!(
        r#"pub fn main() {
  Ok([
    some_long_variable_name_to_force_wrapping,
    some_long_variable_name_to_force_wrapping,
    some_long_variable_name_to_force_wrapping,
  ])
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2442
#[test]
fn single_argument_function() {
    assert_format!(
        r#"pub fn main() {
  Ok(fn() {
    some_long_variable_name_to_force_wrapping()
    some_long_variable_name_to_force_wrapping()
    some_long_variable_name_to_force_wrapping()
  })
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2442
#[test]
fn single_argument_tuple() {
    assert_format!(
        r#"pub fn main() {
  Ok(#(
    some_long_variable_name_to_force_wrapping,
    some_long_variable_name_to_force_wrapping,
    some_long_variable_name_to_force_wrapping,
  ))
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2442
#[test]
fn single_argument_call() {
    assert_format!(
        r#"pub fn main() {
  Ok(do_something(
    some_long_variable_name_to_force_wrapping,
    some_long_variable_name_to_force_wrapping,
    some_long_variable_name_to_force_wrapping,
  ))
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2442
#[test]
fn single_argument_call_nested() {
    assert_format!(
        r#"pub fn main() {
  Ok(
    do_something(do_something_else(
      some_long_variable_name_to_force_wrapping,
      some_long_variable_name_to_force_wrapping,
      some_long_variable_name_to_force_wrapping,
    )),
  )
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2442
#[test]
fn single_argument_call_nested_nested() {
    assert_format!(
        r#"pub fn main() {
  Ok(
    do_something(
      do_something_else(do_a_last_thing(
        some_long_variable_name_to_force_wrapping,
        some_long_variable_name_to_force_wrapping,
        some_long_variable_name_to_force_wrapping,
      )),
    ),
  )
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2512
#[test]
fn list_with_pipe_format() {
    assert_format!(
        r#"pub fn main() {
  [
    "Success!"
      |> ansi(apply: [1, 31]),
    "",
    "Wrote `" <> bin <> "`, `" <> pwsh_bin <> "`",
  ]
}
"#
    );
}

#[test]
fn function_call_close_to_line_limit() {
    assert_format!(
        r#"pub fn main() {
  function_call(
    that,
    is,
    super,
    close,
    to,
    the,
    max,
    line,
    limit,
    of,
    80,
    chars,
  )
}
"#
    );
}

#[test]
fn multiline_string_are_not_broken_with_string_concatenation_if_they_fit() {
    assert_format!(
        r#"pub fn main() {
  "pub fn wibble(" <> arg <> ") ->" <> type_ <> "{
    body
}"
}
"#
    );
}

#[test]
fn nesting_goes_back_to_normal_after_multiline_string() {
    assert_format!(
        r#"pub fn main() {
  let x = {
    "
1
2
" <> long_name_function_call(
      1_111_111_111_111_111,
      222_222_222_222,
      3_333_333_333_333_333,
    )
  }
}
"#
    );
}

#[test]
fn multiline_string_get_broken_on_newlines_as_function_arguments() {
    assert_format!(
        r#"pub fn main() {
  wibble(
    wobble,
    "wobble
  wibble
       wobble",
    wibble,
    wobble,
  )
}
"#
    );
}

#[test]
fn pipeline_used_as_function_arguments_gets_nested() {
    assert_format!(
        r#"pub fn main() {
  wibble(
    a_variable_with_a_long_name
      |> another_variable_with_a_long_name
      |> yet_another_variable_with_a_long_name,
    wobble,
  )
}
"#
    );
}

#[test]
fn pipeline_used_as_function_arguments_is_not_nested_if_it_is_the_only_argument() {
    assert_format!(
        r#"pub fn main() {
  wibble(
    a_variable_with_a_long_name
    |> another_variable_with_a_long_name
    |> yet_another_variable_with_a_long_name,
  )
}
"#
    );
}

#[test]
fn pipeline_inside_list_gets_nested() {
    assert_format!(
        r#"pub fn main() {
  [
    wibble,
    a_variable_with_a_long_name
      |> another_variable_with_a_long_name
      |> yet_another_variable_with_a_long_name,
  ]
}
"#
    );
}

#[test]
fn pipeline_inside_list_is_not_nested_if_only_item() {
    assert_format!(
        r#"pub fn main() {
  [
    a_variable_with_a_long_name
    |> another_variable_with_a_long_name
    |> yet_another_variable_with_a_long_name,
  ]
}
"#
    );
}

#[test]
fn pipeline_inside_tuple_gets_nested() {
    assert_format!(
        r#"pub fn main() {
  #(
    wibble,
    a_variable_with_a_long_name
      |> another_variable_with_a_long_name
      |> yet_another_variable_with_a_long_name,
  )
}
"#
    );
}

#[test]
fn pipeline_inside_tuple_is_not_nested_if_only_item() {
    assert_format!(
        r#"pub fn main() {
  #(
    a_variable_with_a_long_name
    |> another_variable_with_a_long_name
    |> yet_another_variable_with_a_long_name,
  )
}
"#
    );
}

// github.com/gleam-lang/gleam/issues/2608
#[test]
fn comments_are_not_moved_out_of_list_of_literals() {
    assert_format!(
        r#"fn main() {
  [
    1, 2,
    // list
  ]
}
"#
    );
}

// github.com/gleam-lang/gleam/issues/2608
#[test]
fn comments_are_not_moved_out_of_list() {
    assert_format!(
        r#"fn main() {
  [
    wibble,
    wobble,
    // list
  ]
}
"#
    );
}

// github.com/gleam-lang/gleam/issues/2608
#[test]
fn comments_are_not_moved_out_of_case_expressions() {
    assert_format!(
        r#"fn main() {
  case True {
    _ -> Nil
    // case
  }
}
"#
    );
}

// github.com/gleam-lang/gleam/issues/2608
#[test]
fn comments_are_not_moved_out_of_tuples() {
    assert_format!(
        r#"fn main() {
  #(
    1,
    2,
    // tuple
  )
}
"#
    );
}

// github.com/gleam-lang/gleam/issues/2608
#[test]
fn comments_are_not_moved_out_of_function_calls() {
    assert_format!(
        r#"fn main() {
  call(
    1,
    2,
    // function call
  )
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2607
#[test]
fn function_arguments_after_comment_are_not_indented() {
    assert_format!(
        r#"pub fn main() {
  wibble(
    // Wobble
    1 + 1,
    "wibble",
  )
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2607
#[test]
fn tuple_items_after_comment_are_not_indented() {
    assert_format!(
        r#"pub fn main() {
  #(
    // Wobble
    1 + 1,
    "wibble",
  )
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2607
#[test]
fn list_items_after_comment_are_not_indented() {
    assert_format!(
        r#"pub fn main() {
  [
    // Wobble
    1 + 1,
    "wibble",
  ]
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2990
#[test]
fn comments_are_not_moved_out_of_empty_list() {
    assert_format!(
        r#"pub fn main() {
  // This is an empty list!
  [
    // Nothing here...
  ]
}
"#
    );
}

#[test]
fn empty_lists_with_comment_inside_are_indented_properly() {
    assert_format!(
        r#"pub fn main() {
  fun(
    [
      // Nothing here...
    ],
    wibble_wobble_wibble_wobble_wibble_wobble_wibble_wobble,
    [
      // Nothing here as well!
    ],
  )
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2890
#[test]
fn piped_blocks_are_not_needlessly_indented() {
    assert_format!(
        r#"pub fn main() {
  #(
    1,
    {
      "long enough to need to wrap. blah blah blah blah blah blah blah blah blah"
    }
      |> wibble,
    3,
  )
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2924
#[test]
fn record_update_fields_are_not_needlessly_broken() {
    assert_format!(
        r#"pub fn main() {
  Model(
    ..model,
    wibble: wibble_wobble_wibble_wobble + 1,
    wobble: Some(wibble_wobble_wibble_wobble),
  )
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2890
#[test]
fn piped_lists_are_not_needlessly_indented() {
    assert_format!(
        r#"pub fn main() {
  fun(
    [
      ["wibble wobble", "wibble", "wobble"],
      ["long enough to go over", "line limit"],
    ]
      |> list.concat,
    todo,
  )
}
"#
    );
}

#[test]
fn comments_inside_nested_pipe_chain() {
    assert_format!(
        r#"pub fn main() {
  fun(
    thing
      // A comment
      |> wibble
      // Another comment
      |> wobble,
    thing,
  )
}
"#
    );
}

#[test]
fn comments_inside_nested_binop_chain() {
    assert_format!(
        r#"pub fn main() {
  fun(
    thing
      // A comment
      <> wibble
      // Another comment
      <> wobble,
    thing,
  )
}
"#
    );
}

#[test]
fn comments_inside_binop_chain() {
    assert_format!(
        r#"pub fn main() {
  thing
  // A comment
  <> wibble
  // Another comment
  <> wobble
}
"#
    );
}

#[test]
fn internal_attribute_on_function() {
    assert_format!(
        r#"@internal
pub fn main() {
  todo
}
"#
    );
}

#[test]
fn internal_attribute_on_type() {
    assert_format!(
        r#"@internal
pub type Type
"#
    );
}

#[test]
fn internal_attribute_on_const() {
    assert_format!(
        r#"@internal
pub const wibble = 1
"#
    );
}

#[test]
fn comments_inside_contant_list() {
    assert_format!(
        r#"const wibble = [
  // A comment
  1, 2,
  // Another comment
  3,
  // One last comment
]
"#
    );
}

#[test]
fn comments_inside_contant_empty_list() {
    assert_format!(
        r#"const wibble = [
  // A comment
]
"#
    );
}

#[test]
fn comments_inside_contant_tuple() {
    assert_format!(
        r#"const wibble = #(
  // A comment
  1,
  2,
  // Another comment
  3,
  // One last comment
)
"#
    );
}

#[test]
fn comments_inside_contant_empty_tuple() {
    assert_format!(
        r#"const wibble = #(
  // A comment
)
"#
    );
}

#[test]
fn comments_inside_empty_tuple() {
    assert_format!(
        r#"pub fn main() {
  #(
    // A comment!
  )
}
"#
    );
}

#[test]
fn comments_at_the_end_of_anonymous_function() {
    assert_format!(
        r#"pub fn main() {
  fn() {
    1
    // a final comment

    // another final comment
    // at the end of the block
  }
}
"#
    );
}

#[test]
fn comments_in_anonymous_function_args() {
    assert_format!(
        r#"pub fn main() {
  fn(
    // A comment 1
    // A comment 2
  ) {
    1
  }
}
"#
    );
    assert_format!(
        r#"pub fn main() {
  fn(
    // A comment 1
    a,
    // A comment 2
  ) {
    1
  }
}
"#
    );
}

#[test]
fn comments_after_last_argument_of_record_constructor() {
    assert_format!(
        r#"type Record {
  Record(
    field: String,
    // comment_line_1: String,
    // comment_line_2: String,
  )
}
"#
    );
}

#[test]
fn only_comments_in_record_constructor() {
    assert_format!(
        r#"type Record {
  Record(
    // comment_line_1: String,
    // comment_line_2: String,
  )
}
"#
    );
}
#[test]
fn comment_after_spread_operator() {
    assert_format!(
        "type Triple {
  Triple(a: Int, b: Int, c: Int)
}

fn main() {
  let triple = Triple(1, 2, 3)
  let Triple(
    really_really_long_variable_name_a,
    c: really_really_long_variable_name_c,
    ..,
    // comment
  ) = triple
  really_really_long_variable_name_c
}
"
    );
}

#[test]
fn multiline_comment_in_case_block() {
    assert_format!(
        r#"pub fn do_len(list, acc) {
  case list {
    [] -> acc
    [_, ..rest] -> rest |> do_len(acc + 1)
    // Even the opposite wouldn't be optimised:
    // { acc + 1 } |> do_len(rest, _)
  }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3190
#[test]
fn trailing_comments_inside_non_empty_bit_arrays_are_not_moved() {
    assert_format!(
        r#"pub fn main() {
  <<
    1, 2,
    // One and two are above me.
  >>
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3210
#[test]
fn newlines_are_not_stripped_if_two_consecutive_anonymous_function_are_passed_as_arguments() {
    assert_format!(
        r#"pub fn main() {
  fun(
    fn() {
      wibble

      wobble
    },
    fn() { wibble },
  )
}
"#
    );
}

#[test]
fn const_long_concat_string() {
    assert_format_rewrite!(
        r#"const long_string = "some" <> " very" <> " long" <> " string" <> " indeed" <> " please" <> " break"
"#,
        r#"const long_string = "some"
  <> " very"
  <> " long"
  <> " string"
  <> " indeed"
  <> " please"
  <> " break"
"#
    );
}

#[test]
fn const_concat_short_unbroken() {
    assert_format!(
        r#"const x = "some" <> "short" <> "string"
"#
    );
}

#[test]
fn const_concat_long_including_list() {
    assert_format_rewrite!(
        r#"const x = "some long string 1" <> "some long string 2" <> ["here is a list", "with several elements", "in order to make it be too long to fit on one line", "so we can see how it breaks", "onto multiple lines"] <> "and a last string"
"#,
        r#"const x = "some long string 1"
  <> "some long string 2"
  <> [
    "here is a list",
    "with several elements",
    "in order to make it be too long to fit on one line",
    "so we can see how it breaks",
    "onto multiple lines",
  ]
  <> "and a last string"
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/3397
#[test]
fn comment_after_case_branch() {
    assert_format!(
        r#"pub fn main() {
  case x {
    _ ->
      // comment
      [123]
  }
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3397
#[test]
fn comment_after_case_branch_case() {
    assert_format!(
        r#"pub fn main() {
  case x {
    _ ->
      // comment
      case y {
        _ -> todo
      }
  }
}
"#
    );
}

#[test]
fn label_shorthand_call_arg_is_split_like_regular_labelled_args() {
    assert_format!(
        r#"pub fn main() {
  wibble(
    a_punned_arg_that_is_super_long:,
    another_punned_arg:,
    yet_another_pun:,
    ok_thats_enough: wibble,
  )
}
"#
    );
}

#[test]
fn commented_label_shorthand_call_arg_is_split_like_regular_labelled_args() {
    assert_format!(
        r#"pub fn main() {
  wibble(
    // A comment here
    a_punned_arg_that_is_super_long:,
    another_punned_arg:,
    // And a comment there
    yet_another_pun:,
    ok_thats_enough: wibble,
  )
}
"#
    );
}

#[test]
fn label_shorthand_pattern_arg_is_split_like_regular_labelled_patterns() {
    assert_format!(
        r#"pub fn main() {
  let Wibble(
    a_punned_arg_that_is_super_long:,
    another_punned_arg:,
    yet_another_pun:,
    ok_thats_enough: wibble,
  ) = todo
}
"#
    );
}

#[test]
fn record_pattern_with_no_label_shorthand() {
    assert_format!(
        r#"pub fn main() {
  let Wibble(x: x) = todo
}
"#
    );
}

#[test]
fn record_with_no_label_shorthand() {
    assert_format!(
        r#"pub fn main() {
  Wibble(x: x)
}
"#
    );
}

#[test]
fn function_without_label_shorthand() {
    assert_format!(
        r#"pub fn main() {
  wibble(x: x)
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2015
#[test]
fn doc_comments_are_split_by_regular_comments() {
    assert_format!(
        r#"/// Doc comment
// Commented function
// fn wibble() {}

/// Other doc comment
pub fn main() {
  todo
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2015
#[test]
fn it_is_easy_to_tell_two_different_doc_comments_apart_when_a_regular_comment_is_separating_those()
{
    assert_format_rewrite!(
        r#"/// Doc comment
// regular comment
/// Other doc comment
pub fn main() {
  todo
}
"#,
        r#"/// Doc comment
// regular comment

/// Other doc comment
pub fn main() {
  todo
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2015
#[test]
fn multiple_commented_definitions_in_a_row_2() {
    assert_format!(
        r#"/// Stray comment
// regular comment

/// Stray comment
// regular comment

/// Doc comment
pub fn wibble() {
  todo
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2015
#[test]
fn only_stray_comments_and_definition_with_no_doc_comments() {
    assert_format!(
        r#"/// Stray comment
// regular comment

/// Stray comment
// regular comment

pub fn wibble() {
  todo
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2015
#[test]
fn only_stray_comments_and_definition_with_no_doc_comments_2() {
    assert_format_rewrite!(
        r#"/// Stray comment
// regular comment
pub fn wibble () {
  todo
}
"#,
        r#"/// Stray comment
// regular comment

pub fn wibble() {
  todo
}
"#
    );
}

#[test]
fn discard_in_pipe_is_not_turned_into_shorthand_label() {
    assert_format!(
        r#"pub fn main() {
  wibble |> wobble(one: 1, label: _, two: 2)
}
"#
    );
}

// Bug found by Louis
#[test]
fn internal_attribute_does_not_change_formatting_of_a_function() {
    assert_format!(
        r#"@internal
pub fn init(
  start: #(SupervisorFlags, List(ChildSpecification)),
) -> Result(#(Dynamic, Dynamic), never) {
  todo
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3627
#[test]
fn big_grapheme_cluster() {
    assert_format!(
        r#"pub fn main() {
  sw("👩‍👩‍👧‍👦👩‍👩‍👧‍👦👩‍👩‍👧‍👦", [])
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/3720
#[test]
fn record_inside_const_list() {
    assert_format_rewrite!(
        r#"const commands = [
  Command(
    "dev",
    "Start a file watcher that automatically re-compiles your app on all file changes.",
  ), Command("help", "Show this help text."),
]
"#,
        r#"const commands = [
  Command(
    "dev",
    "Start a file watcher that automatically re-compiles your app on all file changes.",
  ),
  Command("help", "Show this help text."),
]
"#
    );
}

#[test]
fn formatter_adds_todo_inside_empty_block() {
    assert_format_rewrite!(
        "pub fn main() {{}}",
        r#"pub fn main() {
  { todo }
}
"#
    );
}

#[test]
fn let_assert_as() {
    assert_format!(
        r#"pub fn main() {
  let assert 10 = 10 as "10 == 10"
}
"#
    );
}

#[test]
fn let_assert_as_long_message() {
    assert_format!(
        r#"pub fn main() {
  let assert Ok(10) = Ok(10)
    as "It's pretty obvious that this will never fail, but just in case, here is why."
}
"#
    );
}

#[test]
fn let_assert_as_long_message_and_value() {
    assert_format!(
        r#"pub fn main() {
  let assert Ok(something) =
    some_very_long_variable_which_always_represents_a_successful_result
    as "As you can see by the incredibly descriptive variable name, this operation never fails."
}
"#
    );
}

#[test]
fn let_assert_as_concatenated_message() {
    assert_format!(
        r#"pub fn main() {
  let assert 1 = 2 as { "This will" <> " " <> "crash" }
}
"#
    );
}

#[test]
fn let_assert_as_variable_message() {
    assert_format!(
        r#"pub fn main() {
  let message = "Hi :)"
  let assert 1 = 2 as message
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/4121
#[test]
fn function_capture_formatted_like_regular_calls() {
    assert_format!(
        r#"pub fn main() {
  capture(a, _, [
    really_long_thing_that_can_be_broken,
    something_else_for_good_measure,
  ])
}
"#
    );
}

#[test]
fn function_capture_formatted_like_regular_calls_2() {
    assert_format!(
        r#"pub fn main() {
  capture(
    a,
    _,
    really_long_thing_that_cannot_be_broken,
    something_else_for_good_measure,
  )
}
"#
    );
}

#[test]
fn function_capture_formatted_like_regular_calls_3() {
    assert_format!(
        r#"pub fn main() {
  list.fold(my_list, _, fn(a) {
    io.print("Meh")
    io.print("Meh")
  })
}
"#
    );
}

#[test]
fn function_capture_formatted_like_regular_calls_inside_a_long_list() {
    assert_format!(
        r#"pub fn main() {
  [
    capture(a, _, [
      really_long_thing_that_can_be_broken,
      something_else_for_good_measure,
    ]),
    regular_call(a, [
      really_long_thing_that_can_be_broken,
      something_else_for_good_measure,
    ]),
  ]
}
"#
    );
}

#[test]
fn function_capture_formatted_like_regular_calls_in_a_pipe() {
    assert_format!(
        r#"pub fn main() {
  [1, 2, 3]
  |> list.fold(from: 1, over: _, with: fn(a, b) {
    // a comment!
    a + b
  })
}
"#
    );
}

#[test]
fn assert() {
    assert_format!(
        "pub fn main() {
  assert True
}
"
    );
}

#[test]
fn assert_with_long_expression() {
    assert_format!(
        "pub fn main() {
  assert some_function_with_a_very_long_name_that_exceeds_the_eighty_character_limit()
}
"
    );
}

#[test]
fn assert_with_message() {
    assert_format!(
        r#"pub fn main() {
  assert True as "This is always true"
}
"#
    );
}

#[test]
fn assert_with_long_message() {
    assert_format!(
        r#"pub fn main() {
  assert True
    as "This should never panic, because it is a literal True value, and so will always be true."
}
"#
    );
}

#[test]
fn assert_with_long_expression_and_long_message() {
    assert_format!(
        r#"pub fn main() {
  assert some_long_function_name_which_if_everything_is_right_should_always_be_true
    as "This should never panic, because the function only ever returns true."
}
"#
    );
}

#[test]
fn echo_with_long_binary_expression() {
    assert_format!(
        r#"pub fn main() {
  echo wibble_wobble_wibble_wobble_wibble_wobble_wibble
    >= wibble_wobble_wibble_wobble_wibble_wobble_wibble

  echo wibble_wobble_wibble_wobble_wibble_wobble_wibble
    + wibble_wobble_wibble_wobble_wibble_wobble_wibble

  echo wibble_wobble_wibble_wobble_wibble_wobble_wibble
    == wibble_wobble_wibble_wobble_wibble_wobble_wibble
}
"#
    );
}

#[test]
fn assert_with_long_binary_expression() {
    assert_format!(
        r#"pub fn main() {
  assert wibble_wobble_wibble_wobble_wibble_wobble_wibble
    >= wibble_wobble_wibble_wobble_wibble_wobble_wibble

  assert wibble_wobble_wibble_wobble_wibble_wobble_wibble
    + wibble_wobble_wibble_wobble_wibble_wobble_wibble

  assert wibble_wobble_wibble_wobble_wibble_wobble_wibble
    == wibble_wobble_wibble_wobble_wibble_wobble_wibble
}
"#
    );
}

#[test]
fn comment_is_not_moved_after_assert() {
    assert_format!(
        "pub fn main() {
  // Wibble!
  assert True
}
"
    );
}

#[test]
fn todo_as_with_comment() {
    assert_format!(
        r#"pub fn main() {
  todo as
    // A little comment explaining something
    "wibble"
}
"#
    );
}

#[test]
fn todo_as_with_comment_on_the_same_line() {
    assert_format_rewrite!(
        r#"pub fn main() {
  todo as // A little comment explaining something
    "wibble"
}
"#,
        r#"pub fn main() {
  todo as
    // A little comment explaining something
    "wibble"
}
"#
    );
}

#[test]
fn todo_as_with_comment_before_the_as() {
    assert_format_rewrite!(
        r#"pub fn main() {
  todo // A little comment explaining something
    as "wibble"
}
"#,
        r#"pub fn main() {
  todo as
    // A little comment explaining something
    "wibble"
}
"#
    );
}

#[test]
fn panic_as_with_comment() {
    assert_format!(
        r#"pub fn main() {
  panic as
    // A little comment explaining something
    "wibble"
}
"#
    );
}

#[test]
fn panic_as_with_comment_on_the_same_line() {
    assert_format_rewrite!(
        r#"pub fn main() {
  panic as // A little comment explaining something
    "wibble"
}
"#,
        r#"pub fn main() {
  panic as
    // A little comment explaining something
    "wibble"
}
"#
    );
}

#[test]
fn panic_as_with_comment_before_the_as() {
    assert_format_rewrite!(
        r#"pub fn main() {
  panic // A little comment explaining something
    as "wibble"
}
"#,
        r#"pub fn main() {
  panic as
    // A little comment explaining something
    "wibble"
}
"#
    );
}

#[test]
fn echo_as_with_comment() {
    assert_format!(
        r#"pub fn main() {
  echo 1 as
    // A little comment explaining something
    "wibble"
}
"#
    );
}

#[test]
fn echo_as_with_comment_on_the_same_line() {
    assert_format_rewrite!(
        r#"pub fn main() {
  echo 1 as // A little comment explaining something
    "wibble"
}
"#,
        r#"pub fn main() {
  echo 1 as
    // A little comment explaining something
    "wibble"
}
"#
    );
}

#[test]
fn echo_as_with_comment_before_the_as() {
    assert_format_rewrite!(
        r#"pub fn main() {
  echo 1 // A little comment explaining something
    as "wibble"
}
"#,
        r#"pub fn main() {
  echo 1 as
    // A little comment explaining something
    "wibble"
}
"#
    );
}

#[test]
fn assert_as_with_comment() {
    assert_format!(
        r#"pub fn main() {
  assert True as
    // A little comment explaining something
    "wibble"
}
"#
    );
}

#[test]
fn assert_as_with_comment_on_the_same_line() {
    assert_format_rewrite!(
        r#"pub fn main() {
  assert True as // A little comment explaining something
    "wibble"
}
"#,
        r#"pub fn main() {
  assert True as
    // A little comment explaining something
    "wibble"
}
"#
    );
}

#[test]
fn assert_as_with_comment_before_the_as() {
    assert_format_rewrite!(
        r#"pub fn main() {
  assert True // A little comment explaining something
    as "wibble"
}
"#,
        r#"pub fn main() {
  assert True as
    // A little comment explaining something
    "wibble"
}
"#
    );
}
