use super::*;

use pretty_assertions::assert_eq;

macro_rules! assert_format {
    ($src:expr $(,)?) => {
        let mut writer = String::new();
        pretty(&mut writer, $src, std::path::Path::new("<stdin>")).unwrap();
        assert_eq!($src, writer);
    };
}

macro_rules! assert_format_rewrite {
    ($src:expr, $output:expr  $(,)?) => {
        let mut writer = String::new();
        pretty(&mut writer, $src, std::path::Path::new("<stdin>")).unwrap();
        assert_eq!(writer, $output);
    };
}

#[test]
fn imports() {
    assert_format!("\n");
    assert_format!("import one\n");
    assert_format!("import one\nimport two\n");
    assert_format!("import one/two/three\n");
    assert_format!("import one/two/three\nimport four/five\n");
    assert_format!("import one.{fun, fun2, fun3}\n");
    assert_format!("import one.{One, Two, fun1, fun2}\n");
    assert_format!("import one.{main as entrypoint}\n");
    assert_format!("import one/two/three as free\n");
    assert_format!("import one/two/three.{thunk} as free\n");
    assert_format!("import one/two/three.{thunk as funky} as free\n");
    assert_format!(
        "import my/cool/module.{
  Ane, Bwo, Chree, Dour, Eive, Fix, Geven, Hight, Iine, Jen, Kleven, Lwelve, Mhirteen,
  Nifteen, Oixteen,
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
import two
import three

pub external type One

pub external type Two

pub external type Three

pub external type Four
"#
    );
}

#[test]
fn external_types() {
    assert_format!("external type Private\n");
    assert_format!("external type Box(a)\n");
    assert_format!("external type Box(a, b, zero)\n");
    assert_format!("pub external type Private\n");
    assert_format!("pub external type Box(a)\n");
    assert_format!("pub external type Box(a, b, zero)\n");
}

#[test]
fn external_fn() {
    assert_format!(
        r#"external fn main() -> Int =
  "app" "main"
"#
    );

    assert_format!(
        r#"external fn main() -> program.Exit =
  "app" "main"
"#
    );

    assert_format!(
        r#"external fn main(List(String)) -> Int =
  "app" "main"
"#
    );

    assert_format!(
        r#"external fn main(argv: List(String)) -> Int =
  "app" "main"
"#
    );

    assert_format!(
        r#"external fn main(
  a_really_long_argument_label: List(String),
  another_really_long_argument_label: whatever,
) -> Int =
  "app" "main"
"#
    );

    assert_format!(
        r#"external fn main(
  a_really_long_argument_label: List(String),
  another_really_long_argument_label: whatever,
) -> Container(
  WowThisTypeHasJustTheLongestName,
  WowThisTypeHasJustTheLongestName,
  WowThisTypeHasJustTheLongestName,
) =
  "app" "main"
"#
    );

    assert_format!(
        r#"external fn erl_filter(
  fn(key, value) -> Bool,
  Map(key, value),
) -> Map(key, value) =
  "maps" "filter"
"#
    );

    assert_format!(
        r#"///
external fn x(a, b, c) -> #(a, b, c) =
  "" ""
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
fn custom_types() {
    assert_format!(
        "type WowThisTypeHasJustTheLongestName(
  some_long_type_variable,
  and_another,
  and_another_again,
) {
  Make
}
"
    );

    assert_format!(
        "type Result(a, e) {
  Ok(a)
  Error(e)
}
"
    );

    assert_format!(
        "type Result(a, e) {
  Ok(value: a)
  Error(error: e)
}
"
    );

    assert_format!(
        "type SillyResult(a, e) {
  Ok(
    first_value_with_really_long_name: a,
    second_value_with_really_long_name: a,
  )
  Error(error: e)
}
"
    );

    assert_format!(
        "type SillyResult(a, e) {
  Ok(
    first_value_with_really_long_name: a,
    second_value_with_really_long_name: List(
      #(Int, fn(a, a, a, a, a, a, a) -> List(a)),
    ),
  )
  Error(error: e)
}
"
    );

    assert_format!(
        "type X {
  X(
    start: fn() -> a_reall_really_long_name_goes_here,
    stop: fn() -> a_reall_really_long_name_goes_here,
  )
}
"
    );

    assert_format!(
        "pub opaque type X {
  X
}
"
    );

    assert_format!(
        "///
pub type Option(a) {
  None
}
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
  Ok(
    1,
    {
      1
      2
    },
  )
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
  thingy(wiggle(my_function(
    // ok!
    one(),
    two(),
  )))
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
  #(
    1,
    {
      1
      2
    },
  )
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

    assert_format!(
        r#"fn main(label_one one, label_two two, label_three three) {
  Nil
}
"#
    );

    assert_format!(
        r#"fn main(label_one one: One, label_two two: Two) {
  Nil
}
"#
    );

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

    assert_format!(
        r#"fn main(label _discarded) {
  Nil
}
"#
    );

    // https://github.com/gleam-lang/gleam/issues/613
    assert_format!(
        r#"fn main() {
  Nil
  // Done
}
"#
    );

    //
    // Module function return annotations
    //

    assert_format!(
        r#"fn main() -> Nil {
  Nil
}
"#
    );

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

    assert_format!(
        r#"fn main() -> Loooooooooooooooooooong(
  Loooooooooooooooooooooooooooooooooooooooooong,
) {
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() -> program.Exit {
  Nil
}
"#
    );

    assert_format!(
        "fn order(first: Set(member), second: Set(member)) -> #(Set(member), Set(member)) {
  Nil
}
"
    );

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
        r#"fn main() {
  1
}
"#
    );

    assert_format!(
        r#"fn main() {
  121234345989000
}
"#
    );

    assert_format!(
        r#"fn main() {
  -12928347925
}
"#
    );

    assert_format!("fn a() {\n  1_234_567\n}\n");
    assert_format!("fn a() {\n  0xCA_B0_05E\n}\n");
    assert_format!("fn a() {\n  0b10_10_0001\n}\n");
    assert_format!("fn a() {\n  0o12_34_567\n}\n");
}

#[test]
fn expr_float() {
    assert_format!(
        r#"fn main() {
  1.0
}
"#
    );

    assert_format!(
        r#"fn main() {
  -1.0
}
"#
    );

    assert_format!(
        r#"fn main() {
  9999.6666
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
  { os.system_time(os.Millisecond) < june_12_2020 * 1000000 }
  |> should.equal(True)
}
"#
    );

    assert_format!(
        r#"fn main() {
  { os.system_time(os.Millisecond) < june_12_2020 * 1000000 }
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

    assert_format!(
        r#"fn main() {
  assert x = 1
  Nil
}
"#
    );

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

    assert_format!(
        r#"fn main() {
  let x = fn(x) { x }
  x
}
"#
    );

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
    one: one,
    two: two,
    three: three,
    four: four,
    five: five,
    six: six,
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
  let _foo = 1
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
  let Person(name, age: age) = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let Person(name: name, age: age) = 1
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

    assert_format!(
        r#"fn main() {
  tup.1
}
"#
    );

    assert_format!(
        r#"fn main() {
  tup.777
}
"#
    );

    assert_format!(
        r#"fn main() {
  {tup.1}.2
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

    assert_format!(
        r#"fn main() {
  todo("todo with a label")
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

    assert_format!(
        "/// one
///two
fn main() {
  Nil
}
"
    );

    assert_format!(
        r#"/// one
///two
external fn whatever() -> Nil =
  "" ""
"#
    );

    assert_format!(
        r#"/// one
///two
external type Thingy
"#
    );

    assert_format!(
        r#"/// one
///two
external type Thingy
"#
    );

    assert_format!(
        r#"/// one
///two
type Whatever {
  Whatever
}
"#
    );

    assert_format!(
        r#"/// one
///two
type Whatever =
  Int
"#
    );

    assert_format!(
        r#"import one

/// one
///two
type Whatever {
  Whatever
}
"#
    );
}

#[test]
fn comments() {
    assert_format!(
        r#"import one

// one
//two
type Whatever {
  Whatever
}
"#
    );

    assert_format!(
        r#"import one

// one
//two
/// three
type Whatever {
  Whatever
}
"#
    );

    assert_format!(
        "// one
fn main() {
  Nil
}
"
    );

    assert_format!(
        "// one
//two
fn main() {
  Nil
}
"
    );

    assert_format!(
        r#"// one
//two
external fn whatever() -> Nil =
  "" ""
"#
    );

    assert_format!(
        r#"// one
//two
external type Thingy
"#
    );

    assert_format!(
        r#"// one
//two
external type Thingy
"#
    );

    assert_format!(
        r#"// one
//two
type Whatever {
  Whatever
}
"#
    );

    assert_format!(
        r#"// one
//two
type Whatever =
  Int
"#
    );

    assert_format!(
        r#"// zero
import one

// one
//two
type Whatever {
  Whatever
}
"#
    );

    assert_format!(
        "fn main() {
  // Hello
  \"world\"
}
"
    );

    assert_format!(
        "fn main() {
  // Hello
  // world
  1
}
"
    );

    assert_format!(
        "fn main() {
  // Hello
  // world
  1.0
}
"
    );

    assert_format!(
        "fn main() {
  // Hello
  // world
  Nil
}
"
    );

    assert_format!(
        "fn main() {
  // Hello
  // world
  []
}
"
    );

    // TODO: The comment should not be on the same line as the element.
    assert_format!(
        "fn main() {
  // Hello
  // world
  [
    // One
    1, // Two
    2,
  ]
}
"
    );

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

    assert_format!(
        "fn main() {
  // Hello
  1
  // world
  2
}
"
    );

    assert_format!(
        "fn main() {
  let // hello
  x = 1
  x
}
"
    );

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

    assert_format!(
        "pub external fn main(
  // comment1
  argument1: Type,
  // comment2
  argument2: Type,
) -> Int =
  \"\" \"\"
"
    );
}

#[test]
fn commented_binop() {
    assert_format!(
        "fn main() {
  1 + // hello
  2
}
"
    );

    assert_format!(
        "fn main() {
  // one
  1 + // two
  2 + // three
  3
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
    assert_format!(
        "pub fn main() {
  run(_)
}
"
    );

    assert_format!(
        "pub fn main() {
  run(1, 2, _, 4, 5)
}
"
    );

    assert_format!(
        "pub fn main() {
  run(1, 2, _, 4, 5)(_)
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
  3 * {
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
  } * 3
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
fn expr_bit_string() {
    // BitString construction

    assert_format!(
        "fn main() {
  let a = 1
  let x = <<1, a, 2:binary>>
  let size = <<3:2, 4:size(3), 5:binary-size(4), 6:size(a)>>
  let unit = <<7:unit(1), 8:binary-unit(2)>>
  x
}
",
    );

    // BitString

    assert_format!(
        "fn main() {
  let a = 1
  let <<b, c, d:binary>> = <<1, a, 2:binary>>
  b
}
",
    );

    assert_format!(
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
fn record_update() {
    assert_format!(
        "pub type Counter {
  Counter(a: Int, b: Int)
}

fn main() {
  let c = Counter(0, 0)
  let c = Counter(..c, a: c.a + 1, b: c.a + c.b)
  c
}
"
    );

    // Long record updates are split onto multiple lines
    assert_format!(
        "pub type Counter {
  Counter(loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong: Int)
}

fn main() {
  let c = Counter(0)
  let c =
    Counter(
      ..c,
      loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong: 1,
    )
  c
}
"
    );
}

#[test]
fn concise_wrapping_of_simple_lists() {
    assert_format!(
        "pub fn main() {
  [
    100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500,
    1600, 1700, 1800, 1900, 2000,
  ]
}
"
    );

    assert_format!(
        "pub fn main() {
  [
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 1.00, 11.0, 12.0, 13.0, 14.0, 15.0,
    16.0, 17.0, 18.0, 19.0, 2.00,
  ]
}
"
    );

    assert_format!(
        r#"pub fn main() {
  [
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
    "eleven", "twelve",
  ]
}
"#
    );

    assert_format!(
        "const values = [
  100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500,
  1600, 1700, 1800, 1900, 2000,
]
"
    );

    assert_format!(
        "const values = [
  1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 1.00, 11.0, 12.0, 13.0, 14.0, 15.0,
  16.0, 17.0, 18.0, 19.0, 2.00,
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
fn concise_wrapping_of_simple_bit_strings() {
    assert_format!(
        "pub fn main() {
  <<
    100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500,
    1600, 1700, 1800, 1900, 2000,
  >>
}
"
    );

    assert_format!(
        "pub fn main() {
  <<
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 1.00, 11.0, 12.0, 13.0, 14.0, 15.0,
    16.0, 17.0, 18.0, 19.0, 2.00,
  >>
}
"
    );

    assert_format!(
        r#"pub fn main() {
  <<
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
    "eleven", "twelve",
  >>
}
"#
    );

    assert_format!(
        "const values = <<
  100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500,
  1600, 1700, 1800, 1900, 2000,
>>
"
    );

    assert_format!(
        "const values = <<
  1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 1.00, 11.0, 12.0, 13.0, 14.0, 15.0,
  16.0, 17.0, 18.0, 19.0, 2.00,
>>
"
    );

    assert_format!(
        r#"const values = <<
  "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
  "eleven", "twelve",
>>
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
  assert Ok(1) = [
    100000000000000000000000000000, 200000000000000000000000000000, 300000000000000000000000000000,
    400000000000000000000000000000,
  ]
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  assert <<11, 2, 4, 5, 6>> = [
    100000000000000000000000000000, 200000000000000000000000000000, 300000000000000000000000000000,
    400000000000000000000000000000,
  ]
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  assert [11, 2, 4, 5, 6] = [
    100000000000000000000000000000, 200000000000000000000000000000, 300000000000000000000000000000,
    400000000000000000000000000000,
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
  assert x = 1
}
"
    );

    assert_format!(
        "pub fn main() {
  assert x = {
    assert y = 1
  }
}
"
    );
}

#[test]
fn block_containing_try() {
    assert_format!(
        "pub fn main() {
  let _ = {
    try _ = 1
    2
  }
}
"
    );

    assert_format!(
        "pub fn main() {
  #(
    {
      try _ = 1
      2
    },
  )
}
"
    );
}

#[test]
fn case_in_call() {
    assert_format!(
        "fn clause_guard_tests(_fns) -> List(Test) {
  example(fn() {
    assert_equal(
      0,
      case Nil {
        _ if yes -> 0
        _ -> 1
      },
    )
  })
}
"
    );
}

#[test]
fn statement_if() {
    assert_format!(
        "external type X

if erlang {
  type Y {
    Y
  }
}

if javascript {
  external type Y
}
"
    );
}

#[test]
fn statement_if_multiple() {
    assert_format!(
        "external type X

if erlang {
  type Y {
    Y
  }

  type Z {
    Z
  }
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/1184
#[test]
fn try_empty_line() {
    assert_format!(
        "pub fn main(x) {
  try _ = x
  try _ = x
  try _ = x

  let x = x
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
