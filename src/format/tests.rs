use super::*;

#[test]
fn module_test() {
    macro_rules! assert_format {
        ($src:expr $(,)?) => {
            println!("\n\n\n{}", $src);
            let src = $src.to_string();
            assert_eq!(src, pretty($src).unwrap());
        };
    }

    //
    // Imports
    //

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
    assert_format!("import my/cool/module.{\n  Ane, Bwo, Chree, Dour, Eive, Fix, Geven, Hight, Iine, Jen, Kleven, Lwelve,\n  Mhirteen, Nifteen, Oixteen,\n}\n");
    assert_format!("import gleam/result.{\n  Aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, Abcde,\n  End,\n}\n");

    //
    // Multiple statements
    //

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

    //
    // External types
    //

    assert_format!("external type Private\n");
    assert_format!("external type Box(a)\n");
    assert_format!("external type Box(a, b, zero)\n");
    assert_format!("pub external type Private\n");
    assert_format!("pub external type Box(a)\n");
    assert_format!("pub external type Box(a, b, zero)\n");

    //
    // External fn
    //

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

    //
    // Type aliases
    //

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
  tuple(a, b)
"
    );

    assert_format!(
        "pub type Sixteen(element) =
  tuple(
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
  ) -> tuple(
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
    //  tuple(
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

    //
    // Custom types
    //

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
      tuple(Int, fn(a, a, a, a, a, a, a) -> List(a)),
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

    //
    // Expr::Fn
    //

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

    //
    // Call exprs
    //

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

    //
    // Tuple
    //

    assert_format!(
        r#"fn main(one, two, three) {
  tuple(
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
  tuple(
    atom.create_from_string("module"),
    atom.create_from_string("gleam@otp@actor"),
  )
}
"#
    );

    //
    // Fn
    //

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

    //
    // Binary operators
    //

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

    //
    // Int
    //

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

    //
    // Float
    //

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

    //
    // String
    //

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

    //
    // Seq
    //

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

    //
    // Var
    //

    assert_format!(
        r#"fn main() {
  one
}
"#
    );

    //
    // ListNil
    //

    assert_format!(
        "fn main() {
  []
}
"
    );

    //
    // ListCons
    //

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

    //
    // Call
    //

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

    //
    // Pipe
    //

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
  tuple(
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
  let x = 1
    |> succ
    |> succ
  x
}
"#
    );

    assert_format!(
        r#"fn main() {
  tuple(1, 2)
  |> pair.first
  |> should.equal(1)
}
"#
    );

    assert_format!(
        r#"fn main() {
  tuple(1, 2)
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

    //
    // Let
    //

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
  let dict = map.from_list(
    [tuple("a", 0), tuple("b", 1), tuple("c", 2), tuple("d", 3)],
  )
  1
}
"#
    );

    //
    // Pattern::Float
    //

    assert_format!(
        r#"fn main() {
  let 1 = 1
  Nil
}
"#
    );

    //
    // Pattern::String
    //

    assert_format!(
        r#"fn main() {
  let 1.0 = 1
  Nil
}
"#
    );

    //
    // Pattern::Var
    //

    assert_format!(
        r#"fn main() {
  let x = 1
  let y = 1
  Nil
}
"#
    );

    //
    // Pattern::Let
    //

    assert_format!(
        r#"fn main() {
  let x as y = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let tuple(x, y, 123 as z) = 1
  Nil
}
"#
    );

    //
    // Pattern::Discard
    //

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

    //
    // Pattern::Nil
    //

    assert_format!(
        r#"fn main() {
  let [] = 1
  Nil
}
"#
    );

    //
    // Pattern::Cons
    //

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

    //
    // Pattern::Constructor
    //

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
  let Person(
    age: really_long_variable_name,
    name: really_long_variable_name,
  ) = 1
  Nil
}
"#
    );

    //
    // Pattern::Tuple
    //

    assert_format!(
        r#"fn main() {
  let tuple() = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let tuple(x) = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let tuple(x, y) = 1
  Nil
}
"#
    );

    assert_format!(
        r#"fn main() {
  let tuple(x, y, z) = 1
  Nil
}
"#
    );

    //
    // Block causes
    //

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

    //
    // Case
    //

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

    //
    // Nested case
    //

    assert_format!(
        r#"fn main() {
  case 1 {
    1 -> case x {
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
    [x] -> case x {
      _ -> 1
    }
  }
}
"#
    );

    //
    // Case then fn
    //

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

    //
    // Multiple subjects
    //

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

    //
    // Alternative patterns
    //

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

    //
    // Clause guards
    //

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
  case tuple(1, 2, 3) {
    _ if x == tuple(1, 2, 3) -> Nil
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

    //
    // FieldAccess
    //

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

    //
    // Tuple
    //

    assert_format!(
        r#"fn main() {
  tuple()
}
"#
    );

    assert_format!(
        r#"fn main() {
  tuple(1)
}
"#
    );

    assert_format!(
        r#"fn main() {
  tuple(1, 2)
}
"#
    );

    assert_format!(
        r#"fn main() {
  tuple(1, 2, 3)
}
"#
    );

    assert_format!(
        r#"fn main() {
  tuple(
    really_long_variable_name,
    really_long_variable_name,
    really_long_variable_name,
    really_long_variable_name,
    really_long_variable_name,
  )
}
"#
    );

    //
    // TupleIndex
    //

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

    //
    // Todo
    //

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

    //
    // Doc comments
    //
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

    //
    // Comments
    //

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

    //
    // Trailing comments
    //

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

    //
    // Commented function arguments
    //

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

    //
    // Commented bin op expressions
    //

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

    //
    // Commented external function arguments
    //

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

    //
    // Commented type constructors
    //

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

    //
    // Commented type constructor arguments
    //

    //
    // Commented type constructor parameters
    //

    //
    // Function captures
    //

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

    // Formats the operator spread syntax
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
    really_long_variable_name_a,
    c: really_long_variable_name_c,
    ..,
  ) = triple
  really_long_variable_name_c
}
"
    );

    //
    // Empty lines
    //

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

    assert_format!(
        "pub fn main() {
  let inc = fn(a) { a + 1 }

  pair.map_first(tuple(1, 2), inc)
  |> should.equal(tuple(2, 2))

  pair.map_first(tuple(1, 2), inc)
  |> should.equal(tuple(2, 2))
}
"
    );

    //
    // Module docs
    //

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

    //
    // Binary operator precedence
    //

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

    //
    // Module level constants
    //

    assert_format!(
        "pub const str = \"a string\"

const my_constant: String = \"Hello\"

pub const int = 4

pub const float = 3.14
"
    );

    // Record update
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
  let c = Counter(
    ..c,
    loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong: 1,
  )
  c
}
"
    );

    //
    // Concise wrapping of simple lists
    //

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
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 1.00, 11.0, 12.0, 13.0, 14.0,
    15.0, 16.0, 17.0, 18.0, 19.0, 2.00,
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
  1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 1.00, 11.0, 12.0, 13.0, 14.0,
  15.0, 16.0, 17.0, 18.0, 19.0, 2.00,
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

    //
    // Concise wrapping of simple bit strings
    //

    assert_format!(
        "pub fn main() {
  <<
    100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400,
    1500, 1600, 1700, 1800, 1900, 2000,
  >>
}
"
    );

    assert_format!(
        "pub fn main() {
  <<
    1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 1.00, 11.0, 12.0, 13.0, 14.0,
    15.0, 16.0, 17.0, 18.0, 19.0, 2.00,
  >>
}
"
    );

    assert_format!(
        r#"pub fn main() {
  <<
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    "ten", "eleven", "twelve",
  >>
}
"#
    );

    assert_format!(
        "const values = <<
  100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400,
  1500, 1600, 1700, 1800, 1900, 2000,
>>
"
    );

    assert_format!(
        "const values = <<
  1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 1.00, 11.0, 12.0, 13.0, 14.0,
  15.0, 16.0, 17.0, 18.0, 19.0, 2.00,
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
fn module_rewrites_test() {
    macro_rules! assert_format_rewrite {
        ($src:expr, $output:expr  $(,)?) => {
            assert_eq!(pretty($src).unwrap(), $output);
        };
    }

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
