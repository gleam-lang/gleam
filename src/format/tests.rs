use super::*;

#[test]
fn module_test() {
    macro_rules! assert_format {
        ($src:expr $(,)?) => {
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

    //
    // Simple exprs
    //

    assert_format!(
        "fn main() {
  todo
}
"
    );

    assert_format!(
        "fn main() {
  1
}
"
    );

    assert_format!(
        "fn main() {
  1.0
}
"
    );

    assert_format!(
        "fn main() {
  Nil
}
"
    );

    assert_format!(
        "fn main() {
  tuple(-1, -2.9)
}
"
    );

    assert_format!(
        "fn main() {
  []
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
        r#"fn main() {
  Person(name: "Al", is_cool: VeryTrue)
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
    // Module function args
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
        r#"fn main(
) -> Loooooooooooooooooooong(
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
        r#"fn main(
) -> Loooooooooooooooooooong(Loooooooooooooooooooooooooooooooooooooooooong) {
  Nil
}
"#
    );

    //
    // Field access
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
}

// fn fully_typed(first: Int) -> Int {
//     first + 1
// }

// fn lets() {
//     let x = 1
//     let y = 2
//     x + y
// }

// fn patterns(x) {
//     case x {
//         1 -> 42
//         _other -> {
//             let x = 3
//             3 + 4
//         }
//     }
// }
