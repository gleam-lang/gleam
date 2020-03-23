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
  Nil
}
"
    );

    //
    // ListCons
    //

    // TODO

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

    // TODO
    //    assert_format!(
    //        r#"fn main() {
    //  1 |> succ
    //}
    //"#
    //    );

    // TODO
    //    assert_format!(
    //        r#"fn main() {
    //  1 |> succ |> succ |> succ
    //}
    //"#
    //    );

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
  )
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

    //
    // Pattern::Int
    //

    //
    // Pattern::Float
    //

    //
    // Pattern::String
    //

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

    // TODO
    //    assert_format!(
    //        r#"fn main() {
    //  let _foo = 1
    //  Nil
    //}
    //"#
    //    );

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

    // TODO: more lists
    //    assert_format!(
    //        r#"fn main() {
    //  let [1] = 1
    //  Nil
    //}
    //"#
    //    );

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
    // Case
    //

    // TODO

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

    // TODO
    //    assert_format!(
    //        r#"fn main() {
    //  {tup.1}.2
    //}
    //"#
    //    );

    //
    // Todo
    //

    assert_format!(
        "fn main() {
  todo
}
"
    );
}
