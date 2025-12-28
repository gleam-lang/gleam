use crate::{assert_js, assert_ts_def};

#[test]
fn exported_functions() {
    assert_js!(
        r#"
pub fn add(x, y) {
    x + y
}"#,
    );
}

#[test]
fn calling_functions() {
    assert_js!(
        r#"
pub fn twice(f: fn(t) -> t, x: t) -> t {
  f(f(x))
}
pub fn add_one(x: Int) -> Int {
  x + 1
}
pub fn add_two(x: Int) -> Int {
  twice(add_one, x)
}

pub fn take_two(x: Int) -> Int {
  twice(fn(y) {y - 1}, x)
}
"#,
    );
}

#[test]
fn function_formatting() {
    assert_js!(
        r#"
pub fn add(the_first_variable_that_should_be_added, the_second_variable_that_should_be_added) {
  the_first_variable_that_should_be_added + the_second_variable_that_should_be_added
}"#,
    );
}

#[test]
fn function_formatting1() {
    assert_js!(
        r#"
pub fn this_function_really_does_have_a_ludicrously_unfeasibly_long_name_for_a_function(x, y) {
x + y
}"#,
    );
}

#[test]
fn function_formatting2() {
    assert_js!(
        r#"
pub fn add(x, y) {
x + y
}

pub fn long() {
  add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, add(1, 1)))))))))))))))
}"#,
    );
}

#[test]
fn function_formatting3() {
    assert_js!(
        r#"
pub fn math(x, y) {
  fn() {
    x + y
    x - y
    2 * x
  }
}"#,
    );
}

#[test]
fn function_formatting_typescript() {
    assert_ts_def!(
        r#"
pub fn add(the_first_variable_that_should_be_added, the_second_variable_that_should_be_added) {
  the_first_variable_that_should_be_added + the_second_variable_that_should_be_added
}"#,
    );
}

#[test]
fn function_formatting_typescript1() {
    assert_ts_def!(
        r#"
pub fn this_function_really_does_have_a_ludicrously_unfeasibly_long_name_for_a_function(x, y) {
x + y
}"#,
    );
}

#[test]
fn tail_call() {
    assert_js!(
        r#"
pub fn count(xs, n) {
  case xs {
    [] -> n
    [_, ..xs] -> count(xs, n + 1)
  }
}
"#,
    );
}

#[test]
fn tail_call_doesnt_clobber_tail_position_tracking() {
    assert_js!(
        r#"
pub fn loop(indentation) {
  case indentation > 0 {
    True -> loop(indentation - 1)
    False -> Nil
  }
}
"#,
    );
}

#[test]
fn pipe_last() {
    assert_js!(
        r#"fn id(x) { x }
pub fn main() {
  1
  |> id
}
"#,
    );
}

#[test]
fn calling_fn_literal() {
    assert_js!(
        r#"pub fn main() {
  fn(x) { x }(1)
}
"#,
    );
}

// Don't mistake calling a function with the same name as the current function
// as tail recursion
#[test]
fn shadowing_current() {
    assert_js!(
        r#"pub fn main() {
  let main = fn() { 0 }
  main()
}
"#,
    );
}

#[test]
fn recursion_with_discards() {
    assert_js!(
        r#"pub fn main(f, _) {
  f()
  main(f, 1)
}
"#,
    );
}

#[test]
fn no_recur_in_anon_fn() {
    assert_js!(
        r#"pub fn main() {
  fn() { main() }
  1
}
"#,
    );
}

#[test]
fn case_in_call() {
    assert_js!(
        r#"pub fn main(f, x) {
  f(case x {
    1 -> 2
    _ -> 0
  })
}
"#,
    );
}

#[test]
fn reserved_word_fn() {
    assert_js!(
        r#"pub fn class() {
  Nil
}
"#,
    );
}

#[test]
fn reserved_word_imported() {
    assert_js!(
        ("for", "pub fn class() { 1 }"),
        r#"import for.{class}

pub fn export() {
  class()
}
"#,
    );
}

#[test]
fn reserved_word_imported_alias() {
    assert_js!(
        ("for", "pub fn class() { 1 }"),
        r#"import for.{class as while} as function

pub fn export() {
  let delete = function.class
  while()
}
"#,
    );
}

#[test]
fn reserved_word_const() {
    assert_js!(
        r#"const in = 1

pub fn export() {
  in
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/1208
#[test]
fn reserved_word_argument() {
    assert_js!(
        r#"pub fn main(with) {
  with
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/1186
#[test]
fn multiple_discard() {
    assert_js!(
        r#"pub fn main(_, _, _) {
  1
}
"#,
    );
}

#[test]
fn keyword_in_recursive_function() {
    assert_js!(
        r#"pub fn main(with: Int) -> Nil {
  main(with - 1)
}
"#,
    );
}

#[test]
fn reserved_word_in_function_arguments() {
    assert_js!(
        r#"pub fn main(arguments, eval) {
  #(arguments, eval)
}
"#,
    );
}

#[test]
fn let_last() {
    assert_js!(
        r#"pub fn main() {
  let x = 1
}
"#,
    );
}

#[test]
fn assert_last() {
    assert_js!(
        r#"pub fn main() {
  let assert x = 1
}
"#,
    );
}

#[test]
fn fn_return_fn_typescript() {
    assert_ts_def!(
        r#"pub fn main(f: fn(Int) -> Int) {
  let func = fn(x, y) { f(x) + f(y) }
  func
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/1637
#[test]
fn variable_rewriting_in_anon_fn_with_matching_parameter() {
    assert_js!(
        r#"pub fn bad() {
  fn(state) {
    let state = state
    state
  }
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/1637
#[test]
fn variable_rewriting_in_anon_fn_with_matching_parameter_in_case() {
    assert_js!(
        r#"pub fn bad() {
  fn(state) {
    let state = case Nil {
      _ -> state
    }
    state
  }
}
"#,
    );
}

// https://github.com/gleam-lang/gleam/issues/1508
#[test]
fn pipe_variable_rebinding() {
    assert_js!(
        "
pub fn main() {
  let version = 1 |> version()
  version
}

pub fn version(n) {
  Ok(1)
}"
    )
}

#[test]
fn pipe_shadow_import() {
    assert_js!(
        ("wibble", "pub fn println(x: String) {  }"),
        r#"
        import wibble.{println}
        pub fn main() {
          let println =
            "oh dear"
            |> println
          println
        }"#
    );
}

#[test]
fn module_const_fn() {
    assert_js!(
        r#"
pub fn int_identity(i: Int) -> Int { i }
pub const int_identity_alias: fn(Int) -> Int = int_identity
pub fn use_int_identity_alias() { int_identity_alias(42) }

pub const compound: #(fn(Int) -> Int, fn(Int) -> Int) = #(int_identity, int_identity_alias)
pub fn use_compound() { compound.0(compound.1(42)) }"#
    );
}

#[test]
fn module_const_fn1() {
    assert_ts_def!(
        r#"
pub fn int_identity(i: Int) -> Int { i }
pub const int_identity_alias: fn(Int) -> Int = int_identity
pub const compound: #(fn(Int) -> Int, fn(Int) -> Int) =
    #(int_identity, int_identity_alias)"#
    )
}

// https://github.com/gleam-lang/gleam/issues/2399
#[test]
fn bad_comma() {
    assert_js!(
        r#"
fn function_with_a_long_name_that_is_intended_to_sit_right_on_the_limit() {
  Nil
}

fn identity(x) {
  x
}

pub fn main() {
  function_with_a_long_name_that_is_intended_to_sit_right_on_the_limit()
  |> identity
}
"#
    )
}

// https://github.com/gleam-lang/gleam/issues/2518
#[test]
fn function_literals_get_properly_wrapped_1() {
    assert_js!(
        r#"pub fn main() {
  fn(n) { n + 1 }(10)
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2518
#[test]
fn function_literals_get_properly_wrapped_2() {
    assert_js!(
        r#"pub fn main() {
  { fn(n) { n + 1 } }(10)
}
"#
    );
}

// https://github.com/gleam-lang/gleam/issues/2518
#[test]
fn function_literals_get_properly_wrapped_3() {
    assert_js!(
        r#"pub fn main() {
  { let a = fn(n) { n + 1 } }(10)
}
"#
    );
}

#[test]
fn labelled_argument_ordering() {
    // https://github.com/gleam-lang/gleam/issues/3671
    assert_js!(
        "
type A { A }
type B { B }
type C { C }
type D { D }

fn wibble(a a: A, b b: B, c c: C, d d: D) {
  Nil
}

pub fn main() {
  wibble(A, C, D, b: B)
  wibble(A, C, D, b: B)
  wibble(B, C, D, a: A)
  wibble(B, C, a: A, d: D)
  wibble(B, C, d: D, a: A)
  wibble(B, D, a: A, c: C)
  wibble(B, D, c: C, a: A)
  wibble(C, D, b: B, a: A)
}
"
    );
}

// During the implementation of https://github.com/gleam-lang/gleam/pull/4337,
// a bug was found where this code would compile incorrectly.
#[test]
fn two_pipes_in_a_row() {
    assert_js!(
        "
pub type Function(a) {
  Function(fn() -> a)
}

pub fn main() {
  [fn() { 1 } |> Function, fn() { 2 } |> Function]
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4472
#[test]
fn pipe_into_block() {
    assert_js!(
        "
fn side_effects(x) { x }

pub fn main() {
  1
  |> side_effects
  |> {
    side_effects(2)
    side_effects
  }
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4472
#[test]
fn pipe_with_block_in_the_middle() {
    assert_js!(
        "
fn side_effects(x) { x }

pub fn main() {
  1
  |> side_effects
  |> {
    side_effects(2)
    side_effects
  }
  |> side_effects
}
"
    );
}

// https://github.com/gleam-lang/gleam/issues/4533
#[test]
fn immediately_invoked_function_expressions_include_statement_level() {
    assert_js!(
        "
fn identity(x) { x }

pub type Wibble {
  Wibble(a: Int, b: Int)
}

pub fn main() {
  let w = Wibble(1, 2)
  identity(Wibble(..w |> identity, b: 4)) |> identity
}
"
    );
}

#[test]
fn public_function_gets_jsdoc() {
    assert_js!(
        "
/// Hello! This is the documentation of the `main`
/// function.
///
pub fn main() { 1 }
"
    );
}

#[test]
fn internal_function_gets_ignored_jsdoc() {
    assert_js!(
        "
/// Hello! This is the documentation of the `main`
/// function, which is internal!
///
@internal
pub fn main() { 1 }
"
    );
}

#[test]
fn star_slash_in_jsdoc() {
    assert_js!(
        "
/// */
///
pub fn main() { 1 }
"
    );
}
