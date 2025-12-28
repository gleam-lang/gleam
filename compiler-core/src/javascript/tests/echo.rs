use crate::assert_js;

#[test]
pub fn echo_with_a_simple_expression() {
    assert_js!(
        r#"
pub fn main() {
  echo 1
}
"#
    );
}

#[test]
pub fn echo_with_a_simple_expression_and_a_message() {
    assert_js!(
        r#"
pub fn main() {
  echo 1 as "hello!"
}
"#
    );
}

#[test]
pub fn echo_with_complex_expression_as_a_message() {
    assert_js!(
        r#"
pub fn main() {
  echo 1 as case name() {
    "Giacomo" -> "hello Jak!"
    _ -> "hello!"
  }
}

fn name() { "Giacomo" }
"#
    );
}

#[test]
pub fn echo_evaluates_printed_value_before_message() {
    assert_js!(
        r#"
pub fn main() {
  echo name() as case name() {
    "Giacomo" -> "hello Jak!"
    _ -> "hello!"
  }
}

fn name() { "Giacomo" }
"#
    );
}

#[test]
pub fn echo_with_a_block_as_a_message() {
    assert_js!(
        r#"
pub fn main() {
  echo 1 as {
    let name = "Giacomo"
    "Hello, " <> name
  }
}
"#
    );
}

#[test]
pub fn multiple_echos_inside_expression() {
    assert_js!(
        r#"
pub fn main() {
  echo 1
  echo 2
}
"#
    );
}

#[test]
pub fn echo_with_a_case_expression() {
    assert_js!(
        r#"
pub fn main() {
  echo case 1 {
    _ -> 2
  }
}
"#
    );
}

#[test]
pub fn echo_with_a_panic() {
    assert_js!(
        r#"
pub fn main() {
  echo panic
}
"#
    );
}

#[test]
pub fn echo_with_a_function_call() {
    assert_js!(
        r#"
pub fn main() {
  echo wibble(1, 2)
}

fn wibble(n: Int, m: Int) { n + m }
"#
    );
}

#[test]
pub fn echo_with_a_function_call_and_a_message() {
    assert_js!(
        r#"
pub fn main() {
  echo wibble(1, 2) as message()
}

fn wibble(n: Int, m: Int) { n + m }
fn message() { "Hello!" }
"#
    );
}

#[test]
pub fn echo_with_a_block() {
    assert_js!(
        r#"
pub fn main() {
  echo {
    Nil
    1
  }
}
"#
    );
}

#[test]
pub fn echo_in_a_pipeline() {
    assert_js!(
        r#"
pub fn main() {
  [1, 2, 3]
  |> echo
  |> wibble
}

pub fn wibble(n) { n }
"#
    )
}

#[test]
pub fn echo_in_a_pipeline_with_message() {
    assert_js!(
        r#"
pub fn main() {
  [1, 2, 3]
  |> echo as "message!!"
  |> wibble
}

pub fn wibble(n) { n }
"#
    )
}

#[test]
pub fn multiple_echos_in_a_pipeline() {
    assert_js!(
        r#"
pub fn main() {
  [1, 2, 3]
  |> echo
  |> wibble
  |> echo
  |> wibble
  |> echo
}

pub fn wibble(n) { n }
"#
    )
}

#[test]
pub fn module_named_inspect() {
    assert_js!(
        ("other", "other/inspect", "pub const x = Nil"),
        r#"
import other/inspect

pub fn main() {
  echo inspect.x
}
"#
    )
}
