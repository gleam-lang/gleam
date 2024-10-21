macro_rules! assert_generic_echo {
    ($src:expr $(,)?) => {{
        let output =
            $crate::javascript::tests::compile_js($src, vec![]).expect("compilation failed");
        let output = output
            .strip_suffix(&format!(
                "{}\n",
                std::include_str!("../../../templates/echo.mjs")
            ))
            .expect("contain generic echo code from `echo.mjs`");
        let output = format!("{output}// ...omitted code from `templates/echo.mjs`...");
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};
}

#[test]
pub fn echo_with_a_simple_expression() {
    assert_generic_echo!(
        r#"
pub fn main() {
  echo 1
}
"#
    );
}

#[test]
pub fn multiple_echos_inside_expression() {
    assert_generic_echo!(
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
    assert_generic_echo!(
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
    assert_generic_echo!(
        r#"
pub fn main() {
  echo panic
}
"#
    );
}

#[test]
pub fn echo_with_a_function_call() {
    assert_generic_echo!(
        r#"
pub fn main() {
  echo wibble(1, 2)
}

fn wibble(n: Int, m: Int) { n + m }
"#
    );
}

#[test]
pub fn echo_with_a_block() {
    assert_generic_echo!(
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
    assert_generic_echo!(
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
pub fn multiple_echos_in_a_pipeline() {
    assert_generic_echo!(
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
