mod numbers;
mod strings;

#[macro_export]
macro_rules! assert_js {
    ($src:expr, $erl:expr $(,)?) => {{
        use crate::javascript::*;
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["the_app".to_string()];
        let mut modules = std::collections::HashMap::new();
        let mut uid = 0;
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert(
            "gleam".to_string(),
            (
                crate::build::Origin::Src,
                crate::type_::build_prelude(&mut uid),
            ),
        );
        let ast = crate::type_::infer_module(&mut 0, ast, &modules, &mut vec![])
            .expect("should successfully infer");
        let mut output = String::new();
        let line_numbers = LineNumbers::new($src);
        module(&ast, &line_numbers, &mut output).unwrap();
        assert_eq!(($src, output), ($src, $erl.to_string()));
    }};
}

#[test]
fn sequences() {
    assert_js!(
        r#"
fn go() {
  "one"
  "two"
  "three"
}
"#,
        r#""use strict";

function go() {
  "one";
  "two";
  return "three";
}
"#
    );
}

#[test]
fn tuple() {
    assert_js!(
        r#"
fn go() {
  #("1", "2", "3")
}
"#,
        r#""use strict";

function go() {
  return ["1", "2", "3"];
}
"#
    );

    assert_js!(
        r#"
fn go() {
  #(
    "1111111111111111111111111111111",
    #("1111111111111111111111111111111", "2", "3"),
    "3",
  )
}
"#,
        r#""use strict";

function go() {
  return [
    "1111111111111111111111111111111",
    ["1111111111111111111111111111111", "2", "3"],
    "3",
  ];
}
"#
    );
}

#[test]
fn tuple_with_block_element() {
    assert_js!(
        r#"
fn go() {
  #(
    "1", 
    {
      "2"
      "3"
    },
  )
}
"#,
        r#""use strict";

function go() {
  return [
    "1",
    (() => {
      "2";
      return "3";
    })(),
  ];
}
"#
    );

    assert_js!(
        r#"
fn go() {
  #(
    "1111111111111111111111111111111",
    #("1111111111111111111111111111111", "2", "3"),
    "3",
  )
}
"#,
        r#""use strict";

function go() {
  return [
    "1111111111111111111111111111111",
    ["1111111111111111111111111111111", "2", "3"],
    "3",
  ];
}
"#
    );
}

#[test]
fn constant_tuples() {
    assert_js!(
        r#"
const a = "Hello"
const b = 1;
const c = 2.0;
const e = #("bob", "dug")
        "#,
        r#""use strict";

const a = "Hello";

const b = 1;

const c = 2.0;

const e = ["bob", "dug"];
"#
    );

    assert_js!(
        r#"
const e = #(
  "loooooooooooooong", "loooooooooooong", "loooooooooooooong",
  "loooooooooooooong", "loooooooooooong", "loooooooooooooong",
)
        "#,
        r#""use strict";

const e = [
  "loooooooooooooong",
  "loooooooooooong",
  "loooooooooooooong",
  "loooooooooooooong",
  "loooooooooooong",
  "loooooooooooooong",
];
"#
    );
}
