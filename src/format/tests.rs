use super::*;

#[test]
fn module_test() {
    macro_rules! assert_format {
        ($src:expr $(,)?) => {
            let src = $src.to_string();
            let stripped = crate::parser::strip_extra(src.as_ref());
            let ast = crate::grammar::ModuleParser::new()
                .parse(&stripped)
                .unwrap();
            assert_eq!(src, pretty_module(&ast));
        };
    }

    // Imports
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

    // External types
    assert_format!("external type Private\n");
    assert_format!("external type Box(a)\n");
    assert_format!("external type Box(a, b, zero)\n");
    assert_format!("pub external type Private\n");
    assert_format!("pub external type Box(a)\n");
    assert_format!("pub external type Box(a, b, zero)\n");

    // External fn
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
}

// pub type RoseTree(a) {
//   Node(val: a, children: List(RoseTree(a)))
//   Leaf(val: a)
// }

// type Option(a) = Result(a, Nil)

// pub external type Opaque

// pub external fn random_float() -> Float = \"rand\" \"uniform\"

// fn fully_typed(first: Int) -> Int {
//     first + 1
// }

// fn id(x: a, y: b) {
//     x
// }

// pub fn x() {
//     id(1.0, 1)
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
