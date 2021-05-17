mod assignments;
mod blocks;
mod bools;
mod custom_types;
mod functions;
mod lists;
mod modules;
mod numbers;
mod strings;
mod tuples;

#[macro_export]
macro_rules! assert_js {
    ($src:expr, $erl:expr $(,)?) => {{
        use crate::javascript::*;
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
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["the_app".to_string()];
        let ast = crate::type_::infer_module(&mut 0, ast, &modules, &mut vec![])
            .expect("should successfully infer");
        let mut output = String::new();
        let line_numbers = LineNumbers::new($src);
        module(&ast, &line_numbers, &mut output).unwrap();
        assert_eq!(($src, output), ($src, $erl.to_string()));
    }};

    (($dep_name:expr, $dep_src:expr), $src:expr, $erl:expr $(,)?) => {{
        use crate::javascript::*;
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
        let (mut ast, _) = crate::parse::parse_module($dep_src).expect("dep syntax error");
        ast.name = $dep_name;
        let dep = crate::type_::infer_module(&mut 0, ast, &modules, &mut vec![])
            .expect("should successfully infer");
        let _ = modules.insert(
            $dep_name.join("/"),
            (crate::build::Origin::Src, dep.type_info),
        );
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["the_app".to_string()];
        let ast = crate::type_::infer_module(&mut 0, ast, &modules, &mut vec![])
            .expect("should successfully infer");
        let mut output = String::new();
        let line_numbers = LineNumbers::new($src);
        module(&ast, &line_numbers, &mut output).unwrap();
        assert_eq!(($src, output), ($src, $erl.to_string()));
    }};
}

#[test]
fn equality() {
    assert_js!(
        r#"
fn go() {
  1 == 2
  1 != 2
}
"#,
        r#""use strict";

function go() {
  $equal(1, 2);
  return !$equal(1, 2);
}

function $equal(x, y) {
  if ($isObject(x) && $isObject(y)) {
    const kx = Object.keys(x);
    const ky = Object.keys(x);

    if (kx.length != ky.length) {
      return false;
    }

    for (const k of kx) {
      const a = x[k];
      const b = y[k];
      if !$equal(a, b) {
        return false
      }
    }

    return true;

  } else {
    return x === y;
  }
}

function $isObject(object) {
  return object != null && typeof object === 'object';
}
"#
    );
}

#[test]
fn todo_throws_error() {
    assert_js!(
        r#"
fn go() {
    todo
}
"#,
        r#""use strict";

function go() {
  throw Object.assign(
    new Error("This has not yet been implemented"),
    { gleam_error: "todo", module: "the_app", function: "go", line: 3 }
  )
}
"#
    );
    assert_js!(
        r#"
fn go() {
    todo("I should do this");
}
"#,
        r#""use strict";

function go() {
  throw Object.assign(
    new Error("I should do this"),
    { gleam_error: "todo", module: "the_app", function: "go", line: 3 }
  )
}
"#
    );
}
