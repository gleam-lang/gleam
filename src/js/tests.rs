use super::*;

macro_rules! assert_js {
    ($src:expr, $erl:expr $(,)?) => {
        // println!("\n\n\n{}\n", $src);
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["the_app".to_string()];
        let ast =
            crate::type_::infer_module(&mut 0, ast, &std::collections::HashMap::new(), &mut vec![])
                .expect("should successfully infer");
        let mut output = String::new();
        let line_numbers = LineNumbers::new($src);
        module(&ast, &line_numbers, &mut output).unwrap();
        assert_eq!(($src, output), ($src, $erl.to_string()));
    };
}

#[test]
fn variable_rewrite() {
    assert_js!(
        r#"
fn go(a) {
  case a {
    99 -> {
      let a = a
      1
    }
    _ -> a
  }
}

                    "#,
        r#"-module(the_app).
-compile(no_auto_import).

-spec go(integer()) -> integer().
go(A) ->
    case A of
        99 ->
            A@1 = A,
            1;

        _ ->
            A
    end.
"#,
    );
}