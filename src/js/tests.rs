mod strings;

#[macro_export]
macro_rules! assert_js {
    ($src:expr, $erl:expr $(,)?) => {
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["the_app".to_string()];
        let modules = std::collections::HashMap::new();
        let ast = crate::type_::infer_module(&mut 0, ast, &modules, &mut vec![])
            .expect("should successfully infer");
        let mut output = String::new();
        let line_numbers = LineNumbers::new($src);
        module(&ast, &line_numbers, &mut output).unwrap();
        assert_eq!(($src, output), ($src, $erl.to_string()));
    };
}
