mod assignments;
mod bit_strings;
mod blocks;
mod bools;
mod case;
mod case_clause_guards;
mod custom_types;
mod externals;
mod functions;
mod lists;
mod modules;
mod numbers;
mod prelude;
mod strings;
mod todo;
mod try_;
mod tuples;

pub static CURRENT_PACKAGE: &str = "thepackage";

#[macro_export]
macro_rules! assert_js {
    ($src:expr $(,)?) => {{
        use crate::javascript::*;
        use std::path::Path;
        let mut modules = std::collections::HashMap::new();
        let mut uid = 0;
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), crate::type_::build_prelude(&mut uid));

        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my".to_string(), "mod".to_string()];
        let ast = crate::type_::infer_module(
            crate::build::Target::JavaScript,
            &mut 0,
            ast,
            crate::build::Origin::Src,
            "thepackage",
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let mut output = String::new();
        let line_numbers = LineNumbers::new($src);
        module(&ast, &line_numbers, Path::new(""), "", &mut output).unwrap();
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    ($src:expr, $erl:expr $(,)?) => {{
        use crate::javascript::*;
        use std::path::Path;
        let mut modules = std::collections::HashMap::new();
        let mut uid = 0;
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), crate::type_::build_prelude(&mut uid));

        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my".to_string(), "mod".to_string()];
        let ast = crate::type_::infer_module(
            crate::build::Target::JavaScript,
            &mut 0,
            ast,
            crate::build::Origin::Src,
            "thepackage",
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let mut output = String::new();
        let line_numbers = LineNumbers::new($src);
        module(&ast, &line_numbers, Path::new(""), "", &mut output).unwrap();
        assert_eq!(($src, output), ($src, $erl.to_string()));
    }};

    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr, $erl:expr $(,)?) => {{
        use crate::javascript::*;
        use std::path::Path;
        let mut modules = std::collections::HashMap::new();
        let mut uid = 0;
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), crate::type_::build_prelude(&mut uid));
        let (mut ast, _) = crate::parse::parse_module($dep_src).expect("dep syntax error");
        ast.name = $dep_name;
        let dep = crate::type_::infer_module(
            crate::build::Target::JavaScript,
            &mut 0,
            ast,
            crate::build::Origin::Src,
            $dep_package,
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let _ = modules.insert($dep_name.join("/"), dep.type_info);
        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my".to_string(), "mod".to_string()];
        let ast = crate::type_::infer_module(
            crate::build::Target::JavaScript,
            &mut 0,
            ast,
            crate::build::Origin::Src,
            CURRENT_PACKAGE,
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let mut output = String::new();
        let line_numbers = LineNumbers::new($src);
        module(&ast, &line_numbers, Path::new(""), "", &mut output).unwrap();
        assert_eq!(($src, output), ($src, $erl.to_string()));
    }};
}
