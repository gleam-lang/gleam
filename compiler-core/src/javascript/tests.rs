mod assignments;
mod bit_strings;
mod blocks;
mod bools;
mod case;
mod case_clause_guards;
mod custom_types;
mod externals;
mod functions;
mod generics;
mod lists;
mod modules;
mod numbers;
mod prelude;
mod records;
mod recursion;
mod results;
mod strings;
mod todo;
mod try_;
mod tuples;
mod type_alias;
mod use_;

pub static CURRENT_PACKAGE: &str = "thepackage";

#[macro_export]
macro_rules! assert_js {
    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr $(,)?) => {{
        use std::path::Path;
        use $crate::{javascript::*, uid::UniqueIdGenerator};
        let mut modules = im::HashMap::new();
        let ids = UniqueIdGenerator::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), $crate::type_::build_prelude(&ids));
        let (mut ast, _) = $crate::parse::parse_module($dep_src).expect("dep syntax error");
        ast.name = $dep_name;
        let dep = $crate::type_::infer_module(
            $crate::build::Target::JavaScript,
            &ids,
            ast,
            $crate::build::Origin::Src,
            $dep_package,
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let _ = modules.insert($dep_name.join("/"), dep.type_info);
        let (mut ast, _) = $crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my".to_string(), "mod".to_string()];
        let ast = $crate::type_::infer_module(
            $crate::build::Target::JavaScript,
            &ids,
            ast,
            $crate::build::Origin::Src,
            CURRENT_PACKAGE,
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let line_numbers = LineNumbers::new($src);
        let output = module(&ast, &line_numbers, Path::new(""), "").unwrap();
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr, $js:expr $(,)?) => {{
        use std::path::Path;
        let mut modules = im::HashMap::new();
        let ids = UniqueIdGenerator::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), $crate::type_::build_prelude(&ids));
        let (mut ast, _) = $crate::parse::parse_module($dep_src).expect("dep syntax error");
        ast.name = $dep_name;
        let dep = $crate::type_::infer_module(
            $crate::build::Target::JavaScript,
            &ids,
            ast,
            $crate::build::Origin::Src,
            $dep_package,
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let _ = modules.insert($dep_name.join("/"), dep.type_info);
        let (mut ast, _) = $crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my".to_string(), "mod".to_string()];
        let ast = $crate::type_::infer_module(
            $crate::build::Target::JavaScript,
            &ids,
            ast,
            $crate::build::Origin::Src,
            CURRENT_PACKAGE,
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let mut output = String::new();
        let line_numbers = LineNumbers::new($src);
        module(&ast, &line_numbers, Path::new(""), "", &mut output).unwrap();
        assert_eq!(($src, output), ($src, $js.to_string()));
    }};

    ($src:expr $(,)?) => {{
        use std::path::Path;
        use $crate::{javascript::*, uid::UniqueIdGenerator};
        let mut modules = im::HashMap::new();
        let ids = UniqueIdGenerator::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), $crate::type_::build_prelude(&ids));

        let (mut ast, _) = $crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my".to_string(), "mod".to_string()];
        let ast = $crate::type_::infer_module(
            $crate::build::Target::JavaScript,
            &ids,
            ast,
            $crate::build::Origin::Src,
            "thepackage",
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let line_numbers = LineNumbers::new($src);
        let output = module(&ast, &line_numbers, Path::new(""), "").unwrap();
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    ($src:expr, $js:expr $(,)?) => {{
        use std::path::Path;
        use $crate::{javascript::*, uid::UniqueIdGenerator};
        let mut modules = im::HashMap::new();
        let ids = UniqueIdGenerator::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), crate::type_::build_prelude(&ids));

        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my".to_string(), "mod".to_string()];
        let ast = crate::type_::infer_module(
            crate::build::Target::JavaScript,
            &ids,
            ast,
            crate::build::Origin::Src,
            "thepackage",
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let line_numbers = LineNumbers::new($src);
        let output = module(&ast, &line_numbers, Path::new(""), "").unwrap();
        assert_eq!(($src, output), ($src, $js.to_string()));
    }};
}

#[macro_export]
macro_rules! assert_ts_def {
    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr $(,)?) => {{
        use std::path::Path;
        use $crate::{javascript::*, uid::UniqueIdGenerator};
        let mut modules = im::HashMap::new();
        let ids = UniqueIdGenerator::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), $crate::type_::build_prelude(&ids));
        let (mut ast, _) = $crate::parse::parse_module($dep_src).expect("dep syntax error");
        ast.name = $dep_name;
        let dep = $crate::type_::infer_module(
            $crate::build::Target::JavaScript,
            &ids,
            ast,
            $crate::build::Origin::Src,
            $dep_package,
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let _ = modules.insert($dep_name.join("/"), dep.type_info);
        let (mut ast, _) = $crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my".to_string(), "mod".to_string()];
        let ast = $crate::type_::infer_module(
            $crate::build::Target::JavaScript,
            &ids,
            ast,
            $crate::build::Origin::Src,
            CURRENT_PACKAGE,
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let output = ts_declaration(&ast, Path::new(""), "").unwrap();
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr, $js:expr $(,)?) => {{
        use std::path::Path;
        let mut modules = im::HashMap::new();
        let ids = UniqueIdGenerator::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), $crate::type_::build_prelude(&ids));
        let (mut ast, _) = $crate::parse::parse_module($dep_src).expect("dep syntax error");
        ast.name = $dep_name;
        let dep = $crate::type_::infer_module(
            $crate::build::Target::JavaScript,
            &ids,
            ast,
            $crate::build::Origin::Src,
            $dep_package,
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let _ = modules.insert($dep_name.join("/"), dep.type_info);
        let (mut ast, _) = $crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my".to_string(), "mod".to_string()];
        let ast = $crate::type_::infer_module(
            $crate::build::Target::JavaScript,
            &ids,
            ast,
            $crate::build::Origin::Src,
            CURRENT_PACKAGE,
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let mut output = String::new();
        ts_declaration(&ast, Path::new(""), "", &mut output).unwrap();
        assert_eq!(($src, output), ($src, $js.to_string()));
    }};

    ($src:expr $(,)?) => {{
        use std::path::Path;
        use $crate::{javascript::*, uid::UniqueIdGenerator};
        let mut modules = im::HashMap::new();
        let ids = UniqueIdGenerator::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), $crate::type_::build_prelude(&ids));

        let (mut ast, _) = $crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my".to_string(), "mod".to_string()];
        let ast = $crate::type_::infer_module(
            $crate::build::Target::JavaScript,
            &ids,
            ast,
            $crate::build::Origin::Src,
            "thepackage",
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let output = ts_declaration(&ast, Path::new(""), "").unwrap();
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    ($src:expr, $js:expr $(,)?) => {{
        use std::path::Path;
        use $crate::{javascript::*, uid::UniqueIdGenerator};
        let mut modules = im::HashMap::new();
        let ids = UniqueIdGenerator::new();
        // DUPE: preludeinsertion
        // TODO: Currently we do this here and also in the tests. It would be better
        // to have one place where we create all this required state for use in each
        // place.
        let _ = modules.insert("gleam".to_string(), crate::type_::build_prelude(&ids));

        let (mut ast, _) = crate::parse::parse_module($src).expect("syntax error");
        ast.name = vec!["my".to_string(), "mod".to_string()];
        let ast = crate::type_::infer_module(
            crate::build::Target::JavaScript,
            &ids,
            ast,
            crate::build::Origin::Src,
            "thepackage",
            &modules,
            &mut vec![],
        )
        .expect("should successfully infer");
        let mut output = String::new();
        ts_declaration(&ast, Path::new(""), "", &mut output).unwrap();
        assert_eq!(($src, output), ($src, $js.to_string()));
    }};
}
