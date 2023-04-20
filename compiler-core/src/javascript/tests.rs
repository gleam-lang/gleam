use crate::{
    build::{Origin, Target},
    javascript::*,
    uid::UniqueIdGenerator,
    warning::TypeWarningEmitter,
};
use std::path::Path;

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
mod panic;
mod prelude;
mod records;
mod recursion;
mod results;
mod strings;
mod todo;
mod tuples;
mod type_alias;
mod use_;

pub static CURRENT_PACKAGE: &str = "thepackage";

#[macro_export]
macro_rules! assert_js {
    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr $(,)?) => {{
        let output =
            $crate::javascript::tests::compile_js($src, Some(($dep_package, $dep_name, $dep_src)));
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr, $js:expr $(,)?) => {{
        let output =
            $crate::javascript::tests::compile_js($src, Some(($dep_package, $dep_name, $dep_src)));
        assert_eq!(($src, output), ($src, $js.to_string()));
    }};

    ($src:expr $(,)?) => {{
        let output = $crate::javascript::tests::compile_js($src, None);
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    ($src:expr, $js:expr $(,)?) => {{
        let output = $crate::javascript::tests::compile_js($src, None);
        assert_eq!(($src, output), ($src, $js.to_string()));
    }};
}

#[macro_export]
macro_rules! assert_ts_def {
    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr $(,)?) => {{
        let output =
            $crate::javascript::tests::compile_ts($src, Some(($dep_package, $dep_name, $dep_src)));
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    ($src:expr $(,)?) => {{
        let output = $crate::javascript::tests::compile_ts($src, None);
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};
}

pub fn compile(src: &str, dep: Option<(&str, &str, &str)>) -> TypedModule {
    let mut modules = im::HashMap::new();
    let ids = UniqueIdGenerator::new();
    // DUPE: preludeinsertion
    // TODO: Currently we do this here and also in the tests. It would be better
    // to have one place where we create all this required state for use in each
    // place.
    let _ = modules.insert("gleam".into(), crate::type_::build_prelude(&ids));

    if let Some((dep_package, dep_name, dep_src)) = dep {
        let (mut ast, _) = crate::parse::parse_module(dep_src).expect("dep syntax error");
        ast.name = dep_name.into();
        let dep = crate::analyse::infer_module(
            Target::JavaScript,
            &ids,
            ast,
            Origin::Src,
            &dep_package.into(),
            &modules,
            &TypeWarningEmitter::null(),
        )
        .expect("should successfully infer");
        let _ = modules.insert(dep_name.into(), dep.type_info);
    }

    let (mut ast, _) = crate::parse::parse_module(src).expect("syntax error");
    ast.name = "my/mod".into();
    crate::analyse::infer_module(
        Target::JavaScript,
        &ids,
        ast,
        Origin::Src,
        &"thepackage".into(),
        &modules,
        &TypeWarningEmitter::null(),
    )
    .expect("should successfully infer")
}

pub fn compile_js(src: &str, dep: Option<(&str, &str, &str)>) -> String {
    let ast = compile(src, dep);
    let line_numbers = LineNumbers::new(src);
    module(&ast, &line_numbers, Path::new(""), &"".into()).unwrap()
}

pub fn compile_ts(src: &str, dep: Option<(&str, &str, &str)>) -> String {
    let ast = compile(src, dep);
    ts_declaration(&ast, Path::new(""), &src.into()).unwrap()
}
