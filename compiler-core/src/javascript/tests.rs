use crate::{
    analyse::TargetSupport,
    build::{Origin, Target},
    config::PackageConfig,
    javascript::*,
    uid::UniqueIdGenerator,
    warning::TypeWarningEmitter,
};
use camino::Utf8Path;

mod assignments;
mod bit_arrays;
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
macro_rules! assert_js_with_multiple_imports {
    ($(($name:literal, $module_src:literal)),+; $src:literal) => {
        let output =
            $crate::javascript::tests::compile_js($src, vec![$((CURRENT_PACKAGE, $name, $module_src)),*]);
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    };
}

#[macro_export]
macro_rules! assert_js {
    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr $(,)?) => {{
        let output =
            $crate::javascript::tests::compile_js($src, vec![($dep_package, $dep_name, $dep_src)]);
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr, $js:expr $(,)?) => {{
        let output =
            $crate::javascript::tests::compile_js($src, Some(($dep_package, $dep_name, $dep_src)));
        assert_eq!(($src, output), ($src, $js.to_string()));
    }};

    ($src:expr $(,)?) => {{
        let output = $crate::javascript::tests::compile_js($src, vec![]);
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    ($src:expr, $js:expr $(,)?) => {{
        let output = $crate::javascript::tests::compile_js($src, vec![]);
        assert_eq!(($src, output), ($src, $js.to_string()));
    }};
}

#[macro_export]
macro_rules! assert_ts_def {
    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr $(,)?) => {{
        let output =
            $crate::javascript::tests::compile_ts($src, vec![($dep_package, $dep_name, $dep_src)]);
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    ($src:expr $(,)?) => {{
        let output = $crate::javascript::tests::compile_ts($src, vec![]);
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};
}

pub fn compile(src: &str, deps: Vec<(&str, &str, &str)>) -> TypedModule {
    let mut modules = im::HashMap::new();
    let ids = UniqueIdGenerator::new();
    // DUPE: preludeinsertion
    // TODO: Currently we do this here and also in the tests. It would be better
    // to have one place where we create all this required state for use in each
    // place.
    let _ = modules.insert(
        PRELUDE_MODULE_NAME.into(),
        crate::type_::build_prelude(&ids),
    );
    let mut direct_dependencies = std::collections::HashMap::from_iter(vec![]);

    deps.iter().for_each(|(dep_package, dep_name, dep_src)| {
        let mut dep_config = PackageConfig::default();
        dep_config.name = (*dep_package).into();
        let parsed = crate::parse::parse_module(dep_src).expect("dep syntax error");
        let mut ast = parsed.module;
        ast.name = (*dep_name).into();
        let line_numbers = LineNumbers::new(dep_src);

        let dep = crate::analyse::ModuleAnalyzer::<()> {
            target: Target::JavaScript,
            ids: &ids,
            origin: Origin::Src,
            importable_modules: &modules,
            warnings: &TypeWarningEmitter::null(),
            direct_dependencies: &std::collections::HashMap::new(),
            target_support: TargetSupport::Enforced,
            package_config: &dep_config,
        }
        .infer_module(ast, line_numbers, "".into())
        .expect("should successfully infer");
        let _ = modules.insert((*dep_name).into(), dep.type_info);
        let _ = direct_dependencies.insert((*dep_package).into(), ());
    });

    let parsed = crate::parse::parse_module(src).expect("syntax error");
    let mut ast = parsed.module;
    ast.name = "my/mod".into();
    let line_numbers = LineNumbers::new(src);
    let mut config = PackageConfig::default();
    config.name = "thepackage".into();

    crate::analyse::ModuleAnalyzer::<()> {
        target: Target::JavaScript,
        ids: &ids,
        origin: Origin::Src,
        importable_modules: &modules,
        warnings: &TypeWarningEmitter::null(),
        direct_dependencies: &direct_dependencies,
        target_support: TargetSupport::NotEnforced,
        package_config: &config,
    }
    .infer_module(ast, line_numbers, "".into())
    .expect("should successfully infer")
}

pub fn compile_js(src: &str, deps: Vec<(&str, &str, &str)>) -> String {
    let ast = compile(src, deps);
    let line_numbers = LineNumbers::new(src);
    module(
        &ast,
        &line_numbers,
        Utf8Path::new(""),
        &"".into(),
        TargetSupport::NotEnforced,
        TypeScriptDeclarations::None,
    )
    .unwrap()
}

pub fn compile_ts(src: &str, deps: Vec<(&str, &str, &str)>) -> String {
    let ast = compile(src, deps);
    ts_declaration(&ast, Utf8Path::new(""), &src.into()).unwrap()
}
