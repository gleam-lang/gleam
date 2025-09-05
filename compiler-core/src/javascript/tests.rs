use crate::{
    analyse::TargetSupport,
    build::{Origin, Target},
    config::PackageConfig,
    inline,
    javascript::*,
    uid::UniqueIdGenerator,
    warning::{TypeWarningEmitter, WarningEmitter},
};
use camino::{Utf8Path, Utf8PathBuf};

mod assert;
mod assignments;
mod bit_arrays;
mod blocks;
mod bools;
mod case;
mod case_clause_guards;
mod consts;
mod custom_types;
mod echo;
mod externals;
mod functions;
mod generics;
mod inlining;
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
    ($(($name:literal, $module_src:literal)),+, $src:literal $(,)?) => {
        let compiled =
            $crate::javascript::tests::compile_js($src, vec![$(($crate::javascript::tests::CURRENT_PACKAGE, $name, $module_src)),*]);
            let mut output = String::from("----- SOURCE CODE\n");
            for (name, src) in [$(($name, $module_src)),*] {
                output.push_str(&format!("-- {name}.gleam\n{src}\n\n"));
            }
            output.push_str(&format!("-- main.gleam\n{}\n\n----- COMPILED JAVASCRIPT\n{compiled}", $src));
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    };

    ($(($dep_package:expr, $dep_name:expr, $dep_src:expr)),+, $src:literal $(,)?) => {{
        let compiled =
            $crate::javascript::tests::compile_js($src, vec![$(($dep_package, $dep_name, $dep_src)),*]);
        let output = format!(
            "----- SOURCE CODE\n{}\n\n----- COMPILED JAVASCRIPT\n{}",
            $src, compiled
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr, $js:expr $(,)?) => {{
        let output =
            $crate::javascript::tests::compile_js($src, Some(($dep_package, $dep_name, $dep_src)));
        assert_eq!(($src, output), ($src, $js.to_string()));
    }};

    ($src:expr $(,)?) => {{
        let compiled =
            $crate::javascript::tests::compile_js($src, vec![]);
        let output = format!(
            "----- SOURCE CODE\n{}\n\n----- COMPILED JAVASCRIPT\n{}",
            $src, compiled
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    ($src:expr, $js:expr $(,)?) => {{
        let output =
            $crate::javascript::tests::compile_js($src, vec![]);
        assert_eq!(($src, output), ($src, $js.to_string()));
    }};
}

#[macro_export]
macro_rules! assert_ts_def {
    (($dep_1_package:expr, $dep_1_name:expr, $dep_1_src:expr), ($dep_2_package:expr, $dep_2_name:expr, $dep_2_src:expr), $src:expr $(,)?) => {{
        let compiled = $crate::javascript::tests::compile_ts(
            $src,
            vec![
                ($dep_1_package, $dep_1_name, $dep_1_src),
                ($dep_2_package, $dep_2_name, $dep_2_src),
            ],
        );
        let output = format!(
            "----- SOURCE CODE\n{}\n\n----- TYPESCRIPT DEFINITIONS\n{}",
            $src, compiled
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    (($dep_package:expr, $dep_name:expr, $dep_src:expr), $src:expr $(,)?) => {{
        let compiled =
            $crate::javascript::tests::compile_ts($src, vec![($dep_package, $dep_name, $dep_src)]);
        let output = format!(
            "----- SOURCE CODE\n{}\n\n----- TYPESCRIPT DEFINITIONS\n{}",
            $src, compiled
        );
        insta::assert_snapshot!(insta::internals::AutoName, output, $src);
    }};

    ($src:expr $(,)?) => {{
        let compiled = $crate::javascript::tests::compile_ts($src, vec![]);
        let output = format!(
            "----- SOURCE CODE\n{}\n\n----- TYPESCRIPT DEFINITIONS\n{}",
            $src, compiled
        );
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
    let mut direct_dependencies = HashMap::from_iter(vec![]);

    deps.iter().for_each(|(dep_package, dep_name, dep_src)| {
        let mut dep_config = PackageConfig::default();
        dep_config.name = (*dep_package).into();
        let parsed = crate::parse::parse_module(
            Utf8PathBuf::from("test/path"),
            dep_src,
            &WarningEmitter::null(),
        )
        .expect("dep syntax error");
        let mut ast = parsed.module;
        ast.name = (*dep_name).into();
        let line_numbers = LineNumbers::new(dep_src);

        let dep = crate::analyse::ModuleAnalyzerConstructor::<()> {
            target: Target::JavaScript,
            ids: &ids,
            origin: Origin::Src,
            importable_modules: &modules,
            warnings: &TypeWarningEmitter::null(),
            direct_dependencies: &HashMap::new(),
            dev_dependencies: &std::collections::HashSet::new(),
            target_support: TargetSupport::Enforced,
            package_config: &dep_config,
        }
        .infer_module(ast, line_numbers, "".into())
        .expect("should successfully infer");
        let _ = modules.insert((*dep_name).into(), dep.type_info);
        let _ = direct_dependencies.insert((*dep_package).into(), ());
    });

    let parsed =
        crate::parse::parse_module(Utf8PathBuf::from("test/path"), src, &WarningEmitter::null())
            .expect("syntax error");
    let mut ast = parsed.module;
    ast.name = "my/mod".into();
    let line_numbers = LineNumbers::new(src);
    let mut config = PackageConfig::default();
    config.name = "thepackage".into();

    let module = crate::analyse::ModuleAnalyzerConstructor::<()> {
        target: Target::JavaScript,
        ids: &ids,
        origin: Origin::Src,
        importable_modules: &modules,
        warnings: &TypeWarningEmitter::null(),
        direct_dependencies: &direct_dependencies,
        dev_dependencies: &std::collections::HashSet::new(),
        target_support: TargetSupport::NotEnforced,
        package_config: &config,
    }
    .infer_module(ast, line_numbers, "src/module.gleam".into())
    .expect("should successfully infer");

    inline::module(module, &modules)
}

pub fn compile_js(src: &str, deps: Vec<(&str, &str, &str)>) -> String {
    let ast = compile(src, deps);
    let line_numbers = LineNumbers::new(src);
    let stdlib_package = StdlibPackage::Present;
    let output = module(ModuleConfig {
        module: &ast,
        line_numbers: &line_numbers,
        src: &"".into(),
        typescript: TypeScriptDeclarations::None,
        stdlib_package,
        path: Utf8Path::new("src/module.gleam"),
        project_root: "project/root".into(),
    });

    output.replace(
        std::include_str!("../../templates/echo.mjs"),
        "// ...omitted code from `templates/echo.mjs`...",
    )
}

pub fn compile_ts(src: &str, deps: Vec<(&str, &str, &str)>) -> String {
    let ast = compile(src, deps);
    ts_declaration(&ast)
}
