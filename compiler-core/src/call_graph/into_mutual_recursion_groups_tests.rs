use std::collections::HashMap;

use camino::Utf8PathBuf;
use ecow::EcoString;
use itertools::Itertools;

use crate::{
    analyse::TargetSupport,
    build::{Origin, Target},
    config::PackageConfig,
    inline,
    line_numbers::LineNumbers,
    type_::PRELUDE_MODULE_NAME,
    uid::UniqueIdGenerator,
    warning::{TypeWarningEmitter, WarningEmitter},
};

fn parse_and_group_functions(src: &str) -> Vec<Vec<EcoString>> {
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

    let path = Utf8PathBuf::from("");
    let parsed = crate::parse::parse_module(path.clone(), src, &WarningEmitter::null())
        .expect("syntax error");

    let ast = crate::analyse::ModuleAnalyzerConstructor::<()> {
        target: Target::Erlang,
        ids: &ids,
        origin: Origin::Src,
        importable_modules: &modules,
        warnings: &TypeWarningEmitter::null(),
        direct_dependencies: &HashMap::new(),
        dev_dependencies: &std::collections::HashSet::new(),
        target_support: TargetSupport::NotEnforced,
        package_config: &PackageConfig::default(),
    }
    .infer_module(parsed.module, LineNumbers::new(src), path.clone())
    .expect("should successfully infer root Erlang");

    let inlined = inline::module(ast, &modules);

    inlined
        .definitions
        .functions
        .iter()
        .map(|group| {
            group
                .iter()
                .map(|function| {
                    let (_, name) = function.name.as_ref().expect("module function to be named");
                    name.clone()
                })
                .sorted()
                .collect_vec()
        })
        .sorted()
        .collect_vec()
}

#[test]
fn mutually_recursive_pair() {
    assert_eq!(
        vec![vec!["a", "b"]],
        parse_and_group_functions(
            "
pub fn a() { b() }
pub fn b() { a() }
"
        ),
    );
}

#[test]
fn mutually_recursive_pair_and_stray_function() {
    assert_eq!(
        vec![vec!["a", "b"], vec!["c"]],
        parse_and_group_functions(
            "
pub fn a() { b() }
pub fn b() { a() }
pub fn c() { Nil }
"
        ),
    );
}

#[test]
fn mutually_recursive_throuple() {
    assert_eq!(
        vec![vec!["a", "b", "c"]],
        parse_and_group_functions(
            "
pub fn a() { b() }
pub fn b() { c() }
pub fn c() { a() }
"
        ),
    );
}

#[test]
fn functions_calling_each_other_but_not_recursively() {
    assert_eq!(
        vec![vec!["a"], vec!["b"], vec!["c"]],
        parse_and_group_functions(
            "
pub fn a() { b() }
pub fn b() { c() }
pub fn c() { Nil }
"
        ),
    );
}

#[test]
fn functions_calling_each_other_but_not_mutually_recursive() {
    assert_eq!(
        vec![vec!["a"], vec!["b"], vec!["c"]],
        parse_and_group_functions(
            "
pub fn a() { b() }
pub fn b() { c() }
pub fn c() { c() }
"
        ),
    );
}

#[test]
fn mutually_recursive_functions_with_case() {
    assert_eq!(
        vec![vec!["a", "b"]],
        parse_and_group_functions(
            "
pub fn a() {
  case todo {
    1 -> Nil
    _ -> b()
  }
}

pub fn b() {
  case todo {
    1 -> { a() }
    _ -> b()
  }
}
"
        ),
    );
}

#[test]
fn functions_calling_each_other_but_not_in_tail_position() {
    assert_eq!(
        vec![vec!["a"], vec!["b"]],
        parse_and_group_functions(
            "
pub fn a() {
    b()
    a()
}

pub fn b() {
  a()
  1
}
"
        ),
    );
}

#[test]
fn the_bigger_cicle_is_always_favoured() {
    assert_eq!(
        vec![vec!["a", "b", "c"]],
        parse_and_group_functions(
            "
// Here we have two cycles: a -> b -> c -> a
// But also: b -> c -> b
// We want to make sure we always pick the cycle including the most
// functions. So here we're expecting to see a single group with all
// the three function.

pub fn a() { b() }
pub fn b() { c() }
pub fn c() {
  case todo {
    1 -> a()
    _ -> b()
  }
}
"
        ),
    );
}

#[test]
fn two_groups_of_mutually_recursive_functions() {
    assert_eq!(
        vec![vec!["a", "b"], vec!["c", "d"]],
        parse_and_group_functions(
            "
pub fn a() { b() }
pub fn b() { a() }

pub fn c() { d() }
pub fn d() {
  case todo {
    1 -> a()
    1 -> d()
    _ -> c()
  }
}
"
        ),
    );
}
