use std::{cell::RefCell, collections::HashMap, sync::Arc};

use camino::Utf8PathBuf;
use hexpm::version::Version;

use crate::{
    manifest::{Base16Checksum, Manifest, ManifestPackage, ManifestPackageSource},
    type_::{
        prelude::{bool, int, tuple},
        pretty::Printer,
        Type,
    }
};

use super::{Publicity, TypeVar};

fn print(type_: Arc<Type>) -> String {
    Printer::new().pretty_print(&type_, 0)
}

fn custom_bool() -> Arc<Type> {
    named("wibble", "one/two", "Bool", Publicity::Public, vec![])
}

fn fn_(args: Vec<Arc<Type>>, retrn: Arc<Type>) -> Arc<Type> {
    Arc::new(Type::Fn { retrn, args })
}

fn named(
    package: &str,
    module: &str,
    name: &str,
    publicity: Publicity,
    args: Vec<Arc<Type>>,
) -> Arc<Type> {
    Arc::new(Type::Named {
        publicity,
        package: package.into(),
        module: module.into(),
        name: name.into(),
        args,
    })
}

fn unbound(id: u64) -> Arc<Type> {
    Arc::new(Type::Var {
        type_: Arc::new(RefCell::new(TypeVar::Unbound { id }))
    })
}

fn generic(id: u64) -> Arc<Type> {
    Arc::new(Type::Var {
        type_: Arc::new(RefCell::new(TypeVar::Generic { id }))
    })
}

fn doc_print(type_: Arc<Type>) -> String {
    let manifest = Manifest {
        // hashmap is not used
        requirements: HashMap::new(),
        packages: vec![
            ManifestPackage {
                name: "onhex".into(),
                version: Version::new(1, 2, 3),
                build_tools: vec!["gleam".into()],
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Hex { outer_checksum: Base16Checksum(vec![]) } 
            },
            ManifestPackage {
                name: "local".into(),
                version: Version::new(0, 0, 1),
                build_tools: vec!["gleam".into()],
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Local {
                    path: Utf8PathBuf::from("../local")
                }
            },
            ManifestPackage {
                name: "git".into(),
                version: Version::new(1, 0, 0),
                build_tools: vec!["gleam".into()],
                otp_app: None,
                requirements: vec![],
                source: ManifestPackageSource::Git {
                    repo: "repo".into(),
                    commit: "commit".into(),
                }
            }
        ]
    };

    let mut printer = Printer::new();
    printer.with_context("mypkg", "mymod");
    printer.with_manifest(&manifest);

    printer.doc_pretty_print(&type_, 0)
}

#[test]
fn repeated_prelude_type() {
    insta::assert_snapshot!(print(tuple(vec![int(), int(), int()])));
}

#[test]
fn prelude_type_clash_prelude_first() {
    insta::assert_snapshot!(print(tuple(vec![bool(), custom_bool()])));
}

#[test]
fn prelude_type_clash_custom_first() {
    insta::assert_snapshot!(print(tuple(vec![custom_bool(), bool()])));
}

#[test]
fn doc_prelude_type() {
    insta::assert_snapshot!(doc_print(tuple(vec![int(), int(), int()])));
}

#[test]
fn doc_same_module_type() {
    insta::assert_snapshot!(doc_print(named("mypkg", "mymod", "Int", Publicity::Public, vec![])))
}

#[test]
fn doc_same_package_type() {
    insta::assert_snapshot!(doc_print(named("mypkg", "wibble/wobble", "Int", Publicity::Public, vec![])))
}

#[test]
fn doc_unknown_package() {
    insta::assert_snapshot!(doc_print(custom_bool()));
}

#[test]
fn doc_hex_package() {
    insta::assert_snapshot!(doc_print(named("onhex", "wibble/wobble", "Whatever", Publicity::Public, vec![])))
}

#[test]
fn doc_private_type() {
    insta::assert_snapshot!(doc_print(named("onhex", "wibble/wobble", "Whatever", Publicity::Private, vec![])))
}

#[test]
fn doc_local_package() {
    insta::assert_snapshot!(doc_print(named("local", "wibble/wobble", "Whatever", Publicity::Public, vec![])))
}

#[test]
fn doc_git_package() {
    insta::assert_snapshot!(doc_print(named("git", "wibble/wobble", "Whatever", Publicity::Public, vec![])))
}

#[test]
fn doc_parameterised_type() {
    insta::assert_snapshot!(doc_print(named("onhex", "wibble/wobble", "Whatever", Publicity::Public, vec![
        named("onhex", "something/else/entirely", "HttpFactorProxyBean", Publicity::Public, vec![]),
        custom_bool(),
        named("mypkg", "wibble/wabble", "Helper", Publicity::Public, vec![int(), int()]),
    ])))
}

#[test]
fn doc_fn_type() {
    insta::assert_snapshot!(doc_print(fn_(vec![
        int(),
        named("onhex", "something/else/entirely", "HttpFactorProxyBean", Publicity::Public, vec![]),
        named("mypkg", "wibble/wabble", "Helper", Publicity::Public, vec![int(), int()]),        
    ], custom_bool())))
}

#[test]
fn doc_generics() {
    insta::assert_snapshot!(doc_print(tuple(vec![
        fn_(vec![unbound(78), unbound(2)], unbound(2)),
        fn_(vec![generic(79), generic(2231)], generic(2)),
    ])))
}
