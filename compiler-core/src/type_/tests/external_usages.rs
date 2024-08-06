use crate::{ast::SrcSpan, type_::tests::compile_module};

#[test]
fn external_unqualified_function_usages() {
    let module = compile_module(
        "test_module",
        "
import test_module/test_module_inner.{wibble}

pub fn main() {
  wibble()
}
",
        None,
        vec![(
            "thepackage",
            "test_module/test_module_inner",
            "pub fn wibble() { 1 }",
        )],
    )
    .unwrap();
    assert_eq!(
        module.type_info.external_value_usages,
        [(
            "test_module/test_module_inner".into(),
            [(
                "wibble".into(),
                [
                    SrcSpan { start: 39, end: 45 },
                    SrcSpan { start: 66, end: 72 }
                ]
                .into()
            )]
            .into()
        )]
        .into()
    );
}

#[test]
fn external_qualified_function_usages() {
    let module = compile_module(
        "test_module",
        "
import test_module/test_module_inner

pub fn main() {
  test_module_inner.wibble()
}
",
        None,
        vec![(
            "thepackage",
            "test_module/test_module_inner",
            "pub fn wibble() { 1 }",
        )],
    )
    .unwrap();
    assert_eq!(
        module.type_info.external_value_usages,
        [(
            "test_module/test_module_inner".into(),
            [("wibble".into(), [SrcSpan { start: 74, end: 81 }].into())].into()
        )]
        .into()
    );
}

#[test]
fn external_unqualified_constant_usages() {
    let module = compile_module(
        "test_module",
        "
import test_module/test_module_inner.{wibble}

pub fn main() {
  wibble
}
",
        None,
        vec![(
            "thepackage",
            "test_module/test_module_inner",
            "pub const wibble = 1",
        )],
    )
    .unwrap();
    assert_eq!(
        module.type_info.external_value_usages,
        [(
            "test_module/test_module_inner".into(),
            [(
                "wibble".into(),
                [
                    SrcSpan { start: 39, end: 45 },
                    SrcSpan { start: 66, end: 72 }
                ]
                .into()
            )]
            .into()
        )]
        .into()
    );
}

#[test]
fn external_qualified_constant_usages() {
    let module = compile_module(
        "test_module",
        "
import test_module/test_module_inner

pub fn main() {
  test_module_inner.wibble
}
",
        None,
        vec![(
            "thepackage",
            "test_module/test_module_inner",
            "pub const wibble = 1",
        )],
    )
    .unwrap();
    assert_eq!(
        module.type_info.external_value_usages,
        [(
            "test_module/test_module_inner".into(),
            [("wibble".into(), [SrcSpan { start: 74, end: 81 }].into())].into()
        )]
        .into()
    );
}

#[test]
fn external_unqualified_constructor_usages() {
    let module = compile_module(
        "test_module",
        "
import test_module/test_module_inner.{Wibble, Wobble}

pub fn main() {
  Wibble(1)
  Wobble(\"wibble\")
}
",
        None,
        vec![(
            "thepackage",
            "test_module/test_module_inner",
            "pub type Wibble { Wibble(Int) Wobble(String) }",
        )],
    )
    .unwrap();
    assert_eq!(
        module.type_info.external_value_usages,
        [(
            "test_module/test_module_inner".into(),
            [
                (
                    "Wibble".into(),
                    [
                        SrcSpan { start: 39, end: 45 },
                        SrcSpan { start: 74, end: 80 }
                    ]
                    .into()
                ),
                (
                    "Wobble".into(),
                    [
                        SrcSpan { start: 47, end: 53 },
                        SrcSpan { start: 86, end: 92 }
                    ]
                    .into()
                )
            ]
            .into()
        )]
        .into()
    );
}

#[test]
fn external_qualified_constructor_usages() {
    let module = compile_module(
        "test_module",
        "
import test_module/test_module_inner

pub fn main() {
  test_module_inner.Wibble(1)
  test_module_inner.Wobble(\"wibble\")
}
",
        None,
        vec![(
            "thepackage",
            "test_module/test_module_inner",
            "pub type Wibble { Wibble(Int) Wobble(String) }",
        )],
    )
    .unwrap();
    assert_eq!(
        module.type_info.external_value_usages,
        [(
            "test_module/test_module_inner".into(),
            [
                ("Wibble".into(), [SrcSpan { start: 74, end: 81 }].into()),
                (
                    "Wobble".into(),
                    [SrcSpan {
                        start: 104,
                        end: 111
                    }]
                    .into()
                )
            ]
            .into()
        )]
        .into()
    );
}

#[test]
fn external_unqualified_pattern_usages() {
    let module = compile_module(
        "test_module",
        "
import test_module/test_module_inner.{Wibble, Wobble}

pub fn main() {
  let wibble = Wibble(1)
  case wibble {
    Wibble(x) -> x + 1
    Wobble(x) -> 2
  }
}
",
        None,
        vec![(
            "thepackage",
            "test_module/test_module_inner",
            "pub type Wibble { Wibble(Int) Wobble(String) }",
        )],
    )
    .unwrap();
    assert_eq!(
        module.type_info.external_value_usages,
        [(
            "test_module/test_module_inner".into(),
            [
                (
                    "Wibble".into(),
                    [
                        SrcSpan { start: 39, end: 45 },
                        SrcSpan { start: 87, end: 93 },
                        SrcSpan {
                            start: 117,
                            end: 126
                        }
                    ]
                    .into()
                ),
                (
                    "Wobble".into(),
                    [
                        SrcSpan { start: 47, end: 53 },
                        SrcSpan {
                            start: 140,
                            end: 149
                        }
                    ]
                    .into()
                )
            ]
            .into()
        )]
        .into()
    );
}

#[test]
fn external_qualified_pattern_usages() {
    let module = compile_module(
        "test_module",
        "
import test_module/test_module_inner

pub fn main() {
  let wibble = test_module_inner.Wibble(1)
  case wibble {
    test_module_inner.Wibble(x) -> x + 1
    test_module_inner.Wobble(x) -> 2
  }
}
",
        None,
        vec![(
            "thepackage",
            "test_module/test_module_inner",
            "pub type Wibble { Wibble(Int) Wobble(String) }",
        )],
    )
    .unwrap();
    assert_eq!(
        module.type_info.external_value_usages,
        [(
            "test_module/test_module_inner".into(),
            [
                (
                    "Wibble".into(),
                    [
                        SrcSpan { start: 87, end: 94 },
                        SrcSpan {
                            start: 118,
                            end: 145
                        }
                    ]
                    .into()
                ),
                (
                    "Wobble".into(),
                    [SrcSpan {
                        start: 159,
                        end: 186
                    }]
                    .into()
                )
            ]
            .into()
        )]
        .into()
    );
}
#[test]
fn external_unqualified_type_usages() {
    let module = compile_module(
        "test_module",
        "
import test_module/test_module_inner.{type Wibble}

pub fn main() -> Wibble {
  test_module_inner.Wibble(1)
}
",
        None,
        vec![(
            "thepackage",
            "test_module/test_module_inner",
            "pub type Wibble { Wibble(Int) Wobble(String) }",
        )],
    )
    .unwrap();
    assert_eq!(
        module.type_info.external_type_usages,
        [(
            "test_module/test_module_inner".into(),
            [(
                "Wibble".into(),
                [
                    SrcSpan { start: 39, end: 50 },
                    SrcSpan { start: 70, end: 76 }
                ]
                .into()
            ),]
            .into()
        )]
        .into()
    );
}

#[test]
fn external_qualified_type_usages() {
    let module = compile_module(
        "test_module",
        "
import test_module/test_module_inner

pub fn main() -> test_module_inner.Wibble {
  test_module_inner.Wibble(1)
}
",
        None,
        vec![(
            "thepackage",
            "test_module/test_module_inner",
            "pub type Wibble { Wibble(Int) Wobble(String) }",
        )],
    )
    .unwrap();
    assert_eq!(
        module.type_info.external_type_usages,
        [(
            "test_module/test_module_inner".into(),
            [("Wibble".into(), [SrcSpan { start: 56, end: 80 }].into()),].into()
        )]
        .into()
    );
}

#[test]
fn external_unqualified_type_in_custom_usages() {
    let module = compile_module(
        "test_module",
        "
import test_module/test_module_inner.{type Wibble}

pub type Wobble {
  Wobble(Wibble)
}

pub fn main() {
    Wobble(test_module_inner.Wibble(1))
}
",
        None,
        vec![(
            "thepackage",
            "test_module/test_module_inner",
            "pub type Wibble { Wibble(Int) Wobble(String) }",
        )],
    )
    .unwrap();
    assert_eq!(
        module.type_info.external_type_usages,
        [(
            "test_module/test_module_inner".into(),
            [(
                "Wibble".into(),
                [
                    SrcSpan { start: 39, end: 50 },
                    SrcSpan { start: 80, end: 86 }
                ]
                .into()
            ),]
            .into()
        )]
        .into()
    );
}

#[test]
fn external_qualified_type_in_custom_usages() {
    let module = compile_module(
        "test_module",
        "
import test_module/test_module_inner

pub type Wobble {
  Wobble(test_module_inner.Wibble)
}

pub fn main() {
    Wobble(test_module_inner.Wibble(1))
}
",
        None,
        vec![(
            "thepackage",
            "test_module/test_module_inner",
            "pub type Wibble { Wibble(Int) Wobble(String) }",
        )],
    )
    .unwrap();
    assert_eq!(
        module.type_info.external_type_usages,
        [(
            "test_module/test_module_inner".into(),
            [("Wibble".into(), [SrcSpan { start: 66, end: 90 }].into()),].into()
        )]
        .into()
    );
}

#[test]
fn external_unqualified_type_in_alias_usages() {
    let module = compile_module(
        "test_module",
        "
import test_module/test_module_inner.{type Wibble}

pub type Wobble = Wibble

pub fn main() -> Wobble {
    test_module_inner.Wibble(1)
}
",
        None,
        vec![(
            "thepackage",
            "test_module/test_module_inner",
            "pub type Wibble { Wibble(Int) Wobble(String) }",
        )],
    )
    .unwrap();
    assert_eq!(
        module.type_info.external_type_usages,
        [(
            "test_module/test_module_inner".into(),
            [(
                "Wibble".into(),
                [
                    SrcSpan { start: 39, end: 50 },
                    SrcSpan { start: 71, end: 77 }
                ]
                .into()
            ),]
            .into()
        )]
        .into()
    );
}

#[test]
fn external_qualified_type_in_alias_usages() {
    let module = compile_module(
        "test_module",
        "
import test_module/test_module_inner

pub type Wobble = test_module_inner.Wibble

pub fn main() -> Wobble {
    test_module_inner.Wibble(1)
}
",
        None,
        vec![(
            "thepackage",
            "test_module/test_module_inner",
            "pub type Wibble { Wibble(Int) Wobble(String) }",
        )],
    )
    .unwrap();
    assert_eq!(
        module.type_info.external_type_usages,
        [(
            "test_module/test_module_inner".into(),
            [("Wibble".into(), [SrcSpan { start: 57, end: 81 }].into()),].into()
        )]
        .into()
    );
}

#[test]
fn external_unqualified_type_annotation_usages() {
    let module = compile_module(
        "test_module",
        "
import test_module/test_module_inner.{type Wibble}

pub fn main() {
  let w: Wibble = test_module_inner.Wibble(1)
}
",
        None,
        vec![(
            "thepackage",
            "test_module/test_module_inner",
            "pub type Wibble { Wibble(Int) Wobble(String) }",
        )],
    )
    .unwrap();
    assert_eq!(
        module.type_info.external_type_usages,
        [(
            "test_module/test_module_inner".into(),
            [(
                "Wibble".into(),
                [
                    SrcSpan { start: 39, end: 50 },
                    SrcSpan { start: 78, end: 84 }
                ]
                .into()
            ),]
            .into()
        )]
        .into()
    );
}

#[test]
fn external_qualified_type_annotation_usages() {
    let module = compile_module(
        "test_module",
        "
import test_module/test_module_inner

pub fn main() {
  let w: test_module_inner.Wibble = test_module_inner.Wibble(1)
}
",
        None,
        vec![(
            "thepackage",
            "test_module/test_module_inner",
            "pub type Wibble { Wibble(Int) Wobble(String) }",
        )],
    )
    .unwrap();
    assert_eq!(
        module.type_info.external_type_usages,
        [(
            "test_module/test_module_inner".into(),
            [("Wibble".into(), [SrcSpan { start: 64, end: 88 }].into()),].into()
        )]
        .into()
    );
}
