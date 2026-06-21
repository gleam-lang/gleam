// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2021 The Gleam contributors

use std::collections::{HashMap, HashSet};

use ecow::EcoString;
use itertools::Itertools;

use crate::javascript::{INDENT, JavaScriptCodegenTarget};
use pretty_arena::*;

/// A collection of JavaScript import statements from Gleam imports and from
/// external functions, to be rendered into a JavaScript module.
///
#[derive(Debug, Default)]
pub(crate) struct Imports<'a, 'doc> {
    imports: HashMap<EcoString, Import<'a, 'doc>>,
    exports: HashSet<EcoString>,
}

impl<'a, 'doc> Imports<'a, 'doc> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register_export(&mut self, export: EcoString) {
        let _ = self.exports.insert(export);
    }

    pub fn register_module(
        &mut self,
        path: EcoString,
        aliases: impl IntoIterator<Item = EcoString>,
        unqualified_imports: impl IntoIterator<Item = Member<'a, 'doc>>,
    ) {
        let import = self
            .imports
            .entry(path.clone())
            .or_insert_with(|| Import::new(path.clone()));
        import.aliases.extend(aliases);
        import.unqualified.extend(unqualified_imports)
    }

    pub fn into_doc(
        self,
        arena: &'doc DocumentArena<'a, 'doc>,
        codegen_target: JavaScriptCodegenTarget,
    ) -> Document<'a, 'doc> {
        let imports = arena.concat(
            self.imports
                .into_values()
                .sorted_by(|a, b| a.path.cmp(&b.path))
                .map(|import| import.into_doc(arena, codegen_target)),
        );

        if self.exports.is_empty() {
            imports
        } else {
            let names = arena.join(
                self.exports
                    .into_iter()
                    .sorted()
                    .map(|string| string.to_doc(arena)),
                arena.break_(",", ", "),
            );
            let names = docvec![
                arena,
                docvec![arena, arena.break_("", " "), names].nest(arena, INDENT),
                arena.break_(",", " ")
            ]
            .group(arena);

            let export_keyword = match codegen_target {
                JavaScriptCodegenTarget::JavaScript => "export {",
                JavaScriptCodegenTarget::TypeScriptDeclarations => "export type {",
            };

            docvec![
                arena,
                imports,
                LINE_DOCUMENT,
                export_keyword,
                names,
                "};",
                LINE_DOCUMENT
            ]
        }
    }

    pub fn is_empty(&self) -> bool {
        self.imports.is_empty() && self.exports.is_empty()
    }

    /// Remove variants which are imported in Gleam code, but not needed to be
    /// imported because singleton constants are used instead.
    ///
    pub fn filter_unused_variants<I>(&mut self, unused: I)
    where
        I: Iterator<Item = (EcoString, EcoString)>,
    {
        for (path, name) in unused {
            if let Some(import_) = self.imports.get_mut(&path)
                && let Some(index) = import_
                    .unqualified
                    .iter()
                    .position(|member| member.name == name)
            {
                _ = import_.unqualified.remove(index);
            }
        }
    }
}

#[derive(Debug)]
struct Import<'a, 'doc> {
    path: EcoString,
    aliases: HashSet<EcoString>,
    unqualified: Vec<Member<'a, 'doc>>,
}

impl<'a, 'doc> Import<'a, 'doc> {
    fn new(path: EcoString) -> Self {
        Self {
            path,
            aliases: Default::default(),
            unqualified: Default::default(),
        }
    }

    pub fn into_doc(
        self,
        arena: &'doc DocumentArena<'a, 'doc>,
        codegen_target: JavaScriptCodegenTarget,
    ) -> Document<'a, 'doc> {
        let path = self.path.to_doc(arena);
        let import_modifier = if codegen_target == JavaScriptCodegenTarget::TypeScriptDeclarations {
            "type "
        } else {
            ""
        };
        let alias_imports = arena.concat(self.aliases.into_iter().sorted().map(|alias| {
            docvec![
                arena,
                "import ",
                import_modifier,
                "* as ",
                alias,
                " from \"",
                path.clone(),
                r#"";"#,
                LINE_DOCUMENT
            ]
        }));
        if self.unqualified.is_empty() {
            alias_imports
        } else {
            let members = self
                .unqualified
                .into_iter()
                .map(|member| member.into_doc(arena));
            let members = arena.join(members, arena.break_(",", ", "));
            let members = docvec![
                arena,
                docvec![arena, arena.break_("", " "), members].nest(arena, INDENT),
                arena.break_(",", " ")
            ]
            .group(arena);
            docvec![
                arena,
                alias_imports,
                "import ",
                import_modifier,
                "{",
                members,
                "} from \"",
                path,
                r#"";"#,
                LINE_DOCUMENT
            ]
        }
    }
}

#[derive(Debug)]
pub struct Member<'a, 'doc> {
    pub name: EcoString,
    pub alias: Option<Document<'a, 'doc>>,
}

impl<'a, 'doc> Member<'a, 'doc> {
    fn into_doc(self, arena: &'doc DocumentArena<'a, 'doc>) -> Document<'a, 'doc> {
        match self.alias {
            None => self.name.to_doc(arena),
            Some(alias) => docvec![arena, self.name, " as ", alias],
        }
    }
}

#[test]
fn into_doc() {
    let arena = DocumentArena::new();
    let mut imports = Imports::new();
    imports.register_module("./gleam/empty".into(), [], []);
    imports.register_module(
        "./multiple/times".into(),
        ["wibble".into(), "wobble".into()],
        [],
    );
    imports.register_module("./multiple/times".into(), ["wubble".into()], []);
    imports.register_module(
        "./multiple/times".into(),
        [],
        [Member {
            name: "one".into(),
            alias: None,
        }],
    );

    imports.register_module(
        "./other".into(),
        [],
        [
            Member {
                name: "one".into(),
                alias: None,
            },
            Member {
                name: "one".into(),
                alias: Some("onee".to_doc(&arena)),
            },
            Member {
                name: "two".into(),
                alias: Some("twoo".to_doc(&arena)),
            },
        ],
    );

    imports.register_module(
        "./other".into(),
        [],
        [
            Member {
                name: "three".into(),
                alias: None,
            },
            Member {
                name: "four".into(),
                alias: None,
            },
        ],
    );

    imports.register_module(
        "./zzz".into(),
        [],
        [
            Member {
                name: "one".into(),
                alias: None,
            },
            Member {
                name: "two".into(),
                alias: None,
            },
        ],
    );

    assert_eq!(
        LINE_DOCUMENT
            .append(
                &arena,
                imports.into_doc(&arena, JavaScriptCodegenTarget::JavaScript)
            )
            .to_pretty_string(40),
        r#"
import * as wibble from "./multiple/times";
import * as wobble from "./multiple/times";
import * as wubble from "./multiple/times";
import { one } from "./multiple/times";
import {
  one,
  one as onee,
  two as twoo,
  three,
  four,
} from "./other";
import { one, two } from "./zzz";
"#
        .to_string()
    );
}
