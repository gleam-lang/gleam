use std::collections::{HashMap, HashSet};

use ecow::EcoString;
use itertools::Itertools;

use crate::{
    ast::SrcSpan,
    docvec,
    javascript::{JavaScriptCodegenTarget, INDENT},
    line_numbers::LineNumbers,
    pretty::{break_, concat, join, line, Document, Documentable},
};

/// A collection of JavaScript import statements from Gleam imports and from
/// external functions, to be rendered into a JavaScript module.
///
#[derive(Debug, Default)]
pub(crate) struct Imports<'a> {
    imports: HashMap<EcoString, Import<'a>>,
    exports: HashSet<EcoString>,
}

impl<'a> Imports<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register_export(&mut self, export: EcoString) {
        let _ = self.exports.insert(export);
    }

    pub fn register_module(
        &mut self,
        path: EcoString,
        location: ImportLocation,
        aliases: impl IntoIterator<Item = EcoString>,
        unqualified_imports: impl IntoIterator<Item = Member<'a>>,
    ) {
        let import = self
            .imports
            .entry(path.clone())
            .or_insert_with(|| Import::new(path.clone(), location));
        import.aliases.extend(aliases);
        import.unqualified.extend(unqualified_imports)
    }

    pub fn into_doc(
        self,
        codegen_target: JavaScriptCodegenTarget,
        line_numbers: &LineNumbers,
    ) -> Document<'a> {
        let imports = concat(
            self.imports
                .into_values()
                .sorted_by(|a, b| a.path.cmp(&b.path))
                .map(|import| Import::into_doc(import, codegen_target, line_numbers)),
        );

        if self.exports.is_empty() {
            imports
        } else {
            let names = join(
                self.exports
                    .into_iter()
                    .sorted()
                    .map(|string| string.to_doc()),
                break_(",", ", "),
            );
            let names = docvec![
                docvec![break_("", " "), names].nest(INDENT),
                break_(",", " ")
            ]
            .group();
            imports
                .append(line())
                .append("export {")
                .append(names)
                .append("};")
                .append(line())
        }
    }

    pub fn is_empty(&self) -> bool {
        self.imports.is_empty() && self.exports.is_empty()
    }
}

#[derive(Debug)]
struct Import<'a> {
    path: EcoString,
    aliases: HashSet<EcoString>,
    unqualified: Vec<Member<'a>>,
    location: ImportLocation,
}

#[derive(Debug)]
pub enum ImportLocation {
    Prelude,
    AtLocation(SrcSpan),
}

impl<'a> Import<'a> {
    fn new(path: EcoString, location: ImportLocation) -> Self {
        Self {
            path,
            aliases: Default::default(),
            unqualified: Default::default(),
            location,
        }
    }

    pub fn into_doc(
        self,
        codegen_target: JavaScriptCodegenTarget,
        line_numbers: &LineNumbers,
    ) -> Document<'a> {
        let path = self.path.to_doc();
        let import_modifier = if codegen_target == JavaScriptCodegenTarget::TypeScriptDeclarations {
            "type "
        } else {
            ""
        };
        let maybe_location = match self.location {
            ImportLocation::Prelude => None,
            ImportLocation::AtLocation(location) => {
                let (start, end) = line_numbers.line_and_column_number_of_src_span(location);
                Some((start, end))
            }
        };
        let alias_imports = concat(self.aliases.into_iter().sorted().map(|alias| {
            let doc = docvec![
                "import ",
                import_modifier,
                "* as ",
                alias,
                " from \"",
                path.clone(),
                r#"";"#,
                line()
            ];
            match maybe_location {
                Some((start, end)) => doc.attach_sourcemap_location(start, end),
                None => doc,
            }
        }));
        if self.unqualified.is_empty() {
            alias_imports
        } else {
            let members = self.unqualified.into_iter().map(Member::into_doc);
            let members = join(members, break_(",", ", "));
            let members = docvec![
                docvec![break_("", " "), members].nest(INDENT),
                break_(",", " ")
            ]
            .group();
            let doc = docvec![
                alias_imports,
                "import ",
                import_modifier,
                "{",
                members,
                "} from \"",
                path,
                r#"";"#,
                line()
            ];
            match maybe_location {
                Some((start, end)) => doc.attach_sourcemap_location(start, end),
                None => doc,
            }
        }
    }
}

#[derive(Debug)]
pub struct Member<'a> {
    pub name: Document<'a>,
    pub alias: Option<Document<'a>>,
}

impl<'a> Member<'a> {
    fn into_doc(self) -> Document<'a> {
        match self.alias {
            None => self.name,
            Some(alias) => docvec![self.name, " as ", alias],
        }
    }
}

#[test]
fn into_doc() {
    let mut imports = Imports::new();
    imports.register_module("./gleam/empty".into(), ImportLocation::Prelude, [], []);
    imports.register_module(
        "./multiple/times".into(),
        ImportLocation::Prelude,
        ["wibble".into(), "wobble".into()],
        [],
    );
    imports.register_module(
        "./multiple/times".into(),
        ImportLocation::Prelude,
        ["wubble".into()],
        [],
    );
    imports.register_module(
        "./multiple/times".into(),
        ImportLocation::Prelude,
        [],
        [Member {
            name: "one".to_doc(),
            alias: None,
        }],
    );

    imports.register_module(
        "./other".into(),
        ImportLocation::Prelude,
        [],
        [
            Member {
                name: "one".to_doc(),
                alias: None,
            },
            Member {
                name: "one".to_doc(),
                alias: Some("onee".to_doc()),
            },
            Member {
                name: "two".to_doc(),
                alias: Some("twoo".to_doc()),
            },
        ],
    );

    imports.register_module(
        "./other".into(),
        ImportLocation::Prelude,
        [],
        [
            Member {
                name: "three".to_doc(),
                alias: None,
            },
            Member {
                name: "four".to_doc(),
                alias: None,
            },
        ],
    );

    imports.register_module(
        "./zzz".into(),
        ImportLocation::Prelude,
        [],
        [
            Member {
                name: "one".to_doc(),
                alias: None,
            },
            Member {
                name: "two".to_doc(),
                alias: None,
            },
        ],
    );

    let line_numbers = LineNumbers::new("");
    assert_eq!(
        line()
            .append(imports.into_doc(JavaScriptCodegenTarget::JavaScript, &line_numbers))
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
