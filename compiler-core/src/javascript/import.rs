use std::collections::HashMap;

use itertools::Itertools;

use crate::{
    ast::UnqualifiedImport,
    docvec,
    javascript::INDENT,
    pretty::{break_, concat, line, Document, Documentable},
};

/// A collection of JavaScript import statements from Gleam imports and from
/// external functions, to be rendered into a JavaScript module.
///
#[derive(Debug, Default)]
pub(crate) struct Imports<'a> {
    imports: HashMap<String, Import<'a>>,
}

impl<'a> Imports<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register_module(
        &mut self,
        js_path: String,
        aliases: impl Iterator<Item = &'a str>,
        unqualified_imports: impl Iterator<Item = Member<'a>>,
    ) {
        let import = self
            .imports
            .entry(js_path.clone())
            .or_insert_with(|| Import::new(js_path.clone()));
        import.aliases.extend(aliases);
        import.unqualified.extend(unqualified_imports)
    }

    pub fn into_doc(self) -> Document<'a> {
        concat(
            self.imports
                .into_values()
                .sorted_by(|a, b| a.path.cmp(&b.path))
                .map(Import::into_doc),
        )
    }
}

#[derive(Debug)]
struct Import<'a> {
    path: String,
    aliases: Vec<&'a str>,
    unqualified: Vec<Member<'a>>,
}

impl<'a> Import<'a> {
    fn new(path: String) -> Self {
        Self {
            path,
            aliases: Default::default(),
            unqualified: Default::default(),
        }
    }

    pub fn into_doc(self) -> Document<'a> {
        let path = Document::String(self.path.clone());
        let alias_imports = concat(self.aliases.into_iter().map(|alias| {
            docvec![
                "import * as ",
                alias,
                " from \"",
                path.clone(),
                r#"";"#,
                line()
            ]
        }));
        if self.unqualified.is_empty() {
            alias_imports
        } else {
            let members = self.unqualified.into_iter().map(Member::into_doc);
            let members = concat(Itertools::intersperse(members, break_(",", ", ")));
            let members = docvec![
                docvec![break_("", " "), members].nest(INDENT),
                break_(",", " ")
            ]
            .group();
            docvec![
                alias_imports,
                "import {",
                members,
                "} from \"",
                path,
                r#"";"#,
                line()
            ]
        }
    }
}

#[derive(Debug)]
pub struct Member<'a> {
    name: Document<'a>,
    alias: Option<Document<'a>>,
}

impl<'a> Member<'a> {
    fn from_unqualified_import(import: &'a UnqualifiedImport) -> Self {
        Self {
            name: import.name.as_str().to_doc(),
            alias: import.as_name.as_ref().map(|alias| alias.as_str().to_doc()),
        }
    }

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
    imports.register_module(
        "./gleam/empty".to_string(),
        std::iter::empty(),
        std::iter::empty(),
    );
    imports.register_module(
        "./multiple/times".to_string(),
        vec!["wibble", "wobble"].into_iter(),
        std::iter::empty(),
    );
    imports.register_module(
        "./multiple/times".to_string(),
        vec!["wubble"].into_iter(),
        std::iter::empty(),
    );
    imports.register_module(
        "./multiple/times".to_string(),
        std::iter::empty(),
        vec![Member {
            name: "one".to_doc(),
            alias: None,
        }]
        .into_iter(),
    );

    imports.register_module(
        "./other".to_string(),
        std::iter::empty(),
        vec![
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
            Member {
                name: "three".to_doc(),
                alias: None,
            },
            Member {
                name: "four".to_doc(),
                alias: None,
            },
        ]
        .into_iter(),
    );

    imports.register_module(
        "./zzz".to_string(),
        std::iter::empty(),
        vec![
            Member {
                name: "one".to_doc(),
                alias: None,
            },
            Member {
                name: "two".to_doc(),
                alias: None,
            },
        ]
        .into_iter(),
    );

    assert_eq!(
        line().append(imports.into_doc()).to_pretty_string(40),
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
