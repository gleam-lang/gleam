use crate::doc::doc::*;
use crate::error::{Error, FileIOAction, FileKind};
use askama::Template;
// use pulldown_cmark;
use crate::error::GleamExpect;
use serde::Serialize;
use std::collections::HashMap;
use std::fs::{read_to_string, File};
use std::io::Write;
use std::path::PathBuf;

#[derive(Debug, Serialize, Clone)]
struct ModuleItem {
    pub name: String,
    pub doc: Option<String>,
    pub signature: String,
}

#[derive(Debug, Serialize, Clone)]
struct Module {
    pub module_name: String,
    pub project_name: String,
    pub project_version: String,
    pub all_modules: Option<Vec<Module>>,
    pub module_doc: String,
    pub types_doc: Vec<ModuleItem>,
    pub functions_doc: Vec<ModuleItem>,
    pub readme: String,
}

pub struct DocWriter {
    project_name: String,
    project_version: String,
    doc_dir: PathBuf,
    modules: Vec<Module>,
    package_root: PathBuf,
}

// TODO
// fn render_markdown(text: &str) -> String {
//     let mut s = String::with_capacity(text.len() * 3 / 2);
//     let p = pulldown_cmark::Parser::new(text);
//     pulldown_cmark::html::push_html(&mut s, p);
//     s
// }

impl DocWriter {
    pub fn new(
        project_name: String,
        project_version: String,
        doc_dir: PathBuf,
        package_root: PathBuf,
    ) -> Self {
        DocWriter {
            project_name,
            project_version,
            doc_dir,
            modules: vec![],
            package_root,
        }
    }

    pub fn add_chunk(self: &mut Self, chunk: EEP48DocChunk) {
        if chunk.anno.file.ends_with("_test") {
            return;
        }
        self.modules.push(self.chunk_to_module(chunk));
    }

    fn write_index(self: &Self, module: &Module) -> Result<(), Error> {
        // let rendered = self
        //     .registry
        //     .render("index", module)
        //     .map_err(|err| Error::FileIO {
        //         path: self.doc_dir.join("index.html"),
        //         action: FileIOAction::Create,
        //         kind: FileKind::File,
        //         err: Some(err.to_string()),
        //     })?;
        let index = PageTemplate {
            content: module.readme.as_ref(),
            functions: &[],
            links: &[],
            pages: &[],
            modules: &[],
            types: &[],
            page_title: module.project_name.as_ref(),
            project_name: module.project_name.as_ref(),
            project_version: module.project_version.as_ref(),
        };

        self.write_to_path(
            index
                .render()
                .gleam_expect("Template rendering failed")
                .as_bytes(),
            self.doc_dir.join("index.html"),
        )
    }

    pub fn write(self: &Self) -> Result<(), Error> {
        let readme =
            read_to_string(self.package_root.join("README.md")).map_err(|_| Error::FileIO {
                path: self.package_root.join("README.md"),
                action: FileIOAction::Read,
                kind: FileKind::File,
                err: Some("Could not read README.md for doc index".to_string()),
            })?;

        self.write_to_path(
            std::include_str!("index.css").as_bytes(),
            self.doc_dir.join("index.css"),
        )?;

        self.modules
            .iter()
            .map(|module| {
                let module = Module {
                    all_modules: Some(self.modules.clone()),
                    module_name: module.module_name.clone(),
                    project_name: module.project_name.clone(),
                    project_version: module.project_version.clone(),
                    module_doc: module.module_doc.clone(),
                    types_doc: module
                        .types_doc
                        .iter()
                        .map(|d| d.clone().to_owned())
                        .collect(),
                    functions_doc: module
                        .functions_doc
                        .iter()
                        .map(|d| d.clone().to_owned())
                        .collect(),
                    readme: readme.to_string(),
                };
                self.write_index(&module)?;
                self.write_doc(module)
            })
            .collect()
    }

    fn chunk_to_module(self: &Self, chunk: EEP48DocChunk) -> Module {
        let transform_doc = |d: EEP48Doc| ModuleItem {
            name: d.name,
            doc: d
                .doc
                .map(|doc| {
                    doc.get(&"en-US".to_string())
                        .unwrap_or(&"".to_string())
                        .clone()
                })
                .to_owned(),
            signature: d.signature.join("\n").trim().to_string(),
        };

        let mut doc_map: HashMap<DocType, Vec<ModuleItem>> = HashMap::new();
        for doc in chunk.docs {
            if let Some(e) = doc_map.get_mut(&doc.typ) {
                e.push(transform_doc(doc));
            } else {
                doc_map.insert(doc.typ, vec![transform_doc(doc)]);
            }
        }

        let mut functions_doc = vec![];
        doc_map
            .get(&DocType::Fn)
            .map(|docs| functions_doc.extend(docs));
        doc_map
            .get(&DocType::ExternalFn)
            .map(|docs| functions_doc.extend(docs));

        let mut types_doc = vec![];
        doc_map
            .get(&DocType::CustomType)
            .map(|docs| types_doc.extend(docs));
        doc_map
            .get(&DocType::TypeAlias)
            .map(|docs| types_doc.extend(docs));
        doc_map
            .get(&DocType::ExternalType)
            .map(|docs| types_doc.extend(docs));

        Module {
            project_name: self.project_name.clone(),
            project_version: self.project_version.clone(),
            module_name: chunk.anno.file.clone(),
            module_doc: chunk
                .module_doc
                .get("en-US")
                .unwrap_or(&"".to_string())
                .clone(),
            functions_doc: functions_doc.iter().map(|d| d.clone().to_owned()).collect(),
            types_doc: types_doc.iter().map(|d| d.clone().to_owned()).collect(),
            all_modules: None,
            readme: "".to_string(),
        }
    }

    fn write_to_path(self: &Self, text: &[u8], path: PathBuf) -> Result<(), Error> {
        path.parent()
            .ok_or_else(|| Error::FileIO {
                action: FileIOAction::FindParent,
                kind: FileKind::Directory,
                path: path.clone(),
                err: None,
            })
            .and_then(|doc_parent_path| {
                std::fs::create_dir_all(doc_parent_path).map_err(|e| Error::FileIO {
                    action: FileIOAction::Create,
                    kind: FileKind::Directory,
                    path: doc_parent_path.to_path_buf(),
                    err: Some(e.to_string()),
                })
            })
            .and_then(|_| {
                File::create(&path).map_err(|e| Error::FileIO {
                    action: FileIOAction::Create,
                    kind: FileKind::File,
                    path: path.clone(),
                    err: Some(e.to_string()),
                })
            })
            .and_then(|mut doc_f| {
                doc_f.write_all(text).map_err(|e| Error::FileIO {
                    action: FileIOAction::WriteTo,
                    kind: FileKind::File,
                    path: path.clone(),
                    err: Some(e.to_string()),
                })
            })
            .and_then(|_| Ok(()))
    }

    fn write_doc(self: &Self, _module: Module) -> Result<(), Error> {
        // let filename = format!("{}/index.html", module.module_name);
        // let doc_dir_path = self.doc_dir.join(&filename);
        // let doc_text = self
        //     .registry
        //     .render("module", &module)
        //     .map_err(|err| Error::FileIO {
        //         action: FileIOAction::Parse,
        //         kind: FileKind::File,
        //         path: doc_dir_path.clone(),
        //         err: Some(err.to_string()),
        //     })?;
        // self.write_to_path(doc_text.as_bytes(), doc_dir_path)
        todo!()
    }
}

struct Link<'a> {
    name: &'a str,
    path: &'a str,
}

struct Function<'a> {
    name: &'a str,
    signature: &'a str,
    documentation: &'a str,
}

struct Type<'a> {
    name: &'a str,
    definition: &'a str,
    documentation: &'a str,
}

#[derive(Template)]
#[template(path = "documentation_page.html")]
struct PageTemplate<'a> {
    page_title: &'a str,
    project_name: &'a str,
    project_version: &'a str,
    pages: &'a [Link<'a>],
    links: &'a [Link<'a>],
    modules: &'a [Link<'a>],
    functions: &'a [Function<'a>],
    types: &'a [Type<'a>],
    content: &'a str,
}

#[derive(Template)]
#[template(path = "documentation_module.html")]
struct ModuleTemplate<'a> {
    page_title: &'a str,
    module_name: &'a str,
    project_name: &'a str,
    project_version: &'a str,
    module_doc: &'a str,
    pages: &'a [Link<'a>],
    links: &'a [Link<'a>],
    modules: &'a [Link<'a>],
    functions: &'a [Function<'a>],
    types: &'a [Type<'a>],
}
