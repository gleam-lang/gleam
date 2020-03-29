use crate::doc::doc::*;
use crate::error::{Error, FileIOAction, FileKind};
use handlebars::Handlebars;
use pulldown_cmark;
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

pub struct DocWriter<'a> {
    project_name: String,
    project_version: String,
    doc_dir: PathBuf,
    modules: Vec<Module>,
    registry: Handlebars<'a>,
    package_root: PathBuf,
}

fn render_markdown(text: &str) -> String {
    let mut s = String::with_capacity(text.len() * 3 / 2);
    let p = pulldown_cmark::Parser::new(&*text);
    pulldown_cmark::html::push_html(&mut s, p);
    s
}

handlebars_helper!(markdown: |text: str| render_markdown(&text));

impl DocWriter<'_> {
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
            registry: {
                let module_template = std::include_str!("views/module.html");
                let index_template = std::include_str!("views/index.html");
                let mut registry = Handlebars::new();
                registry.register_helper("markdown", Box::new(markdown));
                registry
                    .register_template_string("module", module_template)
                    .expect("module template");
                registry
                    .register_template_string("index", index_template)
                    .expect("index template");
                registry
            },
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
        let rendered = self
            .registry
            .render("index", module)
            .map_err(|err| Error::FileIO {
                path: self.doc_dir.join("index.html"),
                action: FileIOAction::Create,
                kind: FileKind::File,
                err: Some(err.to_string()),
            })?;

        self.write_to_path(rendered.as_bytes(), self.doc_dir.join("index.html"))
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
            std::include_str!("views/index.css").as_bytes(),
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

    fn write_doc(self: &Self, module: Module) -> Result<(), Error> {
        let filename = format!("{}/index.html", module.module_name);
        let doc_dir_path = self.doc_dir.join(&filename);
        let doc_text = self
            .registry
            .render("module", &module)
            .map_err(|err| Error::FileIO {
                action: FileIOAction::Parse,
                kind: FileKind::File,
                path: doc_dir_path.clone(),
                err: Some(err.to_string()),
            })?;
        self.write_to_path(doc_text.as_bytes(), doc_dir_path)
    }
}
