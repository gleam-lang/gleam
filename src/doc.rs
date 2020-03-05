use super::ast::{Arg, ArgNames, Statement, TypeAst, TypedModule};
use crate::ast::pretty;
use crate::error::Error;
use crate::pretty::Documentable;
use itertools::Itertools;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

#[derive(Debug, PartialEq)]
struct RawComment {
  pub line_no: usize,
  pub content: String,
}

#[derive(Debug, PartialEq)]
pub struct DocBlockManager {
  pub(self) current_block: Vec<RawComment>,
  pub(self) blocks: Vec<Vec<RawComment>>,
  num_comments: usize,
}

impl DocBlockManager {
  pub fn new() -> Self {
    DocBlockManager {
      current_block: vec![],
      blocks: vec![],
      num_comments: 0,
    }
  }

  pub(self) fn block_count(self: &Self) -> usize {
    if self.current_block.is_empty() {
      self.blocks.len()
    } else {
      self.blocks.len() + 1
    }
  }

  pub(self) fn comment_count(self: &Self) -> usize {
    self.num_comments
  }

  pub(self) fn find_block_for_line(self: &Self, line_no: usize) -> Option<String> {
    for block in &self.blocks {
      if block
        .last()
        .and_then(|b: &RawComment| Some(b.line_no == line_no - (b.content.len() + 5)))
        .unwrap_or(false)
      {
        return Some(block.iter().map(|b| b.content.to_string()).join("\n"));
      }
    }

    if self
      .current_block
      .last()
      .and_then(|b: &RawComment| Some(b.line_no == line_no - (b.content.len() + 5)))
      .unwrap_or(false)
    {
      return Some(
        self
          .current_block
          .iter()
          .map(|b| b.content.to_string())
          .join("\n"),
      );
    }

    None
  }

  pub fn add_line(self: &mut Self, line_no: usize, content: String) {
    match self.current_block.last() {
      Some(RawComment {
        line_no: l,
        content: _,
      }) => {
        let new_line = RawComment {
          line_no: line_no,
          content,
        };
        if (line_no - 1) == *l {
          self.current_block.push(new_line);
        } else {
          let copy = std::mem::replace(&mut self.current_block, vec![]);
          self.blocks.push(copy);
        }
      }
      None => {
        self.current_block.push(RawComment { line_no, content });
      }
    }
    self.num_comments = self.num_comments + 1;
  }

  pub fn gen_doc_chunk(self: &Self, module: &TypedModule) -> EEP48DocChunk {
    let mut module_doc = HashMap::new();

    let mut docs = Vec::<EEP48Doc>::new();
    for statement in module.statements.iter() {
      match statement {
        fun
        @
        Statement::Fn {
          meta,
          name,
          args,
          public,
          return_annotation,
          ..
        } => {
          let doc = fun.clone().to_doc();
          let fn_docs: Option<HashMap<String, String>> = self
            .find_block_for_line(meta.start)
            .map(|d| vec![("en-US".to_string(), d)].into_iter().collect());
          docs.push(EEP48Doc {
            name: name.to_string(),
            arity: args.len(),
            signature: vec![crate::pretty::format(80, doc)],
            doc: fn_docs,
            meta: DocMeta::new("".to_string()),
            typ: DocType::Fn,
          });
        }
        ta @ Statement::TypeAlias {
          meta, args, alias, ..
        } => {
          let doc = ta.clone().to_doc();
          let fn_docs: Option<HashMap<String, String>> = self
            .find_block_for_line(meta.start)
            .map(|d| vec![("en-US".to_string(), d)].into_iter().collect());
          docs.push(EEP48Doc {
            name: alias.to_string(),
            arity: args.len(),
            signature: vec![crate::pretty::format(80, doc)],
            doc: fn_docs,
            meta: DocMeta::new("".to_string()),
            typ: DocType::Fn,
          });
        }
        ct @ Statement::CustomType {
          meta, name, args, ..
        } => {
          let doc = ct.clone().to_doc();
          let fn_docs: Option<HashMap<String, String>> = self
            .find_block_for_line(meta.start)
            .map(|d| vec![("en-US".to_string(), d)].into_iter().collect());
          docs.push(EEP48Doc {
            name: name.to_string(),
            arity: args.len(),
            signature: vec![crate::pretty::format(80, doc)],
            doc: fn_docs,
            meta: DocMeta::new("".to_string()),
            typ: DocType::Fn,
          });
        }
        fun @ Statement::ExternalFn {
          meta, name, args, ..
        } => {
          let doc = fun.clone().to_doc();
          let fn_docs: Option<HashMap<String, String>> = self
            .find_block_for_line(meta.start)
            .map(|d| vec![("en-US".to_string(), d)].into_iter().collect());
          docs.push(EEP48Doc {
            name: name.to_string(),
            arity: args.len(),
            signature: vec![crate::pretty::format(80, doc)],
            doc: fn_docs,
            meta: DocMeta::new("".to_string()),
            typ: DocType::Fn,
          });
        }
        et @ Statement::ExternalType {
          meta, name, args, ..
        } => {
          let doc = et.clone().to_doc();
          let fn_docs: Option<HashMap<String, String>> = self
            .find_block_for_line(meta.start)
            .map(|d| vec![("en-US".to_string(), d)].into_iter().collect());
          docs.push(EEP48Doc {
            name: name.to_string(),
            arity: args.len(),
            signature: vec![crate::pretty::format(80, doc)],
            doc: fn_docs,
            meta: DocMeta::new("".to_string()),
            typ: DocType::Fn,
          });
        }
        Statement::Import { .. } => {}
      }
    }
    // TODO: Not the real logic
    module_doc.insert(
      "en-US".to_string(),
      self
        .blocks
        .first()
        .unwrap_or(&vec![])
        .iter()
        .map(|rc| rc.content.to_string())
        .join("\n")
        .to_string(),
    );
    EEP48DocChunk {
      anno: ErlAnno {
        line: 0,
        column: 0,
        file: module.name_string(),
      },
      module_doc: module_doc,
      meta: DocMeta {
        authors: None,
        deprecated: None,
        since: None,
        cross_references: None,
        edit_url: format!("https://www.example.com/{}", module.name_string()),
      },
      docs,
    }
  }
}

fn type_to_string(_typ: &TypeAst) -> String {
  "".to_string()
}

fn arg_to_string(arg: &Arg) -> Option<String> {
  match &arg.names {
    ArgNames::Named { name } => Some(match &arg.annotation {
      Some(annotation) => format!("{}: {}", name, type_to_string(annotation)),
      None => name.to_string(),
    }),
    ArgNames::NamedLabelled { name, label } => Some(match &arg.annotation {
      Some(annotation) => format!("{} {}: {}", label, name, type_to_string(annotation)),
      None => name.to_string(),
    }),
    _ => None,
  }
}

#[derive(Debug, PartialEq)]
struct EEP48Doc {
  name: String,
  arity: usize,
  signature: Vec<String>,
  doc: Option<HashMap<String, String>>,
  meta: DocMeta,
  typ: DocType,
}

#[derive(Debug, PartialEq)]
enum DocType {
  Fn,
  TypeAlias,
  CustomType,
  ExternalFn,
  ExternalType,
}

#[derive(Debug, PartialEq)]
struct ErlAnno {
  pub(self) line: usize,
  pub(self) column: usize,
  pub(self) file: String,
}

#[derive(Debug, PartialEq)]
enum CrossReference {
  ModuleReference {
    module: Vec<String>,
  },
  ModuleItemReference {
    module: Vec<String>,
    name: String,
    arity: usize,
    typ: DocType,
  },
}

/// Our supported meta keys
#[derive(Debug, PartialEq)]
struct DocMeta {
  authors: Option<Vec<String>>,
  deprecated: Option<bool>,
  since: Option<String>,
  cross_references: Option<Vec<CrossReference>>,
  edit_url: String,
}

impl DocMeta {
  pub(self) fn new(edit_url: String) -> Self {
    DocMeta {
      authors: None,
      deprecated: None,
      since: None,
      cross_references: None,
      edit_url,
    }
  }
}

/// A data structure to hold data to generate a BEAM chunk
/// in compliance with http://erlang.org/eeps/eep-0048.html
#[derive(Debug, PartialEq)]
pub struct EEP48DocChunk {
  anno: ErlAnno,
  module_doc: HashMap<String, String>,
  meta: DocMeta,
  docs: Vec<EEP48Doc>,
}

impl EEP48DocChunk {
  pub fn filename(self: &Self) -> String {
    format!("{}.html", self.anno.file)
  }

  pub fn doc_text(self: &Self, project_name: String) -> String {
    let module_doc = self
      .module_doc
      .get(&"en-US".to_string())
      .unwrap_or(&"".to_string())
      .to_string();
    let function_docs: String = self
      .docs
      .iter()
      .filter(|d| match d.typ {
        DocType::Fn { .. } => true,
        _ => false,
      })
      .map(|d: &EEP48Doc| {
        format!(
          "
<h3>{}</h3>
<p>{}</p>
<code>{}</code>
          ",
          d.name,
          d.doc
            .as_ref()
            .and_then(|doc_map| doc_map.get(&"en-US".to_string()))
            .unwrap_or(&"".to_string()),
          d.signature.join("\n")
        )
      })
      .join("\n")
      .to_string();
    if module_doc.is_empty() {
      "".to_string()
    } else {
      format!(
        "
  <html>
    <head>
      <title>{} - {}</title>
    </head>
    <body>
      <h1>{}</h1>
      {}
      <h2>Functions</h2>
      {}
    </body>
  </html>
      ",
        project_name, self.anno.file, self.anno.file, module_doc, function_docs
      )
    }
  }
}

pub struct DocWriter {
  chunks: Vec<EEP48DocChunk>,
  project_name: String,
  doc_dir: PathBuf,
}

impl DocWriter {
  pub fn new(project_name: String, doc_dir: PathBuf) -> Self {
    DocWriter {
      chunks: vec![],
      project_name,
      doc_dir,
    }
  }

  pub fn add_chunk(self: &mut Self, chunk: EEP48DocChunk) {
    self.chunks.push(chunk);
  }

  pub fn write(self: &Self) -> Result<(), Error> {
    let template_fn = move |s| "".to_string();

    self
      .chunks
      .iter()
      .map(|chunk| self.write_doc(chunk, template_fn))
      .collect()
  }

  fn write_doc<F>(self: &Self, doc: &EEP48DocChunk, template_fn: F) -> Result<(), Error>
  where
    F: FnOnce(String) -> String,
  {
    use crate::error::{FileIOAction, FileKind};
    let doc_text = doc.doc_text(self.project_name.clone());
    let filename = doc.filename();
    let doc_dir_path = self.doc_dir.join(&filename);
    doc_dir_path
      .parent()
      .ok_or_else(|| Error::FileIO {
        action: FileIOAction::FindParent,
        kind: FileKind::Directory,
        path: doc_dir_path.clone(),
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
        File::create(&doc_dir_path).map_err(|e| Error::FileIO {
          action: FileIOAction::Create,
          kind: FileKind::File,
          path: doc_dir_path.clone(),
          err: Some(e.to_string()),
        })
      })
      .and_then(|mut doc_f| {
        doc_f
          .write_all(doc_text.as_bytes())
          .map_err(|e| Error::FileIO {
            action: FileIOAction::WriteTo,
            kind: FileKind::File,
            path: doc_dir_path.clone(),
            err: Some(e.to_string()),
          })
      })
      .and_then(|_| Ok(()))
  }
}
