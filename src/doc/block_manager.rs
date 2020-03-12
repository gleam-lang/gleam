use crate::ast::{Statement, TypedModule};
use crate::doc::doc::*;
use crate::pretty::Documentable;
use itertools::Itertools;
use std::collections::HashMap;

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
        .and_then(|b: &RawComment| Some(b.line_no == line_no - 1))
        .unwrap_or(false)
      {
        let doc_text = block.iter().map(|b| b.content.to_string()).join("\n");
        return Some(doc_text);
      }
    }

    if self
      .current_block
      .last()
      .and_then(|b: &RawComment| Some(b.line_no == line_no - 1))
      .unwrap_or(false)
    {
      let doc_text = self
        .current_block
        .iter()
        .map(|b| b.content.to_string())
        .join("\n");
      return Some(doc_text);
    }

    None
  }

  pub fn add_line(self: &mut Self, line_no: usize, content: String) {
    match self.current_block.last() {
      Some(RawComment {
        line_no: l,
        content: c,
      }) => {
        let length = content.len();
        let new_line = RawComment {
          line_no: line_no,
          content,
        };
        if (line_no - length - 4) == *l {
          self.current_block.push(new_line);
        } else {
          let copy = std::mem::replace(&mut self.current_block, vec![]);
          self.blocks.push(copy);
          self.current_block.push(new_line);
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
        Statement::Fn {
          meta,
          name,
          args,
          public: true,
          return_annotation,
          ..
        } => {
          let doc = crate::ast::pretty::function_signature(
            meta.clone(),
            name.clone(),
            args.clone(),
            true,
            return_annotation.clone(),
          );
          let fn_docs: Option<HashMap<String, String>> =
            self.find_block_for_line(meta.start).map(|d| {
              vec![("en-US".to_string(), d.trim().to_string())]
                .into_iter()
                .collect()
            });
          docs.push(EEP48Doc {
            name: name.to_string(),
            arity: args.len(),
            signature: vec![crate::pretty::format(80, doc)],
            doc: fn_docs,
            meta: DocMeta::new("".to_string()),
            typ: DocType::Fn,
          });
        }
        Statement::TypeAlias {
          meta,
          args,
          alias,
          resolved_type,
          public: true,
        } => {
          let doc = Statement::<crate::ast::TypedExpr>::TypeAlias {
            meta: meta.clone(),
            args: args.clone(),
            alias: alias.clone(),
            resolved_type: resolved_type.clone(),
            public: true,
          }
          .to_doc();
          let fn_docs: Option<HashMap<String, String>> = self
            .find_block_for_line(meta.start)
            .map(|d| vec![("en-US".to_string(), d)].into_iter().collect());
          docs.push(EEP48Doc {
            name: alias.to_string(),
            arity: args.len(),
            signature: vec![crate::pretty::format(80, doc)],
            doc: fn_docs,
            meta: DocMeta::new("".to_string()),
            typ: DocType::TypeAlias,
          });
        }
        Statement::CustomType {
          meta,
          name,
          args,
          constructors,
          ..
        } => {
          let doc = Statement::<crate::ast::TypedExpr>::CustomType {
            meta: meta.clone(),
            args: args.clone(),
            name: name.clone(),
            constructors: constructors.clone(),
            public: true,
          }
          .to_doc();
          let fn_docs: Option<HashMap<String, String>> = self
            .find_block_for_line(meta.start)
            .map(|d| vec![("en-US".to_string(), d)].into_iter().collect());
          docs.push(EEP48Doc {
            name: name.to_string(),
            arity: args.len(),
            signature: vec![crate::pretty::format(80, doc)],
            doc: fn_docs,
            meta: DocMeta::new("".to_string()),
            typ: DocType::CustomType,
          });
        }
        Statement::ExternalFn {
          meta,
          name,
          args,
          retrn,
          ..
        } => {
          use crate::ast;
          let doc = crate::ast::pretty::function_signature(
            meta.clone(),
            name.clone(),
            args
              .clone()
              .iter()
              .map(|arg: &ast::ExternalFnArg| ast::Arg {
                names: arg
                  .label
                  .clone()
                  .map(|l| ast::ArgNames::LabelledDiscard { label: l })
                  .unwrap_or(ast::ArgNames::Discard),
                annotation: Some(arg.typ.clone()),
                meta: ast::Meta { start: 0, end: 0 },
              })
              .collect(),
            true,
            Some(retrn.clone()),
          );
          let fn_docs: Option<HashMap<String, String>> = self
            .find_block_for_line(meta.start)
            .map(|d| vec![("en-US".to_string(), d)].into_iter().collect());
          docs.push(EEP48Doc {
            name: name.to_string(),
            arity: args.len(),
            signature: vec![crate::pretty::format(80, doc.to_doc())],
            doc: fn_docs,
            meta: DocMeta::new("".to_string()),
            typ: DocType::Fn,
          });
        }
        Statement::ExternalType {
          meta, name, args, ..
        } => {
          let doc = Statement::<crate::ast::TypedExpr>::ExternalType {
            meta: meta.clone(),
            args: args.clone(),
            name: name.clone(),
            public: true,
          }
          .to_doc();
          let fn_docs: Option<HashMap<String, String>> = self
            .find_block_for_line(meta.start)
            .map(|d| vec![("en-US".to_string(), d)].into_iter().collect());
          docs.push(EEP48Doc {
            name: name.to_string(),
            arity: args.len(),
            signature: vec![crate::pretty::format(80, doc)],
            doc: fn_docs,
            meta: DocMeta::new("".to_string()),
            typ: DocType::ExternalType,
          });
        }
        _ => {}
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
