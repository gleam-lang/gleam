//! This module implements the functionality described in
//! ["Strictly Pretty" (2000) by Christian Lindig][0], with a few
//! extensions.
//!
//! This module is heavily influenced by Elixir's Inspect.Algebra and
//! JavaScript's Prettier.
//!
//! [0]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.34.2200
//!
//! ## Extensions
//!
//! - `ForceBreak` from Prettier.
//! - `FlexBreak` from Elixir.

#[cfg(test)]
mod tests;

use crate::{fs::Utf8Writer, GleamExpect, Result};

pub trait Documentable {
    fn to_doc(self) -> Document;
}

impl Documentable for &str {
    fn to_doc(self) -> Document {
        Document::String(self.to_string())
    }
}

impl Documentable for String {
    fn to_doc(self) -> Document {
        Document::String(self)
    }
}

impl Documentable for isize {
    fn to_doc(self) -> Document {
        Document::String(format!("{}", self))
    }
}

impl Documentable for i64 {
    fn to_doc(self) -> Document {
        Document::String(format!("{}", self))
    }
}

impl Documentable for usize {
    fn to_doc(self) -> Document {
        Document::String(format!("{}", self))
    }
}

impl Documentable for f64 {
    fn to_doc(self) -> Document {
        Document::String(format!("{:?}", self))
    }
}

impl Documentable for u64 {
    fn to_doc(self) -> Document {
        Document::String(format!("{:?}", self))
    }
}

impl Documentable for Document {
    fn to_doc(self) -> Document {
        self
    }
}

impl Documentable for Vec<Document> {
    fn to_doc(self) -> Document {
        concat(self.into_iter())
    }
}

impl<D: Documentable> Documentable for Option<D> {
    fn to_doc(self) -> Document {
        match self {
            Some(d) => d.to_doc(),
            None => Document::Nil,
        }
    }
}

pub fn concat(docs: impl Iterator<Item = Document>) -> Document {
    Document::Vec(docs.collect())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Document {
    /// Returns a document entity used to represent nothingness
    Nil,

    /// A mandatory linebreak
    Line(usize),

    /// Forces contained groups to break
    ForceBreak,

    /// May break contained document based on best fit, thus flex break
    FlexBreak(Box<Document>),

    /// Renders `broken` if group is broken, `unbroken` otherwise
    Break { broken: String, unbroken: String },

    /// Join multiple documents together
    Vec(Vec<Document>),

    /// Nests the given document by the given indent
    Nest(isize, Box<Document>),

    /// Nests the given document to the current cursor position
    NestCurrent(Box<Document>),

    /// Nests the given document to the current cursor position
    Group(Box<Document>),

    /// A string to render
    String(String),
}

#[derive(Debug, Clone)]
enum Mode {
    Broken,
    Unbroken,
}

fn fits(mut limit: isize, mut docs: im::Vector<(isize, Mode, Document)>) -> bool {
    loop {
        if limit < 0 {
            return false;
        };

        let (indent, mode, document) = match docs.pop_front() {
            Some(x) => x,
            None => return true,
        };

        match document {
            Document::Nil => (),

            Document::Line(_) => return true,

            Document::ForceBreak => return false,

            Document::Nest(i, doc) => docs.push_front((i + indent, mode, *doc)),

            // TODO: Remove
            Document::NestCurrent(doc) => docs.push_front((indent, mode, *doc)),

            Document::Group(doc) => docs.push_front((indent, Mode::Unbroken, *doc)),

            Document::String(s) => limit -= s.len() as isize,

            Document::Break { unbroken, .. } => match mode {
                Mode::Broken => return true,
                Mode::Unbroken => limit -= unbroken.len() as isize,
            },

            Document::FlexBreak(doc) => docs.push_front((indent, mode, *doc)),

            Document::Vec(vec) => {
                for doc in vec.into_iter().rev() {
                    docs.push_front((indent, mode.clone(), doc));
                }
            }
        }
    }
}

fn fmt(
    writer: &mut impl Utf8Writer,
    limit: isize,
    mut width: isize,
    mut docs: im::Vector<(isize, Mode, Document)>,
) -> Result<()> {
    while let Some((indent, mode, document)) = docs.pop_front() {
        match document {
            Document::Nil | Document::ForceBreak => (),

            Document::Line(i) => {
                for _ in 0..i {
                    writer.str_write("\n")?;
                }
                writer.str_write(" ".repeat(indent as usize).as_str())?;
                width = indent;
            }

            Document::Break { broken, unbroken } => {
                width = match mode {
                    Mode::Unbroken => {
                        writer.str_write(unbroken.as_str())?;
                        width + unbroken.len() as isize
                    }
                    Mode::Broken => {
                        writer.str_write(broken.as_str())?;
                        writer.str_write("\n")?;
                        writer.str_write(" ".repeat(indent as usize).as_str())?;
                        indent
                    }
                };
            }

            Document::String(s) => {
                width += s.len() as isize;
                writer.str_write(s.as_str())?;
            }

            Document::Vec(vec) => {
                for doc in vec.into_iter().rev() {
                    docs.push_front((indent, mode.clone(), doc));
                }
            }

            Document::Nest(i, doc) => {
                docs.push_front((indent + i, mode, *doc));
            }

            Document::NestCurrent(doc) => {
                docs.push_front((width, mode, *doc));
            }

            Document::Group(doc) | Document::FlexBreak(doc) => {
                // TODO: don't clone the doc
                let group_docs = im::vector![(indent, Mode::Unbroken, (*doc).clone())];
                if fits(limit - width, group_docs) {
                    docs.push_front((indent, Mode::Unbroken, *doc));
                } else {
                    docs.push_front((indent, Mode::Broken, *doc));
                }
            }
        }
    }
    Ok(())
}

pub fn nil() -> Document {
    Document::Nil
}

pub fn line() -> Document {
    Document::Line(1)
}

pub fn lines(i: usize) -> Document {
    Document::Line(i)
}

pub fn force_break() -> Document {
    Document::ForceBreak
}

pub fn break_(broken: &str, unbroken: &str) -> Document {
    Document::Break {
        broken: broken.to_string(),
        unbroken: unbroken.to_string(),
    }
}

pub fn delim(d: &str) -> Document {
    Document::Break {
        broken: d.to_string(),
        unbroken: format!("{} ", d),
    }
}

impl Document {
    pub fn group(self) -> Self {
        Self::Group(Box::new(self))
    }

    pub fn flex_break(self) -> Self {
        Self::FlexBreak(Box::new(self))
    }

    pub fn nest(self, indent: isize) -> Self {
        Self::Nest(indent, Box::new(self))
    }

    pub fn nest_current(self) -> Self {
        Self::NestCurrent(Box::new(self))
    }

    pub fn append(self, second: impl Documentable) -> Self {
        match self {
            Self::Vec(mut vec) => {
                vec.push(second.to_doc());
                Self::Vec(vec)
            }
            first => Self::Vec(vec![first, second.to_doc()]),
        }
    }

    pub fn to_pretty_string(self, limit: isize) -> String {
        let mut buffer = String::new();
        self.pretty_print(limit, &mut buffer)
            .gleam_expect("Writing to string buffer failed");
        buffer
    }

    pub fn surround(self, open: impl Documentable, closed: impl Documentable) -> Self {
        open.to_doc().append(self).append(closed)
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Document::Nil => true,
            Document::Line(_)
            | Document::ForceBreak
            | Document::Break { .. }
            | Document::Nest(_, _)
            | Document::NestCurrent(_) => false,
            Document::Vec(vec) => vec.is_empty(),
            Document::String(s) => s.is_empty(),
            Document::Group(doc) | Document::FlexBreak(doc) => doc.is_nil(),
        }
    }

    // TODO: return a result
    pub fn pretty_print(self, limit: isize, writer: &mut impl Utf8Writer) -> Result<()> {
        let docs = im::vector![(0, Mode::Unbroken, Document::Group(Box::new(self)))];
        fmt(writer, limit, 0, docs)?;
        Ok(())
    }
}
