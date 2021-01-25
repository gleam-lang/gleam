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

use crate::wrap;
use crate::{fs::Utf8Writer, GleamExpect, Result};

macro_rules! docvec {
    () => {
        Document::Vec(Vec::new())
    };

    ($($x:expr),+ $(,)?) => {
        Document::Vec(vec![$($x.into_doc()),+])
    };
}

/// Coerce a value into a Document.
/// Note we do not implement this for String as a slight pressure to favour str
/// over String.
pub trait Documentable<'a> {
    fn into_doc(self) -> Document<'a>;
}

impl<'a> Documentable<'a> for &'a str {
    fn into_doc(self) -> Document<'a> {
        Document::Str(self)
    }
}

impl<'a> Documentable<'a> for isize {
    fn into_doc(self) -> Document<'a> {
        Document::String(format!("{}", self))
    }
}

impl<'a> Documentable<'a> for i64 {
    fn into_doc(self) -> Document<'a> {
        Document::String(format!("{}", self))
    }
}

impl<'a> Documentable<'a> for usize {
    fn into_doc(self) -> Document<'a> {
        Document::String(format!("{}", self))
    }
}

impl<'a> Documentable<'a> for f64 {
    fn into_doc(self) -> Document<'a> {
        Document::String(format!("{:?}", self))
    }
}

impl<'a> Documentable<'a> for u64 {
    fn into_doc(self) -> Document<'a> {
        Document::String(format!("{:?}", self))
    }
}

impl<'a> Documentable<'a> for Document<'a> {
    fn into_doc(self) -> Document<'a> {
        self
    }
}

impl<'a> Documentable<'a> for Vec<Document<'a>> {
    fn into_doc(self) -> Document<'a> {
        Document::Vec(self)
    }
}

impl<'a, D: Documentable<'a>> Documentable<'a> for Option<D> {
    fn into_doc(self) -> Document<'a> {
        match self {
            Some(d) => d.into_doc(),
            None => Document::Nil,
        }
    }
}

pub fn concat<'a>(docs: impl Iterator<Item = Document<'a>>) -> Document<'a> {
    Document::Vec(docs.collect())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Document<'a> {
    /// Returns a document entity used to represent nothingness
    Nil,

    /// A mandatory linebreak
    Line(usize),

    /// Forces contained groups to break
    ForceBreak,

    /// May break contained document based on best fit, thus flex break
    FlexBreak(Box<Self>),

    /// Renders `broken` if group is broken, `unbroken` otherwise
    // TODO: str not string
    Break { broken: &'a str, unbroken: &'a str },

    /// Join multiple documents together
    Vec(Vec<Self>),

    /// Nests the given document by the given indent
    Nest(isize, Box<Self>),

    /// Nests the given document to the current cursor position
    NestCurrent(Box<Self>),

    /// Nests the given document to the current cursor position
    Group(Box<Self>),

    /// A string to render
    String(String),

    /// A str to render
    Str(&'a str),
}

#[derive(Debug, Clone)]
enum Mode {
    Broken,
    Unbroken,
}

fn fits(mut limit: isize, mut docs: im::Vector<(isize, Mode, Document<'_>)>) -> bool {
    loop {
        if limit < 0 {
            return false;
        };

        let (indent, mode, document) = if let Some(x) = docs.pop_front() {
            x
        } else {
            return true;
        };

        match document {
            Document::Nil => (),

            Document::Line(_) => return true,

            Document::ForceBreak => return false,

            Document::Nest(i, doc) => docs.push_front((i + indent, mode, *doc)),

            // TODO: Remove
            Document::NestCurrent(doc) | Document::FlexBreak(doc) => {
                docs.push_front((indent, mode, *doc))
            }

            Document::Group(doc) => docs.push_front((indent, Mode::Unbroken, *doc)),

            Document::Str(s) => limit -= wrap!(s.len(), isize),
            Document::String(s) => limit -= wrap!(s.len(), isize),

            Document::Break { unbroken, .. } => match mode {
                Mode::Broken => return true,
                Mode::Unbroken => limit -= wrap!(unbroken.len(), isize),
            },

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
    mut docs: im::Vector<(isize, Mode, Document<'_>)>,
) -> Result<()> {
    while let Some((indent, mode, document)) = docs.pop_front() {
        match document {
            Document::Nil | Document::ForceBreak => (),

            Document::Line(i) => {
                for _ in 0..i {
                    writer.str_write("\n")?;
                }
                for _ in 0..indent {
                    writer.str_write(" ")?;
                }
                width = indent;
            }

            Document::Break { broken, unbroken } => {
                width = match mode {
                    Mode::Unbroken => {
                        writer.str_write(unbroken)?;
                        width + wrap!(unbroken.len(), isize)
                    }
                    Mode::Broken => {
                        writer.str_write(broken)?;
                        writer.str_write("\n")?;
                        for _ in 0..indent {
                            writer.str_write(" ")?;
                        }
                        indent
                    }
                };
            }

            Document::String(s) => {
                width += wrap!(s.len(), isize);
                writer.str_write(s.as_str())?;
            }

            Document::Str(s) => {
                width += wrap!(s.len(), isize);
                writer.str_write(s)?;
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

pub fn nil<'a>() -> Document<'a> {
    Document::Nil
}

pub fn line<'a>() -> Document<'a> {
    Document::Line(1)
}

pub fn lines<'a>(i: usize) -> Document<'a> {
    Document::Line(i)
}

pub fn force_break<'a>() -> Document<'a> {
    Document::ForceBreak
}

pub fn break_<'a>(broken: &'a str, unbroken: &'a str) -> Document<'a> {
    Document::Break { broken, unbroken }
}

impl<'a> Document<'a> {
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

    pub fn append(self, second: impl Documentable<'a>) -> Self {
        match self {
            Self::Vec(mut vec) => {
                vec.push(second.into_doc());
                Self::Vec(vec)
            }
            first => Self::Vec(vec![first, second.into_doc()]),
        }
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn to_pretty_string(self, limit: isize) -> String {
        let mut buffer = String::new();
        self.pretty_print(limit, &mut buffer)
            .gleam_expect("Writing to string buffer failed");
        buffer
    }

    pub fn surround(self, open: impl Documentable<'a>, closed: impl Documentable<'a>) -> Self {
        open.into_doc().append(self).append(closed)
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
            Document::Str(s) => s.is_empty(),
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
