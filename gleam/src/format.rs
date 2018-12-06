#![allow(dead_code)]

// TODO: Use im vector rather than VecDeque?

use std::collections::VecDeque;

pub enum Document {
    /// Returns a document entity used to represent nothingness
    Nil,

    /// A mandatory linebreak
    Line,

    /// Forces contained groups to break
    ForceBreak,

    /// Renders `broken` if group is broken, `unbroken` otherwise
    Break { broken: String, unbroken: String },

    /// Join 2 documents together
    Cons {
        left: Box<Document>,
        right: Box<Document>,
    },

    /// Nests the given document by the given indent
    Nest {
        indent: isize,
        contents: Box<Document>,
    },

    /// Nests the given document to the current cursor position
    NestCurrent { contents: Box<Document> },

    /// Nests the given document to the current cursor position
    Group { contents: Box<Document> },
}

enum Mode {
    Broken,
    Unbroken,
}

//   defp fits?(limit, [{indent, m, doc_cons(x, y)} | t]) do
//     fits?(limit, [{indent, m, x} | [{indent, m, y} | t]])
//   end

//   # Indent is never used in `fits?/2`, why do we have clauses for it?
//   defp fits?(limit, [{indent, m, doc_nest(x, :current)} | t]) do
//     fits?(limit, [{indent, m, x} | t])
//   end

//   # Indent is never used in `fits?/2`, why do we have clauses for it?
//   defp fits?(limit, [{indent, m, doc_nest(x, i)} | t]) do
//     fits?(limit, [{indent + i, m, x} | t])
//   end

//   defp fits?(limit, [{_, _, s} | t]) when is_binary(s) do
//     fits?((limit - byte_size(s)), t)
//   end

//   defp fits?(limit, [{_, :unbroken, doc_break(s, _)} | t]) do
//     fits?((limit - byte_size(s)), t)
//   end

//   defp fits?(limit, [{indent, _, doc_group(x)} | t]) do
//     fits?(limit, [{indent, :unbroken, x} | t])
//   end

fn fits(limit: isize, mut docs: VecDeque<(usize, Mode, Document)>) -> bool {
    if limit < 0 {
        return false;
    };

    let (indent, mode, doc) = match docs.pop_front() {
        Some(triple) => triple,
        None => return true,
    };

    match doc {
        Document::Line => true,
        Document::Nil => fits(limit, docs),
        Document::Line => unimplemented!(),
        Document::ForceBreak => false,
        Document::Break { .. } => match mode {
            Mode::Broken => true,
            Mode::Unbroken => unimplemented!(),
        },
        Document::Cons { left, right } => unimplemented!(),
        Document::Nest { indent, contents } => unimplemented!(),
        Document::NestCurrent { contents } => unimplemented!(),
        Document::Group { contents } => unimplemented!(),
    }
}
