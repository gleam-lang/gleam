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
//! - `ForcedBreak` from Elixir.
//! - `FlexBreak` from Elixir.
//!
//! The way this module works is fairly simple conceptually, however the actual
//! behaviour in practice can be hard to wrap one's head around.
//!
//! The basic premise is the `Document` type, which is a tree structure,
//! containing some text as well as information on how it can be formatted.
//! Once the document is constructed, it can be printed using the
//! `to_pretty_string` function.
//!
//! It will then traverse the tree, and construct
//! a string, attempting to wrap lines to that they do not exceed the line length
//! limit specified. Where and when it wraps lines is determined by the structure
//! of the `Document` itself.
//!
#![allow(clippy::wrong_self_convention)]

#[cfg(test)]
mod tests;

use ecow::{EcoString, eco_format};
use itertools::Itertools;
use num_bigint::BigInt;
use unicode_segmentation::UnicodeSegmentation;

use crate::{Result, io::Utf8Writer};

/// Join multiple documents together in a vector. This macro calls the `to_doc`
/// method on each element, providing a concise way to write a document sequence.
/// For example:
///
/// ```rust:norun
/// docvec!["Hello", line(), "world!"]
/// ```
///
/// Note: each document in a docvec is not separated in any way: the formatter
/// will never break a line unless a `Document::Break` or `Document::Line`
/// is used. Therefore, `docvec!["a", "b", "c"]` is equivalent to
/// `"abc".to_doc()`.
///
#[macro_export]
macro_rules! docvec {
    () => {
        Document::Vec(Vec::new())
    };

    ($($x:expr),+ $(,)?) => {
        Document::Vec(vec![$($x.to_doc()),+])
    };
}

/// Coerce a value into a Document.
/// Note we do not implement this for String as a slight pressure to favour str
/// over String.
pub trait Documentable<'a> {
    fn to_doc(self) -> Document<'a>;
}

impl<'a> Documentable<'a> for char {
    fn to_doc(self) -> Document<'a> {
        Document::eco_string(eco_format!("{self}"))
    }
}

impl<'a> Documentable<'a> for &'a str {
    fn to_doc(self) -> Document<'a> {
        Document::str(self)
    }
}

impl<'a> Documentable<'a> for EcoString {
    fn to_doc(self) -> Document<'a> {
        Document::eco_string(self)
    }
}

impl<'a> Documentable<'a> for &EcoString {
    fn to_doc(self) -> Document<'a> {
        Document::eco_string(self.clone())
    }
}

impl<'a> Documentable<'a> for isize {
    fn to_doc(self) -> Document<'a> {
        Document::eco_string(eco_format!("{self}"))
    }
}

impl<'a> Documentable<'a> for i64 {
    fn to_doc(self) -> Document<'a> {
        Document::eco_string(eco_format!("{self}"))
    }
}

impl<'a> Documentable<'a> for usize {
    fn to_doc(self) -> Document<'a> {
        Document::eco_string(eco_format!("{self}"))
    }
}

impl<'a> Documentable<'a> for f64 {
    fn to_doc(self) -> Document<'a> {
        Document::eco_string(eco_format!("{self:?}"))
    }
}

impl<'a> Documentable<'a> for u64 {
    fn to_doc(self) -> Document<'a> {
        Document::eco_string(eco_format!("{self:?}"))
    }
}

impl<'a> Documentable<'a> for u32 {
    fn to_doc(self) -> Document<'a> {
        Document::eco_string(eco_format!("{self}"))
    }
}

impl<'a> Documentable<'a> for u16 {
    fn to_doc(self) -> Document<'a> {
        Document::eco_string(eco_format!("{self}"))
    }
}

impl<'a> Documentable<'a> for u8 {
    fn to_doc(self) -> Document<'a> {
        Document::eco_string(eco_format!("{self}"))
    }
}

impl<'a> Documentable<'a> for BigInt {
    fn to_doc(self) -> Document<'a> {
        Document::eco_string(eco_format!("{self}"))
    }
}

impl<'a> Documentable<'a> for Document<'a> {
    fn to_doc(self) -> Document<'a> {
        self
    }
}

impl<'a> Documentable<'a> for Vec<Document<'a>> {
    fn to_doc(self) -> Document<'a> {
        Document::Vec(self)
    }
}

impl<'a, D: Documentable<'a>> Documentable<'a> for Option<D> {
    fn to_doc(self) -> Document<'a> {
        self.map(Documentable::to_doc).unwrap_or_else(nil)
    }
}

/// Joins an iterator into a single document, in the same way as `docvec!`.
pub fn concat<'a>(docs: impl IntoIterator<Item = Document<'a>>) -> Document<'a> {
    Document::Vec(docs.into_iter().collect())
}

/// Joins an iterator into a single document, interspersing each element with
/// another document. This is useful for example in argument lists, where a
/// list of arguments must all be separated with a comma.
pub fn join<'a>(
    docs: impl IntoIterator<Item = Document<'a>>,
    separator: Document<'a>,
) -> Document<'a> {
    concat(Itertools::intersperse(docs.into_iter(), separator))
}

/// A pretty printable document. A tree structure, made up of text and other
/// elements which determine how it can be formatted.
///
/// The variants of this enum should probably not be constructed directly,
/// rather use the helper functions of the same names to construct them.
/// For example, use `line()` instead of `Document::Line(1)`.
///
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Document<'a> {
    /// A mandatory linebreak. This is always printed as a string of newlines,
    /// equal in length to the number specified.
    Line(usize),

    /// Forces the breaks of the wrapped document to be considered as not
    /// fitting on a single line. Used in combination with a `Group` it can be
    /// used to force its `Break`s to always break.
    ForceBroken(Box<Self>),

    /// Ignore the next break, forcing it to render as unbroken.
    NextBreakFits(Box<Self>, NextBreakFitsMode),

    /// A document after which the formatter can insert a newline. This determines
    /// where line breaks can occur, outside of hardcoded `Line`s.
    /// See `break_` and `flex_break` for usage.
    Break {
        broken: &'a str,
        unbroken: &'a str,
        kind: BreakKind,
    },

    /// Join multiple documents together. The documents are not separated in any
    /// way: the formatter will only print newlines if `Document::Break` or
    /// `Document::Line` is used.
    Vec(Vec<Self>),

    /// Nests the given document by the given indent, depending on the specified
    /// condition. See `Document::nest`, `Document::set_nesting` and
    /// `Document::nest_if_broken` for usages.
    Nest(isize, NestMode, NestCondition, Box<Self>),

    /// Groups a document. When pretty printing a group, the formatter will
    /// first attempt to fit the entire group on one line. If it fails, all
    /// `break_` documents in the group will render broken.
    ///
    /// Nested groups are handled separately to their parents, so if the
    /// outermost group is broken, any sub-groups might be rendered broken
    /// or unbroken, depending on whether they fit on a single line.
    Group(Box<Self>),

    /// Renders a string slice. This will always render the string verbatim,
    /// without any line breaks or other modifications to it.
    Str {
        string: &'a str,
        /// The number of extended grapheme clusters in the string.
        /// This is what the pretty printer uses as the width of the string as it
        /// is closes to what a human would consider the "length" of a string.
        ///
        /// Since computing the number of grapheme clusters requires walking over
        /// the string we precompute it to avoid iterating through a string over
        /// and over again in the pretty printing algorithm.
        ///
        graphemes: isize,
    },

    /// Renders an `EcoString`. This will always render the string verbatim,
    /// without any line breaks or other modifications to it.
    EcoString {
        string: EcoString,
        /// The number of extended grapheme clusters in the string.
        /// This is what the pretty printer uses as the width of the string as it
        /// is closes to what a human would consider the "length" of a string.
        ///
        /// Since computing the number of grapheme clusters requires walking over
        /// the string we precompute it to avoid iterating through a string over
        /// and over again in the pretty printing algorithm.
        ///
        graphemes: isize,
    },

    /// A string that is not taken into account when determining line length.
    /// This is useful for additional formatting text which won't be rendered
    /// in the final output, such as ANSI codes or HTML elements.
    ZeroWidthString { string: EcoString },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    /// The mode used when a group doesn't fit on a single line: when `Broken`
    /// the `Break`s inside it will be rendered as newlines, splitting the
    /// group.
    Broken,

    /// The default mode used when a group can fit on a single line: all its
    /// `Break`s will be rendered as their unbroken string and kept on a single
    /// line.
    Unbroken,

    /// This mode is used by the `NextBreakFit` document to force a break to be
    /// considered as broken.
    ForcedBroken,

    /// This mode is used to disable a `NextBreakFit` document.
    ForcedUnbroken,
}

/// A flag that can be used to enable or disable a `NextBreakFit` document.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NextBreakFitsMode {
    Enabled,
    Disabled,
}

/// A flag that can be used to conditionally disable a `Nest` document.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NestCondition {
    /// This always applies the nesting. This is a sensible default that will
    /// work for most of the cases.
    Always,
    /// Only applies the nesting if the wrapping `Group` couldn't fit on a
    /// single line and has been broken.
    IfBroken,
}

/// Used to change the way nesting of documents work.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NestMode {
    /// If the nesting mode is `Increase`, the current indentation will be
    /// increased by the specified value.
    Increase,
    /// If the nesting mode is `Set`, the current indentation is going to be set
    /// to exactly the specified value.
    ///
    /// `doc.nest(2).set_nesting(0)`
    /// "wibble
    /// wobble    <- no indentation is added!
    /// wubble"
    Set,
}

fn fits(
    limit: isize,
    mut current_width: isize,
    mut docs: im::Vector<(isize, Mode, &Document<'_>)>,
) -> bool {
    // The `fits` function is going to take each document from the `docs` queue
    // and check if those can fit on a single line. In order to do so documents
    // are going to be pushed in front of this queue and have to be accompanied
    // by additional information:
    // - the document indentation, that can be increased by the `Nest` block.
    // - the current mode; this is needed to know if a group is being broken or
    //   not and treat `Break`s differently as a consequence. You can see how
    //   the behaviour changes in [ref:break-fit].
    //
    // The loop might be broken earlier without checking all documents under one
    // of two conditions:
    // - the documents exceed the line `limit` and surely won't fit
    //   [ref:document-unfit].
    // - the documents are sure to fit the line - for example, if we meet a
    //   broken `Break` [ref:break-fit] or a newline [ref:newline-fit].
    loop {
        // [tag:document-unfit] If we've exceeded the maximum width allowed for
        // a line, it means that the document won't fit on a single line, we can
        // break the loop.
        if current_width > limit {
            return false;
        };

        // We start by checking the first document of the queue. If there's no
        // documents then we can safely say that it fits (if reached this point
        // it means that the limit wasn't exceeded).
        let (indent, mode, document) = match docs.pop_front() {
            Some(x) => x,
            None => return true,
        };

        match document {
            // If a document is marked as `ForceBroken` we can immediately say
            // that it doesn't fit, so that every break is going to be
            // forcefully broken.
            Document::ForceBroken(doc) => match mode {
                // If the mode is `ForcedBroken` it means that we have to ignore
                // this break [ref:forced-broken], so we go check the inner
                // document ignoring the effects of this one.
                Mode::ForcedBroken => docs.push_front((indent, mode, doc)),
                Mode::Broken | Mode::Unbroken | Mode::ForcedUnbroken => return false,
            },

            // [tag:newline-fit] When we run into a line we know that the
            // document has a bit that fits in the current line; if it didn't
            // fit (that is, it exceeded the maximum allowed width) the loop
            // would have been broken by one of the earlier checks.
            Document::Line(_) => return true,

            // If the nesting level is increased we go on checking the wrapped
            // document and increase its indentation level based on the nesting
            // condition.
            Document::Nest(i, nest_mode, condition, doc) => match condition {
                NestCondition::IfBroken => docs.push_front((indent, mode, doc)),
                NestCondition::Always => {
                    let new_indent = match nest_mode {
                        NestMode::Increase => indent + i,
                        NestMode::Set => *i,
                    };
                    docs.push_front((new_indent, mode, doc))
                }
            },

            // As a general rule, a group fits if it can stay on a single line
            // without its breaks being broken down.
            Document::Group(doc) => match mode {
                // If an outer group was broken, we still try to fit the inner
                // group on a single line, that's why for the inner document
                // we change the mode back to `Unbroken`.
                Mode::Broken => docs.push_front((indent, Mode::Unbroken, doc)),
                // Any other mode is preserved as-is: if the mode is forced it
                // has to be left unchanged, and if the mode is already unbroken
                // there's no need to change it.
                Mode::Unbroken | Mode::ForcedBroken | Mode::ForcedUnbroken => {
                    docs.push_front((indent, mode, doc))
                }
            },

            // When we run into a string we increase the current_width; looping
            // back we will check if we've exceeded the maximum allowed width.
            Document::Str { graphemes, .. } | Document::EcoString { graphemes, .. } => {
                current_width += graphemes
            }

            // Zero width strings do nothing: they do not contribute to line length
            Document::ZeroWidthString { .. } => {}

            // If we get to a break we need to first see if it has to be
            // rendered as its unbroken or broken string, depending on the mode.
            Document::Break { unbroken, .. } => match mode {
                // [tag:break-fit] If the break has to be broken we're done!
                // We haven't exceeded the maximum length (otherwise the loop
                // iteration would have stopped with one of the earlier checks),
                // and - since it needs to be broken - we'll have to go on a new
                // line anyway.
                // This means that the document inspected so far will fit on a
                // single line, thus we return true.
                Mode::Broken | Mode::ForcedBroken => return true,
                // If the break is not broken then it will be rendered inline as
                // its unbroken string, so we treat it exactly as if it were a
                // normal string.
                Mode::Unbroken | Mode::ForcedUnbroken => current_width += unbroken.len() as isize,
            },

            // The `NextBreakFits` can alter the current mode to `ForcedBroken`
            // or `ForcedUnbroken` based on its enabled flag.
            Document::NextBreakFits(doc, enabled) => match enabled {
                // [tag:disable-next-break] If it is disabled then we check the
                // wrapped document changing the mode to `ForcedUnbroken`.
                NextBreakFitsMode::Disabled => docs.push_front((indent, Mode::ForcedUnbroken, doc)),
                NextBreakFitsMode::Enabled => match mode {
                    // If we're in `ForcedUnbroken` mode it means that the check
                    // was disabled by a document wrapping this one
                    // [ref:disable-next-break]; that's why we do nothing and
                    // check the wrapped document as if it were a normal one.
                    Mode::ForcedUnbroken => docs.push_front((indent, mode, doc)),
                    // [tag:forced-broken] Any other mode is turned into
                    // `ForcedBroken` so that when we run into a break, the
                    // response to the question "Does the document fit?" will be
                    // yes [ref:break-fit].
                    // This is why this is called `NextBreakFit` I think.
                    Mode::Broken | Mode::Unbroken | Mode::ForcedBroken => {
                        docs.push_front((indent, Mode::ForcedBroken, doc))
                    }
                },
            },

            // If there's a sequence of documents we will check each one, one
            // after the other to see if - as a whole - they can fit on a single
            // line.
            Document::Vec(vec) => {
                // The array needs to be reversed to preserve the order of the
                // documents since each one is pushed _to the front_ of the
                // queue of documents to check.
                for doc in vec.iter().rev() {
                    docs.push_front((indent, mode, doc));
                }
            }
        }
    }
}

/// The kind of line break this `Document::Break` is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BreakKind {
    /// A `flex_break`.
    Flex,
    /// A `break_`.
    Strict,
}

fn format(
    writer: &mut impl Utf8Writer,
    limit: isize,
    mut width: isize,
    mut docs: im::Vector<(isize, Mode, &Document<'_>)>,
) -> Result<()> {
    // As long as there are documents to print we'll take each one by one and
    // output the corresponding string to the given writer.
    //
    // Each document in the `docs` queue also has an accompanying indentation
    // and mode:
    // - the indentation is used to keep track of the current indentation,
    //   you might notice in [ref:format-nest] that it adds documents to the
    //   queue increasing their current indentation.
    // - the mode is used to keep track of the state of the documents inside a
    //   group. For example, if a group doesn't fit on a single line its
    //   documents will be split into multiple lines and the mode set to
    //   `Broken` to keep track of this.
    while let Some((indent, mode, document)) = docs.pop_front() {
        match document {
            // When we run into a line we print the given number of newlines and
            // add the indentation required by the given document.
            Document::Line(i) => {
                for _ in 0..*i {
                    writer.str_write("\n")?;
                }
                for _ in 0..indent {
                    writer.str_write(" ")?;
                }
                width = indent;
            }

            // Flex breaks are NOT conditional to the mode: if the mode is
            // already `Unbroken`, then the break is left unbroken (like strict
            // breaks); any other mode is ignored.
            // A flexible break will only be split if the following documents
            // can't fit on the same line; otherwise, it is just displayed as an
            // unbroken `Break`.
            Document::Break {
                broken,
                unbroken,
                kind: BreakKind::Flex,
            } => {
                let unbroken_width = width + unbroken.len() as isize;
                // Every time we need to check again if the remaining piece can
                // fit. If it does, the flexible break is not broken.
                if mode == Mode::Unbroken || fits(limit, unbroken_width, docs.clone()) {
                    writer.str_write(unbroken)?;
                    width = unbroken_width;
                } else {
                    writer.str_write(broken)?;
                    writer.str_write("\n")?;
                    for _ in 0..indent {
                        writer.str_write(" ")?;
                    }
                    width = indent;
                }
            }

            // Strict breaks are conditional to the mode. They differ from
            // flexible break because, if a group gets split - that is the mode
            // is `Broken` or `ForceBroken` - ALL of the breaks in that group
            // will be split. You can notice the difference with flexible breaks
            // because here we only check the mode and then take action; before
            // we would try and see if the remaining documents fit on a single
            // line before deciding if the (flexible) break can be split or not.
            Document::Break {
                broken,
                unbroken,
                kind: BreakKind::Strict,
            } => match mode {
                // If the mode requires the break to be broken, then its broken
                // string is printed, then we start a newline and indent it
                // according to the current indentation level.
                Mode::Broken | Mode::ForcedBroken => {
                    writer.str_write(broken)?;
                    writer.str_write("\n")?;
                    for _ in 0..indent {
                        writer.str_write(" ")?;
                    }
                    width = indent;
                }
                // If the mode doesn't require the break to be broken, then its
                // unbroken string is printed as if it were a normal string;
                // also updating the width of the current line.
                Mode::Unbroken | Mode::ForcedUnbroken => {
                    writer.str_write(unbroken)?;
                    width += unbroken.len() as isize
                }
            },

            // Strings are printed as they are and the current width is
            // increased accordingly.
            Document::EcoString { string, graphemes } => {
                width += graphemes;
                writer.str_write(string)?;
            }

            Document::Str { string, graphemes } => {
                width += graphemes;
                writer.str_write(string)?;
            }

            Document::ZeroWidthString { string } => {
                // We write the string, but do not increment the length
                writer.str_write(string)?;
            }

            // If multiple documents need to be printed, then they are all
            // pushed to the front of the queue and will be printed one by one.
            Document::Vec(vec) => {
                // Just like `fits`, the elements will be pushed _on the front_
                // of the queue. In order to keep their original order they need
                // to be pushed in reverse order.
                for doc in vec.iter().rev() {
                    docs.push_front((indent, mode, doc));
                }
            }

            // A `Nest` document doesn't result in anything being printed, its
            // only effect is to increase the current nesting level for the
            // wrapped document [tag:format-nest].
            Document::Nest(i, nest_mode, condition, doc) => match (condition, mode) {
                // The nesting is only applied under two conditions:
                // - either the nesting condition is `Always`.
                // - or the condition is `IfBroken` and the group was actually
                //   broken (that is, the current mode is `Broken`).
                (NestCondition::Always, _) | (NestCondition::IfBroken, Mode::Broken) => {
                    let new_indent = match nest_mode {
                        NestMode::Increase => indent + i,
                        NestMode::Set => *i,
                    };
                    docs.push_front((new_indent, mode, doc))
                }
                // If none of the above conditions is met, then the nesting is
                // not applied.
                _ => docs.push_front((indent, mode, doc)),
            },

            Document::Group(doc) => {
                // When we see a group we first try and see if it can fit on a
                // single line without breaking any break; that is why we use
                // the `Unbroken` mode here: we want to try to fit everything on
                // a single line.
                let group_docs = im::vector![(indent, Mode::Unbroken, doc.as_ref())];
                if fits(limit, width, group_docs) {
                    // If everything can stay on a single line we print the
                    // wrapped document with the `Unbroken` mode, leaving all
                    // the group's break as unbroken.
                    docs.push_front((indent, Mode::Unbroken, doc));
                } else {
                    // Otherwise, we need to break the group. We print the
                    // wrapped document changing its mode to `Broken` so that
                    // all its breaks will be split on newlines.
                    docs.push_front((indent, Mode::Broken, doc));
                }
            }

            // `ForceBroken` and `NextBreakFits` only change the way the `fit`
            // function works but do not actually change the formatting of a
            // document by themselves. That's why when we run into those we
            // just go on printing the wrapped document without altering the
            // current mode.
            Document::ForceBroken(document) | Document::NextBreakFits(document, _) => {
                docs.push_front((indent, mode, document));
            }
        }
    }
    Ok(())
}

/// Renders an empty document.
pub fn nil<'a>() -> Document<'a> {
    Document::Vec(vec![])
}

/// Renders a single newline.
pub fn line<'a>() -> Document<'a> {
    Document::Line(1)
}

/// Renders a string of newlines, equal in length to the number provided.
pub fn lines<'a>(i: usize) -> Document<'a> {
    Document::Line(i)
}

/// A document after which the formatter can insert a newline. This determines
/// where line breaks can occur, outside of hardcoded `Line`s.
///
/// If the formatter determines that a group cannot fit on a single line,
/// all breaks in the group will be rendered as broken. Otherwise, they
/// will be rendered as unbroken.
///
/// A broken `Break` renders the `broken` string, followed by a newline.
/// An unbroken `Break` renders the `unbroken` string by itself.
///
/// For example:
/// ```rust:norun
/// let document = docvec!["Hello", break_("", ", "), "world!"];
/// assert_eq!(document.to_pretty_string(20), "Hello, world!");
/// assert_eq!(document.to_pretty_string(10), "Hello\nworld!");
/// ```
///
pub fn break_<'a>(broken: &'a str, unbroken: &'a str) -> Document<'a> {
    Document::Break {
        broken,
        unbroken,
        kind: BreakKind::Strict,
    }
}

/// A document after which the formatter can insert a newline, similar to
/// `break_()`. The difference is that when a group is rendered broken, all
/// breaks are rendered broken. However, `flex_break` decides whether to
/// break or not for every individual `flex_break`.
///
/// For example:
/// ```rust:norun
/// let with_breaks = docvec!["Hello", break_("", ", "), "pretty", break_("", ", "), "printed!"];
/// assert_eq!(with_breaks.to_pretty_string(20), "Hello\npretty\nprinted!");
///
/// let with_flex_breaks = docvec!["Hello", flex_break("", ", "), "pretty", flex_break("", ", "), "printed!"];
/// assert_eq!(with_flex_breaks.to_pretty_string(20), "Hello, pretty\nprinted!");
/// ```
///
pub fn flex_break<'a>(broken: &'a str, unbroken: &'a str) -> Document<'a> {
    Document::Break {
        broken,
        unbroken,
        kind: BreakKind::Flex,
    }
}

/// A string that is not taken into account when determining line length.
/// This is useful for additional formatting text which won't be rendered
/// in the final output, such as ANSI codes or HTML elements.
///
/// For example:
/// ```rust:norun
/// let document = docvec!["Hello", zero_width_string("This is a very long string"), break_("", ""), "world"];
/// assert_eq!(document.to_pretty_string(20), "HelloThis is a very long stringworld");
/// ```
///
pub fn zero_width_string<'a>(string: EcoString) -> Document<'a> {
    Document::ZeroWidthString { string }
}

impl<'a> Document<'a> {
    /// Creates a document from a string slice.
    pub fn str(string: &'a str) -> Self {
        Document::Str {
            graphemes: string.graphemes(true).count() as isize,
            string,
        }
    }

    /// Creates a document from an owned `EcoString`.
    pub fn eco_string(string: EcoString) -> Self {
        Document::EcoString {
            graphemes: string.graphemes(true).count() as isize,
            string,
        }
    }

    /// Groups a document. When pretty printing a group, the formatter will
    /// first attempt to fit the entire group on one line. If it fails, all
    /// `break_` documents in the group will render broken.
    ///
    /// Nested groups are handled separately to their parents, so if the
    /// outermost group is broken, any sub-groups might be rendered broken
    /// or unbroken, depending on whether they fit on a single line.
    pub fn group(self) -> Self {
        Self::Group(Box::new(self))
    }

    /// Sets the indentation level of a document.
    pub fn set_nesting(self, indent: isize) -> Self {
        Self::Nest(indent, NestMode::Set, NestCondition::Always, Box::new(self))
    }

    /// Nests a document by a certain indentation. When rending linebreaks, the
    /// formatter will print a new line followed by the current indentation.
    pub fn nest(self, indent: isize) -> Self {
        Self::Nest(
            indent,
            NestMode::Increase,
            NestCondition::Always,
            Box::new(self),
        )
    }

    /// Nests a document by a certain indentation, but only if the current
    /// group is broken.
    pub fn nest_if_broken(self, indent: isize) -> Self {
        Self::Nest(
            indent,
            NestMode::Increase,
            NestCondition::IfBroken,
            Box::new(self),
        )
    }

    /// Forces all `break_` and `flex_break` documents in the current group
    /// to render broken.
    pub fn force_break(self) -> Self {
        Self::ForceBroken(Box::new(self))
    }

    /// Force the next `Break` to render unbroken, regardless of whether it
    /// fits on the line or not.
    pub fn next_break_fits(self, mode: NextBreakFitsMode) -> Self {
        Self::NextBreakFits(Box::new(self), mode)
    }

    /// Appends one document to another. Equivalent to `docvec![self, second]`,
    /// except that it `self` is already a `Document::Vec`, it will append
    /// directly to it instead of allocating a new vector.
    ///
    /// Useful when chaining multiple documents together in a fashion where
    /// they cannot be put all into one `docvec!` macro.
    pub fn append(self, second: impl Documentable<'a>) -> Self {
        match self {
            Self::Vec(mut vec) => {
                vec.push(second.to_doc());
                Self::Vec(vec)
            }
            Self::Line(..)
            | Self::ForceBroken(..)
            | Self::NextBreakFits(..)
            | Self::Break { .. }
            | Self::Nest(..)
            | Self::Group(..)
            | Self::Str { .. }
            | Self::EcoString { .. }
            | Self::ZeroWidthString { .. } => Self::Vec(vec![self, second.to_doc()]),
        }
    }

    /// Prints a document into a `String`, attempting to limit lines to `limit`
    /// characters in length.
    pub fn to_pretty_string(self, limit: isize) -> String {
        let mut buffer = String::new();
        self.pretty_print(limit, &mut buffer)
            .expect("Writing to string buffer failed");
        buffer
    }

    /// Surrounds a document in two delimiters. Equivalent to
    /// `docvec![option, self, closed]`.
    pub fn surround(self, open: impl Documentable<'a>, closed: impl Documentable<'a>) -> Self {
        open.to_doc().append(self).append(closed)
    }

    /// Prints a document into `writer`, attempting to limit lines to `limit`
    /// characters in length.
    pub fn pretty_print(&self, limit: isize, writer: &mut impl Utf8Writer) -> Result<()> {
        let docs = im::vector![(0, Mode::Unbroken, self)];
        format(writer, limit, 0, docs)?;
        Ok(())
    }

    /// Returns true when the document contains no printable characters
    /// (whitespace and newlines are considered printable characters).
    pub fn is_empty(&self) -> bool {
        use Document::*;
        match self {
            Line(n) => *n == 0,
            EcoString { string, .. } => string.is_empty(),
            Str { string, .. } => string.is_empty(),
            // assuming `broken` and `unbroken` are equivalent
            Break { broken, .. } => broken.is_empty(),
            ForceBroken(d) | Nest(_, _, _, d) | Group(d) | NextBreakFits(d, _) => d.is_empty(),
            Vec(docs) => docs.iter().all(|d| d.is_empty()),
            // Zero-width strings don't count towards line length, but they are
            // still printed and so are not empty. (Unless their string contents
            // is also empty)
            ZeroWidthString { string } => string.is_empty(),
        }
    }
}
