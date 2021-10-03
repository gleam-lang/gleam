// Based off of https://github.com/steveklabnik/semver-parser/blob/bee9de80aaa9653c5eb46a83658606cb21151e65/src/parser.rs

use std::fmt;
use std::mem;

use self::Error::*;
use super::lexer::{self, Lexer, Token};
use crate::version::{Identifier, Version};
use thiserror::Error;

type PubgrubRange = pubgrub::range::Range<Version>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Error)]
pub enum Error {
    /// Needed more tokens for parsing, but none are available.
    UnexpectedEnd,
    /// Unexpected token.
    UnexpectedToken(String),
    /// An error occurred in the lexer.
    Lexer(lexer::Error),
    /// More input available.
    MoreInput(String),
    /// Encountered empty predicate in a set of predicates.
    EmptyPredicate,
    /// Encountered an empty range.
    EmptyRange,
}

impl<'input> From<lexer::Error> for Error {
    fn from(value: lexer::Error) -> Self {
        Error::Lexer(value)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;

        match *self {
            UnexpectedEnd => write!(fmt, "expected more input"),
            UnexpectedToken(ref token) => write!(fmt, "encountered unexpected token: {:?}", token),
            Lexer(ref error) => write!(fmt, "lexer error: {:?}", error),
            MoreInput(ref tokens) => write!(fmt, "expected end of input, but got: {:?}", tokens),
            EmptyPredicate => write!(fmt, "encountered empty predicate"),
            EmptyRange => write!(fmt, "encountered empty range"),
        }
    }
}

/// impl for backwards compatibility.
impl From<Error> for String {
    fn from(value: Error) -> Self {
        value.to_string()
    }
}

/// A recursive-descent parser for parsing version requirements.
pub struct Parser<'input> {
    /// Source of token.
    lexer: Lexer<'input>,
    /// Lookaehead.
    c1: Option<Token<'input>>,
}

impl<'input> Parser<'input> {
    /// Construct a new parser for the given input.
    pub fn new(input: &'input str) -> Result<Parser<'input>, Error> {
        let mut lexer = Lexer::new(input);

        let c1 = if let Some(c1) = lexer.next() {
            Some(c1?)
        } else {
            None
        };

        Ok(Parser { lexer, c1 })
    }

    /// Pop one token.
    #[inline(always)]
    fn pop(&mut self) -> Result<Token<'input>, Error> {
        let c1 = if let Some(c1) = self.lexer.next() {
            Some(c1?)
        } else {
            None
        };

        mem::replace(&mut self.c1, c1).ok_or_else(|| UnexpectedEnd)
    }

    /// Peek one token.
    #[inline(always)]
    fn peek(&mut self) -> Option<&Token<'input>> {
        self.c1.as_ref()
    }

    /// Skip whitespace if present.
    fn skip_whitespace(&mut self) -> Result<(), Error> {
        match self.peek() {
            Some(&Token::Whitespace(_, _)) => self.pop().map(|_| ()),
            _ => Ok(()),
        }
    }

    /// Check that some whitespace is next and then discard it
    fn expect_whitespace(&mut self) -> Result<(), Error> {
        match self.pop()? {
            Token::Whitespace(_, _) => Ok(()),
            token => Err(UnexpectedToken(token.to_string())),
        }
    }

    /// Parse a single numeric.
    pub fn numeric(&mut self) -> Result<u32, Error> {
        match self.pop()? {
            Token::Numeric(number) => Ok(number),
            token => Err(UnexpectedToken(token.to_string())),
        }
    }

    fn dot(&mut self) -> Result<(), Error> {
        match self.pop()? {
            Token::Dot => Ok(()),
            token => Err(UnexpectedToken(token.to_string())),
        }
    }

    /// Parse a dot, then a numeric.
    fn dot_numeric(&mut self) -> Result<u32, Error> {
        self.dot()?;
        self.numeric()
    }

    /// Parse an string identifier.
    ///
    /// Like, `foo`, or `bar`, or `beta-1`.
    pub fn identifier(&mut self) -> Result<Identifier, Error> {
        let identifier = match self.pop()? {
            Token::AlphaNumeric(identifier) => {
                // TODO: Borrow?
                Identifier::AlphaNumeric(identifier.to_string())
            }
            Token::Numeric(n) => Identifier::Numeric(n),
            tok => return Err(UnexpectedToken(tok.to_string())),
        };

        if let Some(&Token::Hyphen) = self.peek() {
            // pop the peeked hyphen
            self.pop()?;
            // concat with any following identifiers
            Ok(identifier
                .concat("-")
                .concat(&self.identifier()?.to_string()))
        } else {
            Ok(identifier)
        }
    }

    /// Parse all pre-release identifiers, separated by dots.
    ///
    /// Like, `abcdef.1234`.
    fn pre(&mut self) -> Result<Vec<Identifier>, Error> {
        match self.peek() {
            Some(&Token::Hyphen) => {}
            _ => return Ok(vec![]),
        }

        // pop the peeked hyphen.
        self.pop()?;
        self.parts()
    }

    /// Parse a dot-separated set of identifiers.
    fn parts(&mut self) -> Result<Vec<Identifier>, Error> {
        let mut parts = Vec::new();

        parts.push(self.identifier()?);

        while let Some(&Token::Dot) = self.peek() {
            self.pop()?;

            parts.push(self.identifier()?);
        }

        Ok(parts)
    }

    /// Parse optional build metadata.
    ///
    /// Like, `` (empty), or `+abcdef`.
    fn plus_build_metadata(&mut self) -> Result<Option<String>, Error> {
        match self.peek() {
            Some(&Token::Plus) => self.pop()?,
            _ => return Ok(None),
        };

        let mut buffer = String::new();

        loop {
            match self.pop() {
                Err(UnexpectedEnd) => break,
                Ok(Token::LeadingZero(s)) => buffer.push_str(s),
                Ok(Token::AlphaNumeric(s)) => buffer.push_str(s),
                Ok(Token::Numeric(s)) => buffer.push_str(&s.to_string()),
                Ok(Token::Dot) => buffer.push('.'),
                Ok(token) => return Err(UnexpectedToken(token.to_string())),
                Err(error) => return Err(error),
            }
        }

        if buffer.is_empty() {
            Err(UnexpectedEnd)
        } else {
            Ok(Some(buffer))
        }
    }

    /// Parse a version.
    ///
    /// Like, `1.0.0` or `3.0.0-beta.1`.
    pub fn version(&mut self) -> Result<Version, Error> {
        self.skip_whitespace()?;

        let major = self.numeric()?;
        let minor = self.dot_numeric()?;
        let patch = self.dot_numeric()?;
        let pre = self.pre()?;
        let build = self.plus_build_metadata()?;

        self.skip_whitespace()?;

        Ok(Version {
            major,
            minor,
            patch,
            pre,
            build,
        })
    }

    /// Parse a version range requirement.
    ///
    /// Like, `~> 1.0.0` or `3.0.0-beta.1 or < 1.0 and > 0.2.3`.
    pub fn range(&mut self) -> Result<PubgrubRange, Error> {
        let mut range: Option<PubgrubRange> = None;

        loop {
            let constraint = self.range_ands_section()?;
            range = Some(match range {
                None => constraint,
                Some(range) => range.union(&constraint),
            });
            if self.peek() == Some(&Token::Or) {
                self.pop()?;
                self.expect_whitespace()?;
            } else {
                break;
            }
        }

        self.skip_whitespace()?;
        range.ok_or(UnexpectedEnd)
    }

    fn pessimistic_version_constraint(&mut self) -> Result<PubgrubRange, Error> {
        let mut included_patch = false;
        let major = self.numeric()?;
        let minor = self.dot_numeric()?;
        let patch = match self.peek() {
            Some(Token::Dot) => {
                included_patch = true;
                self.pop()?;
                self.numeric()?
            }
            _ => 0,
        };
        let pre = self.pre()?;
        let build = self.plus_build_metadata()?;

        let lower = Version {
            major,
            minor,
            patch,
            pre,
            build,
        };
        let upper = if included_patch {
            lower.bump_minor()
        } else {
            lower.bump_major()
        };
        Ok(
            PubgrubRange::higher_than(lower)
                .intersection(&PubgrubRange::strictly_lower_than(upper)),
        )
    }

    fn range_ands_section(&mut self) -> Result<PubgrubRange, Error> {
        use Token::*;
        let mut range = None;
        let and = |range: Option<PubgrubRange>, constraint: PubgrubRange| {
            Some(match range {
                None => constraint,
                Some(range) => range.intersection(&constraint),
            })
        };
        loop {
            self.skip_whitespace()?;
            match self.peek() {
                None => break,
                Some(Numeric(_)) => range = and(range, PubgrubRange::exact(self.version()?)),

                Some(Eq) => {
                    self.pop()?;
                    range = and(range, PubgrubRange::exact(self.version()?));
                }

                Some(NotEq) => {
                    self.pop()?;
                    let version = self.version()?;
                    let bumped = version.bump_patch();
                    let below = PubgrubRange::strictly_lower_than(version);
                    let above = PubgrubRange::higher_than(bumped);
                    range = and(range, below.union(&above));
                }

                Some(Gt) => {
                    self.pop()?;
                    range = and(
                        range,
                        PubgrubRange::higher_than(self.version()?.bump_patch()),
                    );
                }

                Some(GtEq) => {
                    self.pop()?;
                    range = and(range, PubgrubRange::higher_than(self.version()?));
                }

                Some(Lt) => {
                    self.pop()?;
                    range = and(range, PubgrubRange::strictly_lower_than(self.version()?));
                }

                Some(LtEq) => {
                    self.pop()?;
                    range = and(
                        range,
                        PubgrubRange::strictly_lower_than(self.version()?.bump_patch()),
                    );
                }

                Some(Pessimistic) => {
                    self.pop()?;
                    self.skip_whitespace()?;
                    range = and(range, self.pessimistic_version_constraint()?);
                }

                Some(_) => return Err(UnexpectedToken(self.pop()?.to_string())),
            };
            if self.peek() == Some(&Token::And) {
                self.pop()?;
                self.expect_whitespace()?;
            } else {
                break;
            }
        }
        range.ok_or(UnexpectedEnd)
    }

    /// Check if we have reached the end of input.
    pub fn is_eof(&mut self) -> bool {
        self.c1.is_none()
    }

    /// Get the rest of the tokens in the parser.
    ///
    /// Useful for debugging.
    pub fn tail(&mut self) -> Result<Vec<Token<'input>>, Error> {
        let mut out = Vec::new();

        if let Some(t) = self.c1.take() {
            out.push(t);
        }

        while let Some(t) = self.lexer.next() {
            out.push(t?);
        }

        Ok(out)
    }
}
