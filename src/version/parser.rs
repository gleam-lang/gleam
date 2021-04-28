// Based off of https://github.com/steveklabnik/semver-parser/blob/bee9de80aaa9653c5eb46a83658606cb21151e65/src/parser.rs

use std::fmt;
use std::mem;

use self::Error::*;
use super::{
    lexer::{self, Lexer, Token},
    requirement::Requirement,
    Comparator,
};
use crate::version::{Identifier, Operator, Version};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Error<'input> {
    /// Needed more tokens for parsing, but none are available.
    UnexpectedEnd,
    /// Unexpected token.
    UnexpectedToken(Token<'input>),
    /// An error occurred in the lexer.
    Lexer(lexer::Error),
    /// More input available.
    MoreInput(Vec<Token<'input>>),
    /// Encountered empty predicate in a set of predicates.
    EmptyPredicate,
    /// Encountered an empty range.
    EmptyRange,
}

impl<'input> From<lexer::Error> for Error<'input> {
    fn from(value: lexer::Error) -> Self {
        Error::Lexer(value)
    }
}

impl<'input> fmt::Display for Error<'input> {
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
impl<'input> From<Error<'input>> for String {
    fn from(value: Error<'input>) -> Self {
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
    pub fn new(input: &'input str) -> Result<Parser<'input>, Error<'input>> {
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
    fn pop(&mut self) -> Result<Token<'input>, Error<'input>> {
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
    fn skip_whitespace(&mut self) -> Result<(), Error<'input>> {
        match self.peek() {
            Some(&Token::Whitespace(_, _)) => self.pop().map(|_| ()),
            _ => Ok(()),
        }
    }

    /// Check that some whitespace is next and then discard it
    fn expect_whitespace(&mut self) -> Result<(), Error<'input>> {
        match self.pop()? {
            Token::Whitespace(_, _) => Ok(()),
            token => Err(UnexpectedToken(token)),
        }
    }

    // /// Parse a single component.
    // pub fn component(&mut self) -> Result<u64, Error<'input>> {
    //     match self.pop()? {
    //         Token::Numeric(number) => Ok(number),
    //         tok => Err(UnexpectedToken(tok)),
    //     }
    // }

    /// Parse a single numeric.
    pub fn numeric(&mut self) -> Result<u64, Error<'input>> {
        match self.pop()? {
            Token::Numeric(number) => Ok(number),
            tok => Err(UnexpectedToken(tok)),
        }
    }

    // /// Parse a dot, then a component.
    // pub fn dot_component(&mut self) -> Result<u64, Error<'input>> {
    //     match self.pop()? {
    //         Token::Dot => {}
    //         tok => return Err(UnexpectedToken(tok)),
    //     }
    //     self.component()
    // }

    fn dot(&mut self) -> Result<(), Error<'input>> {
        match self.pop()? {
            Token::Dot => Ok(()),
            tok => Err(UnexpectedToken(tok)),
        }
    }

    /// Parse a dot, then a numeric.
    fn dot_numeric(&mut self) -> Result<u64, Error<'input>> {
        self.dot()?;
        self.numeric()
    }

    /// Parse an string identifier.
    ///
    /// Like, `foo`, or `bar`, or `beta-1`.
    pub fn identifier(&mut self) -> Result<Identifier, Error<'input>> {
        let identifier = match self.pop()? {
            Token::AlphaNumeric(identifier) => {
                // TODO: Borrow?
                Identifier::AlphaNumeric(identifier.to_string())
            }
            Token::Numeric(n) => Identifier::Numeric(n),
            tok => return Err(UnexpectedToken(tok)),
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
    fn pre(&mut self) -> Result<Vec<Identifier>, Error<'input>> {
        match self.peek() {
            Some(&Token::Hyphen) => {}
            _ => return Ok(vec![]),
        }

        // pop the peeked hyphen.
        self.pop()?;
        self.parts()
    }

    /// Parse a dot-separated set of identifiers.
    fn parts(&mut self) -> Result<Vec<Identifier>, Error<'input>> {
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
    fn plus_build_metadata(&mut self) -> Result<Option<String>, Error<'input>> {
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
                Ok(token) => return Err(UnexpectedToken(token)),
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
    pub fn version(&mut self) -> Result<Version, Error<'input>> {
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

    /// Parse a requirement.
    ///
    /// Like, `~> 1.0.0` or `3.0.0-beta.1 or < 1.0 and > 0.2.3`.
    pub fn requirement(&mut self) -> Result<Requirement, Error<'input>> {
        let alternatives = self.requirement_alternatives()?;
        self.skip_whitespace()?;
        Ok(Requirement(alternatives))
    }

    fn requirement_alternatives(&mut self) -> Result<Vec<Vec<Comparator>>, Error<'input>> {
        let mut alternatives = Vec::new();

        loop {
            alternatives.push(self.requirement_comparators()?);
            if self.peek() == Some(&Token::Or) {
                self.pop()?;
                self.expect_whitespace()?;
            } else {
                break;
            }
        }

        if alternatives.is_empty() {
            Err(UnexpectedEnd)
        } else {
            Ok(alternatives)
        }
    }

    fn basic_comparison_operator(&mut self) -> Result<Operator, Error<'input>> {
        match self.pop()? {
            Token::Eq => Ok(Operator::Eq),
            Token::NotEq => Ok(Operator::NotEq),
            Token::Gt => Ok(Operator::Gt),
            Token::Lt => Ok(Operator::Lt),
            Token::LtEq => Ok(Operator::LtEq),
            Token::GtEq => Ok(Operator::GtEq),

            token => Err(UnexpectedToken(token)),
        }
    }

    fn pessimistic_version_constraint(
        &mut self,
    ) -> Result<(Comparator, Comparator), Error<'input>> {
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
        let lower = Comparator {
            operator: Operator::GtEq,
            version: lower,
        };
        let upper = Comparator {
            operator: Operator::Lt,
            version: upper,
        };
        Ok((lower, upper))
    }

    fn requirement_comparators(&mut self) -> Result<Vec<Comparator>, Error<'input>> {
        use Token::*;
        let mut comparators = Vec::new();
        let comp = |operator, version| Comparator { operator, version };
        loop {
            self.skip_whitespace()?;
            match self.peek() {
                None => break,

                Some(Numeric(_)) => comparators.push(comp(Operator::Eq, self.version()?)),

                Some(Lt) | Some(LtEq) | Some(GtEq) | Some(Eq) | Some(Gt) | Some(NotEq) => {
                    comparators.push(comp(self.basic_comparison_operator()?, self.version()?))
                }

                Some(Pessimistic) => {
                    self.pop()?;
                    self.skip_whitespace()?;
                    let (lower, upper) = self.pessimistic_version_constraint()?;
                    comparators.push(lower);
                    comparators.push(upper);
                }

                Some(_) => return Err(UnexpectedToken(self.pop()?)),
            };
            if self.peek() == Some(&Token::And) {
                self.pop()?;
                self.expect_whitespace()?;
            } else {
                break;
            }
        }
        if comparators.is_empty() {
            Err(UnexpectedEnd)
        } else {
            Ok(comparators)
        }
    }

    /// Check if we have reached the end of input.
    pub fn is_eof(&mut self) -> bool {
        self.c1.is_none()
    }

    /// Get the rest of the tokens in the parser.
    ///
    /// Useful for debugging.
    pub fn tail(&mut self) -> Result<Vec<Token<'input>>, Error<'input>> {
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
