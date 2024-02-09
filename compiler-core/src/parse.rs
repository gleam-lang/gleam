// Gleam Parser
//
// Terminology:
//   Expression Unit:
//     Essentially a thing that goes between operators.
//     Int, Bool, function call, "{" expression-sequence "}", case x {}, ..etc
//
//   Expression:
//     One or more Expression Units separated by an operator
//
//   Binding:
//     (let|let assert|use) name (:TypeAnnotation)? = Expression
//
//   Expression Sequence:
//     * One or more Expressions
//     * A Binding followed by at least one more Expression Sequences
//
// Naming Conventions:
//   parse_x
//      Parse a specific part of the grammar, not erroring if it cannot.
//      Generally returns `Result<Option<A>, ParseError>`, note the inner Option
//
//   expect_x
//      Parse a generic or specific part of the grammar, erroring if it cannot.
//      Generally returns `Result<A, ParseError>`, note no inner Option
//
//   maybe_x
//      Parse a generic part of the grammar. Returning `None` if it cannot.
//      Returns `Some(x)` and advances the token stream if it can.
//
// Operator Precedence Parsing:
//   Needs to take place in expressions and in clause guards.
//   It is accomplished using the Simple Precedence Parser algorithm.
//   See: https://en.wikipedia.org/wiki/Simple_precedence_parser
//
//   It relies or the operator grammar being in the general form:
//   e ::= expr op expr | expr
//   Which just means that exprs and operators always alternate, starting with an expr
//
//   The gist of the algorithm is:
//   Create 2 stacks, one to hold expressions, and one to hold un-reduced operators.
//   While consuming the input stream, if an expression is encountered add it to the top
//   of the expression stack. If an operator is encountered, compare its precedence to the
//   top of the operator stack and perform the appropriate action, which is either using an
//   operator to reduce 2 expressions on the top of the expression stack or put it on the top
//   of the operator stack. When the end of the input is reached, attempt to reduce all of the
//   expressions down to a single expression(or no expression) using the remaining operators
//   on the operator stack. If there are any operators left, or more than 1 expression left
//   this is a syntax error. But the implementation here shouldn't need to handle that case
//   as the outer parser ensures the correct structure.
//
pub mod error;
pub mod extra;
pub mod lexer;
mod token;

use crate::analyse::Inferred;
use crate::ast::{
    Arg, ArgNames, AssignName, Assignment, AssignmentKind, BinOp, BitArrayOption, BitArraySegment,
    CallArg, Clause, ClauseGuard, Constant, CustomType, Definition, Function, HasLocation, Import,
    Module, ModuleConstant, Pattern, RecordConstructor, RecordConstructorArg, RecordUpdateSpread,
    SrcSpan, Statement, TargetedDefinition, TodoKind, TypeAlias, TypeAst, TypeAstConstructor,
    TypeAstFn, TypeAstHole, TypeAstTuple, TypeAstVar, UnqualifiedImport, UntypedArg, UntypedClause,
    UntypedClauseGuard, UntypedConstant, UntypedDefinition, UntypedExpr, UntypedModule,
    UntypedPattern, UntypedRecordUpdateArg, UntypedStatement, Use, UseAssignment, CAPTURE_VARIABLE,
};
use crate::build::Target;
use crate::parse::extra::ModuleExtra;
use crate::type_::expression::Implementations;
use crate::type_::Deprecation;
use ecow::EcoString;
use error::{LexicalError, ParseError, ParseErrorType};
use lexer::{LexResult, Spanned};
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::str::FromStr;
use token::Token;
use vec1::{vec1, Vec1};

#[cfg(test)]
mod tests;

#[derive(Debug)]
pub struct Parsed {
    pub module: UntypedModule,
    pub extra: ModuleExtra,
}

#[derive(Debug, Default)]
struct Attributes {
    target: Option<Target>,
    deprecated: Deprecation,
    external_erlang: Option<(EcoString, EcoString)>,
    external_javascript: Option<(EcoString, EcoString)>,
}

impl Attributes {
    fn has_function_only(&self) -> bool {
        self.external_erlang.is_some() || self.external_javascript.is_some()
    }
}

//
// Public Interface
//
pub fn parse_module(src: &str) -> Result<Parsed, ParseError> {
    let lex = lexer::make_tokenizer(src);
    let mut parser = Parser::new(lex);
    let mut parsed = parser.parse_module()?;
    parsed.extra = parser.extra;
    Ok(parsed)
}

//
// Test Interface
//
#[cfg(test)]
pub fn parse_statement_sequence(src: &str) -> Result<Vec1<UntypedStatement>, ParseError> {
    let lex = lexer::make_tokenizer(src);
    let mut parser = Parser::new(lex);
    let expr = parser.parse_statement_seq();
    let expr = parser.ensure_no_errors_or_remaining_input(expr)?;
    if let Some((e, _)) = expr {
        Ok(e)
    } else {
        parse_error(ParseErrorType::ExpectedExpr, SrcSpan { start: 0, end: 0 })
    }
}

//
// Test Interface
//
#[cfg(test)]
pub fn parse_const_value(src: &str) -> Result<Constant<(), ()>, ParseError> {
    let lex = lexer::make_tokenizer(src);
    let mut parser = Parser::new(lex);
    let expr = parser.parse_const_value();
    let expr = parser.ensure_no_errors_or_remaining_input(expr)?;
    if let Some(e) = expr {
        Ok(e)
    } else {
        parse_error(ParseErrorType::ExpectedExpr, SrcSpan { start: 0, end: 0 })
    }
}

//
// Parser
//
#[derive(Debug)]
pub struct Parser<T: Iterator<Item = LexResult>> {
    tokens: T,
    lex_errors: Vec<LexicalError>,
    tok0: Option<Spanned>,
    tok1: Option<Spanned>,
    extra: ModuleExtra,
    doc_comments: VecDeque<(u32, String)>,
}
impl<T> Parser<T>
where
    T: Iterator<Item = LexResult>,
{
    pub fn new(input: T) -> Self {
        let mut parser = Parser {
            tokens: input,
            lex_errors: vec![],
            tok0: None,
            tok1: None,
            extra: ModuleExtra::new(),
            doc_comments: VecDeque::new(),
        };
        let _ = parser.next_tok();
        let _ = parser.next_tok();
        parser
    }

    fn parse_module(&mut self) -> Result<Parsed, ParseError> {
        let definitions = Parser::series_of(self, &Parser::parse_definition, None);
        let definitions = self.ensure_no_errors_or_remaining_input(definitions)?;
        let module = Module {
            name: "".into(),
            documentation: vec![],
            type_info: (),
            definitions,
        };
        Ok(Parsed {
            module,
            extra: Default::default(),
        })
    }

    // The way the parser is currently implemented, it cannot exit immediately while advancing
    // the token stream upon seeing a LexError. That is to avoid having to put `?` all over the
    // place and instead we collect LexErrors in `self.lex_errors` and attempt to continue parsing.
    // Once parsing has returned we want to surface an error in the order:
    // 1) LexError, 2) ParseError, 3) More Tokens Left
    fn ensure_no_errors_or_remaining_input<A>(
        &mut self,
        parse_result: Result<A, ParseError>,
    ) -> Result<A, ParseError> {
        let parse_result = self.ensure_no_errors(parse_result)?;
        if let Some((start, _, end)) = self.next_tok() {
            // there are still more tokens
            let expected = vec!["An import, const, type, if block, or function.".into()];
            return parse_error(
                ParseErrorType::UnexpectedToken {
                    expected,
                    hint: None,
                },
                SrcSpan { start, end },
            );
        }
        // no errors
        Ok(parse_result)
    }

    // The way the parser is currently implemented, it cannot exit immediately
    // while advancing the token stream upon seeing a LexError. That is to avoid
    // having to put `?` all over the place and instead we collect LexErrors in
    // `self.lex_errors` and attempt to continue parsing.
    // Once parsing has returned we want to surface an error in the order:
    // 1) LexError, 2) ParseError
    fn ensure_no_errors<A>(
        &mut self,
        parse_result: Result<A, ParseError>,
    ) -> Result<A, ParseError> {
        if let Some(error) = self.lex_errors.first() {
            // Lex errors first
            let location = error.location;
            let error = error.clone();
            parse_error(ParseErrorType::LexError { error }, location)
        } else {
            // Return any existing parse error
            parse_result
        }
    }

    fn parse_definition(&mut self) -> Result<Option<TargetedDefinition>, ParseError> {
        let mut attributes = Attributes::default();
        let location = self.parse_attributes(&mut attributes)?;

        let def = match (self.tok0.take(), self.tok1.as_ref()) {
            // Imports
            (Some((start, Token::Import, _)), _) => {
                self.advance();
                self.parse_import(start)
            }
            // Module Constants
            (Some((_, Token::Const, _)), _) => {
                self.advance();
                self.parse_module_const(false, &attributes)
            }
            (Some((_, Token::Pub, _)), Some((_, Token::Const, _))) => {
                self.advance();
                self.advance();
                self.parse_module_const(true, &attributes)
            }

            // Function
            (Some((start, Token::Fn, _)), _) => {
                self.advance();
                self.parse_function(start, false, false, &mut attributes)
            }
            (Some((start, Token::Pub, _)), Some((_, Token::Fn, _))) => {
                self.advance();
                self.advance();
                self.parse_function(start, true, false, &mut attributes)
            }

            // Custom Types, and Type Aliases
            (Some((start, Token::Type, _)), _) => {
                self.advance();
                self.parse_custom_type(start, false, false, &mut attributes)
            }
            (Some((start, Token::Pub, _)), Some((_, Token::Opaque, _))) => {
                self.advance();
                self.advance();
                let _ = self.expect_one(&Token::Type)?;
                self.parse_custom_type(start, true, true, &mut attributes)
            }
            (Some((start, Token::Pub, _)), Some((_, Token::Type, _))) => {
                self.advance();
                self.advance();
                self.parse_custom_type(start, true, false, &mut attributes)
            }

            (t0, _) => {
                self.tok0 = t0;
                Ok(None)
            }
        }?;

        match (def, location) {
            (Some(definition), _) if definition.is_function() => Ok(Some(TargetedDefinition {
                definition,
                target: attributes.target,
            })),

            (Some(definition), None) => Ok(Some(TargetedDefinition {
                definition,
                target: attributes.target,
            })),

            (_, Some(location)) if attributes.has_function_only() => {
                parse_error(ParseErrorType::ExpectedFunctionDefinition, location)
            }

            (Some(definition), _) => Ok(Some(TargetedDefinition {
                definition,
                target: attributes.target,
            })),

            (_, Some(location)) => parse_error(ParseErrorType::ExpectedDefinition, location),

            (None, None) => Ok(None),
        }
    }

    //
    // Parse Expressions
    //

    // examples:
    //   unit
    //   unit op unit
    //   unit op unit pipe unit(call)
    //   unit op unit pipe unit(call) pipe unit(call)
    fn parse_expression(&mut self) -> Result<Option<UntypedExpr>, ParseError> {
        // uses the simple operator parser algorithm
        let mut opstack = vec![];
        let mut estack = vec![];
        let mut last_op_start = 0;
        let mut last_op_end = 0;
        loop {
            match self.parse_expression_unit()? {
                Some(unit) => estack.push(unit),
                _ if estack.is_empty() => return Ok(None),
                _ => {
                    return parse_error(
                        ParseErrorType::OpNakedRight,
                        SrcSpan {
                            start: last_op_start,
                            end: last_op_end,
                        },
                    );
                }
            }

            if let Some((op_s, t, op_e)) = self.tok0.take() {
                if let Some(p) = precedence(&t) {
                    // Is Op
                    self.advance();
                    last_op_start = op_s;
                    last_op_end = op_e;
                    let _ = handle_op(
                        Some(((op_s, t, op_e), p)),
                        &mut opstack,
                        &mut estack,
                        &do_reduce_expression,
                    );
                } else {
                    // Is not Op
                    self.tok0 = Some((op_s, t, op_e));
                    break;
                }
            } else {
                break;
            }
        }

        Ok(handle_op(
            None,
            &mut opstack,
            &mut estack,
            &do_reduce_expression,
        ))
    }

    fn parse_expression_unit_collapsing_single_value_blocks(
        &mut self,
    ) -> Result<Option<UntypedExpr>, ParseError> {
        match self.parse_expression_unit()? {
            Some(expression) => Ok(Some(expression)),
            None => Ok(None),
        }
    }

    // examples:
    //   1
    //   "one"
    //   True
    //   fn() { "hi" }
    //   unit().unit().unit()
    //   A(a.., label: tuple(1))
    //   { expression_sequence }
    fn parse_expression_unit(&mut self) -> Result<Option<UntypedExpr>, ParseError> {
        let mut expr = match self.tok0.take() {
            Some((start, Token::String { value }, end)) => {
                self.advance();
                UntypedExpr::String {
                    location: SrcSpan { start, end },
                    value,
                }
            }
            Some((start, Token::Int { value }, end)) => {
                self.advance();
                UntypedExpr::Int {
                    location: SrcSpan { start, end },
                    value,
                }
            }

            Some((start, Token::Float { value }, end)) => {
                self.advance();
                UntypedExpr::Float {
                    location: SrcSpan { start, end },
                    value,
                }
            }

            // var lower_name and UpName
            Some((start, Token::Name { name } | Token::UpName { name }, end)) => {
                self.advance();
                UntypedExpr::Var {
                    location: SrcSpan { start, end },
                    name,
                }
            }

            Some((start, Token::Todo, mut end)) => {
                self.advance();
                let mut message = None;
                if self.maybe_one(&Token::As).is_some() {
                    let msg_expr = self.expect_expression_unit()?;
                    end = msg_expr.location().end;
                    message = Some(Box::new(msg_expr));
                }
                UntypedExpr::Todo {
                    location: SrcSpan { start, end },
                    kind: TodoKind::Keyword,
                    message,
                }
            }

            Some((start, Token::Panic, mut end)) => {
                self.advance();
                let mut label = None;
                if self.maybe_one(&Token::As).is_some() {
                    let msg_expr = self.expect_expression_unit()?;
                    end = msg_expr.location().end;
                    label = Some(Box::new(msg_expr));
                }
                UntypedExpr::Panic {
                    location: SrcSpan { start, end },
                    message: label,
                }
            }

            Some((start, Token::Hash, _)) => {
                self.advance();
                let _ = self.expect_one(&Token::LeftParen)?;
                let elems =
                    Parser::series_of(self, &Parser::parse_expression, Some(&Token::Comma))?;
                let (_, end) = self.expect_one(&Token::RightParen)?;
                UntypedExpr::Tuple {
                    location: SrcSpan { start, end },
                    elems,
                }
            }

            // list
            Some((start, Token::LeftSquare, _)) => {
                self.advance();
                let elements =
                    Parser::series_of(self, &Parser::parse_expression, Some(&Token::Comma))?;

                // Parse an optional tail
                let mut spread_given = false;
                let mut tail = None;
                if self.maybe_one(&Token::DotDot).is_some() {
                    spread_given = true;
                    tail = self.parse_expression()?.map(Box::new);
                    let _ = self.maybe_one(&Token::Comma);
                }
                let (_, end) = self.expect_one(&Token::RightSquare)?;

                // Return errors for malformed lists
                if spread_given && tail.is_none() {
                    return parse_error(
                        ParseErrorType::ListSpreadWithoutTail,
                        SrcSpan {
                            start: end - 1,
                            end,
                        },
                    );
                }
                if tail.is_some() && elements.is_empty() {
                    return parse_error(
                        ParseErrorType::ListSpreadWithoutElements,
                        SrcSpan { start, end },
                    );
                }

                UntypedExpr::List {
                    location: SrcSpan { start, end },
                    elements,
                    tail,
                }
            }

            // BitArray
            Some((start, Token::LtLt, _)) => {
                self.advance();
                let segments = Parser::series_of(
                    self,
                    &|s| {
                        Parser::parse_bit_array_segment(
                            s,
                            &Parser::parse_expression_unit_collapsing_single_value_blocks,
                            &Parser::expect_expression,
                            &bit_array_expr_int,
                        )
                    },
                    Some(&Token::Comma),
                )?;
                let (_, end) = self.expect_one(&Token::GtGt)?;
                UntypedExpr::BitArray {
                    location: SrcSpan { start, end },
                    segments,
                }
            }
            Some((start, Token::Fn, _)) => {
                self.advance();
                let mut attributes = Attributes::default();
                match self.parse_function(start, false, true, &mut attributes)? {
                    Some(Definition::Function(Function {
                        location,
                        arguments: args,
                        body,
                        return_annotation,
                        end_position,
                        ..
                    })) => UntypedExpr::Fn {
                        location: SrcSpan::new(location.start, end_position),
                        is_capture: false,
                        arguments: args,
                        body,
                        return_annotation,
                    },

                    _ => {
                        // this isn't just none, it could also be Some(UntypedExpr::..)
                        return self.next_tok_unexpected(vec!["An opening parenthesis.".into()]);
                    }
                }
            }

            // expression block  "{" "}"
            Some((start, Token::LeftBrace, _)) => {
                self.advance();
                self.parse_block(start)?
            }

            // case
            Some((start, Token::Case, case_e)) => {
                self.advance();
                let subjects =
                    Parser::series_of(self, &Parser::parse_expression, Some(&Token::Comma))?;
                let _ = self.expect_one(&Token::LeftBrace)?;
                let clauses = Parser::series_of(self, &Parser::parse_case_clause, None)?;
                let (_, end) = self.expect_one(&Token::RightBrace)?;
                if subjects.is_empty() {
                    return parse_error(
                        ParseErrorType::ExpectedExpr,
                        SrcSpan { start, end: case_e },
                    );
                } else if clauses.is_empty() {
                    return parse_error(ParseErrorType::NoCaseClause, SrcSpan { start, end });
                } else {
                    UntypedExpr::Case {
                        location: SrcSpan { start, end },
                        subjects,
                        clauses,
                    }
                }
            }

            // helpful error on possibly trying to group with ""
            Some((start, Token::LeftParen, _)) => {
                return parse_error(ParseErrorType::ExprLparStart, SrcSpan { start, end: start });
            }

            // Boolean negation
            Some((start, Token::Bang, _end)) => {
                self.advance();
                match self.parse_expression_unit()? {
                    Some(value) => UntypedExpr::NegateBool {
                        location: SrcSpan {
                            start,
                            end: value.location().end,
                        },
                        value: Box::from(value),
                    },
                    None => {
                        return parse_error(
                            ParseErrorType::ExpectedExpr,
                            SrcSpan { start, end: start },
                        )
                    }
                }
            }

            // Int negation
            Some((start, Token::Minus, _end)) => {
                self.advance();
                match self.parse_expression_unit()? {
                    Some(value) => UntypedExpr::NegateInt {
                        location: SrcSpan {
                            start,
                            end: value.location().end,
                        },
                        value: Box::from(value),
                    },
                    None => {
                        return parse_error(
                            ParseErrorType::ExpectedExpr,
                            SrcSpan { start, end: start },
                        )
                    }
                }
            }

            // if it reaches this code block, there must be no "let" or "assert" at the beginning of the expression
            Some((start, Token::Equal, end)) => {
                return parse_error(ParseErrorType::NoLetBinding, SrcSpan { start, end })
            }

            Some((start, Token::Colon, end)) => {
                return parse_error(ParseErrorType::NoLetBinding, SrcSpan { start, end })
            }

            t0 => {
                self.tok0 = t0;
                return Ok(None);
            }
        };

        // field access and call can stack up
        loop {
            if let Some((dot_start, _)) = self.maybe_one(&Token::Dot) {
                let start = expr.location().start;
                // field access
                match self.tok0.take() {
                    // tuple access
                    Some((_, Token::Int { value }, end)) => {
                        self.advance();
                        let v = value.replace("_", "");
                        if let Ok(index) = u64::from_str(&v) {
                            expr = UntypedExpr::TupleIndex {
                                location: SrcSpan { start, end },
                                index,
                                tuple: Box::new(expr),
                            }
                        } else {
                            return parse_error(
                                ParseErrorType::InvalidTupleAccess,
                                SrcSpan { start, end },
                            );
                        }
                    }

                    Some((_, Token::Name { name: label }, end)) => {
                        self.advance();
                        expr = UntypedExpr::FieldAccess {
                            location: SrcSpan { start, end },
                            label_location: SrcSpan {
                                start: dot_start,
                                end,
                            },
                            label,
                            container: Box::new(expr),
                        }
                    }

                    Some((_, Token::UpName { name: label }, end)) => {
                        self.advance();
                        expr = UntypedExpr::FieldAccess {
                            location: SrcSpan { start, end },
                            label_location: SrcSpan {
                                start: dot_start,
                                end,
                            },
                            label,
                            container: Box::new(expr),
                        }
                    }

                    t0 => {
                        self.tok0 = t0;
                        return self.next_tok_unexpected(vec![
                            "A positive integer or a field name.".into(),
                        ]);
                    }
                }
            } else if self.maybe_one(&Token::LeftParen).is_some() {
                let start = expr.location().start;
                if let Some((dot_s, _)) = self.maybe_one(&Token::DotDot) {
                    // Record update
                    let base = self.expect_expression()?;
                    let base_e = base.location().end;
                    let spread = RecordUpdateSpread {
                        base: Box::new(base),
                        location: SrcSpan {
                            start: dot_s,
                            end: base_e,
                        },
                    };
                    let mut args = vec![];
                    if self.maybe_one(&Token::Comma).is_some() {
                        args = Parser::series_of(
                            self,
                            &Parser::parse_record_update_arg,
                            Some(&Token::Comma),
                        )?;
                    }
                    let (_, end) = self.expect_one(&Token::RightParen)?;

                    expr = UntypedExpr::RecordUpdate {
                        location: SrcSpan { start, end },
                        constructor: Box::new(expr),
                        spread,
                        arguments: args,
                    };
                } else {
                    // Call
                    let args = self.parse_fn_args()?;
                    let (_, end) = self.expect_one(&Token::RightParen)?;
                    expr = make_call(expr, args, start, end)?;
                }
            } else {
                // done
                break;
            }
        }

        Ok(Some(expr))
    }

    // A `use` expression
    // use <- function
    // use <- function()
    // use <- function(a, b)
    // use <- module.function(a, b)
    // use a, b, c <- function(a, b)
    // use a, b, c, <- function(a, b)
    fn parse_use(&mut self, start: u32) -> Result<UntypedStatement, ParseError> {
        let assignments = if let Some((_, Token::LArrow, _)) = self.tok0 {
            vec![]
        } else {
            Parser::series_of(self, &Parser::parse_use_assignment, Some(&Token::Comma))?
        };

        _ = self.expect_one(&Token::LArrow)?;
        let call = self.expect_expression()?;

        Ok(Statement::Use(Use {
            location: SrcSpan::new(start, call.location().end),
            assignments,
            call: Box::new(call),
        }))
    }

    fn parse_use_assignment(&mut self) -> Result<Option<UseAssignment>, ParseError> {
        let start = self.tok0.as_ref().map(|t| t.0).unwrap_or(0);

        let pattern = self.parse_pattern()?.ok_or_else(|| ParseError {
            error: ParseErrorType::ExpectedPattern,
            location: SrcSpan { start, end: start },
        })?;

        let annotation = self.parse_type_annotation(&Token::Colon)?;
        let end = match annotation {
            Some(ref a) => a.location().end,
            None => pattern.location().end,
        };

        Ok(Some(UseAssignment {
            location: SrcSpan { start, end },
            pattern,
            annotation,
        }))
    }

    // An assignment, with `Let` already consumed
    fn parse_assignment(&mut self, start: u32) -> Result<UntypedStatement, ParseError> {
        let kind = if let Some((_, Token::Assert, _)) = self.tok0 {
            _ = self.next_tok();
            AssignmentKind::Assert
        } else {
            AssignmentKind::Let
        };
        let pattern = if let Some(p) = self.parse_pattern()? {
            p
        } else {
            // DUPE: 62884
            return self.next_tok_unexpected(vec!["A pattern".into()])?;
        };
        let annotation = self.parse_type_annotation(&Token::Colon)?;
        let (eq_s, eq_e) = self.maybe_one(&Token::Equal).ok_or(ParseError {
            error: ParseErrorType::ExpectedEqual,
            location: SrcSpan {
                start: pattern.location().start,
                end: pattern.location().end,
            },
        })?;
        let value = self.parse_expression()?.ok_or(ParseError {
            error: ParseErrorType::ExpectedValue,
            location: SrcSpan {
                start: eq_s,
                end: eq_e,
            },
        })?;
        Ok(Statement::Assignment(Assignment {
            location: SrcSpan {
                start,
                end: value.location().end,
            },
            value: Box::new(value),
            pattern,
            annotation,
            kind,
        }))
    }

    // examples:
    //   expr
    //   expr expr..
    //   expr assignment..
    //   assignment
    //   assignment expr..
    //   assignment assignment..
    fn parse_statement_seq(&mut self) -> Result<Option<(Vec1<UntypedStatement>, u32)>, ParseError> {
        let mut statements = vec![];
        let mut start = None;
        let mut end = 0;

        // Try and parse as many expressions as possible
        while let Some(statement) = self.parse_statement()? {
            if start.is_none() {
                start = Some(statement.location().start);
            }
            end = statement.location().end;
            statements.push(statement);
        }

        match Vec1::try_from_vec(statements) {
            Ok(statements) => Ok(Some((statements, end))),
            Err(_) => Ok(None),
        }
    }

    fn parse_statement(&mut self) -> Result<Option<UntypedStatement>, ParseError> {
        match self.tok0.take() {
            Some((start, Token::Use, _)) => {
                self.advance();
                Ok(Some(self.parse_use(start)?))
            }

            Some((start, Token::Let, _)) => {
                self.advance();
                Ok(Some(self.parse_assignment(start)?))
            }

            token => {
                self.tok0 = token;
                let expression = self.parse_expression()?.map(Statement::Expression);
                Ok(expression)
            }
        }
    }

    fn parse_block(&mut self, start: u32) -> Result<UntypedExpr, ParseError> {
        let body = self.parse_statement_seq()?;
        let (_, end) = self.expect_one(&Token::RightBrace)?;
        let location = SrcSpan { start, end };
        match body {
            None => parse_error(ParseErrorType::NoExpression, SrcSpan { start, end }),
            Some((statements, _)) => Ok(UntypedExpr::Block {
                statements,
                location,
            }),
        }
    }

    // The left side of an "=" or a "->"
    fn parse_pattern(&mut self) -> Result<Option<UntypedPattern>, ParseError> {
        let pattern = match self.tok0.take() {
            // Pattern::Var or Pattern::Constructor start
            Some((start, Token::Name { name }, end)) => {
                self.advance();

                // A variable is not permitted on the left hand side of a `<>`
                if let Some((_, Token::LtGt, _)) = self.tok0.as_ref() {
                    return concat_pattern_variable_left_hand_side_error(start, end);
                }

                if self.maybe_one(&Token::Dot).is_some() {
                    self.expect_constructor_pattern(Some((start, name, end)))?
                } else {
                    match name.as_str() {
                        "true" | "false" => {
                            return parse_error(
                                ParseErrorType::LowcaseBooleanPattern,
                                SrcSpan { start, end },
                            )
                        }
                        _ => Pattern::Variable {
                            location: SrcSpan { start, end },
                            name,
                            type_: (),
                        },
                    }
                }
            }
            // Constructor
            Some((start, tok @ Token::UpName { .. }, end)) => {
                self.tok0 = Some((start, tok, end));
                self.expect_constructor_pattern(None)?
            }

            Some((start, Token::DiscardName { name }, end)) => {
                self.advance();

                // A discard is not permitted on the left hand side of a `<>`
                if let Some((_, Token::LtGt, _)) = self.tok0.as_ref() {
                    return concat_pattern_variable_left_hand_side_error(start, end);
                }

                Pattern::Discard {
                    location: SrcSpan { start, end },
                    name,
                    type_: (),
                }
            }

            Some((start, Token::String { value }, end)) => {
                self.advance();

                match self.tok0 {
                    // String matching with assignment, it could either be a
                    // String prefix matching: "Hello, " as greeting <> name -> ...
                    // or a full string matching: "Hello, World!" as greeting -> ...
                    Some((_, Token::As, _)) => {
                        self.advance();
                        let (name_start, name, name_end) = self.expect_name()?;
                        let name_span = SrcSpan {
                            start: name_start,
                            end: name_end,
                        };

                        match self.tok0 {
                            // String prefix matching with assignment
                            // "Hello, " as greeting <> name -> ...
                            Some((_, Token::LtGt, _)) => {
                                self.advance();
                                let (r_start, right, r_end) = self.expect_assign_name()?;
                                Pattern::StringPrefix {
                                    location: SrcSpan { start, end: r_end },
                                    left_location: SrcSpan {
                                        start,
                                        end: name_end,
                                    },
                                    right_location: SrcSpan {
                                        start: r_start,
                                        end: r_end,
                                    },
                                    left_side_string: value,
                                    left_side_assignment: Some((name, name_span)),
                                    right_side_assignment: right,
                                }
                            }
                            // Full string matching with assignment
                            _ => {
                                return Ok(Some(Pattern::Assign {
                                    name,
                                    location: name_span,
                                    pattern: Box::new(Pattern::String {
                                        location: SrcSpan { start, end },
                                        value,
                                    }),
                                }));
                            }
                        }
                    }

                    // String prefix matching with no left side assignment
                    // "Hello, " <> name -> ...
                    Some((_, Token::LtGt, _)) => {
                        self.advance();
                        let (r_start, right, r_end) = self.expect_assign_name()?;
                        Pattern::StringPrefix {
                            location: SrcSpan { start, end: r_end },
                            left_location: SrcSpan { start, end },
                            right_location: SrcSpan {
                                start: r_start,
                                end: r_end,
                            },
                            left_side_string: value,
                            left_side_assignment: None,
                            right_side_assignment: right,
                        }
                    }

                    // Full string matching
                    // "Hello, World!" -> ...
                    _ => Pattern::String {
                        location: SrcSpan { start, end },
                        value,
                    },
                }
            }
            Some((start, Token::Int { value }, end)) => {
                self.advance();
                Pattern::Int {
                    location: SrcSpan { start, end },
                    value,
                }
            }
            Some((start, Token::Float { value }, end)) => {
                self.advance();
                Pattern::Float {
                    location: SrcSpan { start, end },
                    value,
                }
            }
            Some((start, Token::Hash, _)) => {
                self.advance();
                let _ = self.expect_one(&Token::LeftParen)?;
                let elems = Parser::series_of(self, &Parser::parse_pattern, Some(&Token::Comma))?;
                let (_, end) = self.expect_one(&Token::RightParen)?;
                Pattern::Tuple {
                    location: SrcSpan { start, end },
                    elems,
                }
            }
            // BitArray
            Some((start, Token::LtLt, _)) => {
                self.advance();
                let segments = Parser::series_of(
                    self,
                    &|s| {
                        Parser::parse_bit_array_segment(
                            s,
                            &|s| match Parser::parse_pattern(s) {
                                Ok(Some(Pattern::BitArray { location, .. })) => {
                                    parse_error(ParseErrorType::NestedBitArrayPattern, location)
                                }
                                x => x,
                            },
                            &Parser::expect_bit_array_pattern_segment_arg,
                            &bit_array_pattern_int,
                        )
                    },
                    Some(&Token::Comma),
                )?;
                let (_, end) = self.expect_one(&Token::GtGt)?;
                Pattern::BitArray {
                    location: SrcSpan { start, end },
                    segments,
                }
            }
            // List
            Some((start, Token::LeftSquare, _)) => {
                self.advance();
                let elements =
                    Parser::series_of(self, &Parser::parse_pattern, Some(&Token::Comma))?;
                let tail = if let Some((_, Token::DotDot, _)) = self.tok0 {
                    self.advance();
                    let pat = self.parse_pattern()?;
                    let _ = self.maybe_one(&Token::Comma);
                    Some(pat)
                } else {
                    None
                };
                let (end, rsqb_e) = self.expect_one(&Token::RightSquare)?;
                let tail = match tail {
                    // There is a tail and it has a Pattern::Var or Pattern::Discard
                    Some(Some(pat @ (Pattern::Variable { .. } | Pattern::Discard { .. }))) => {
                        Some(pat)
                    }
                    // There is a tail and but it has no content, implicit discard
                    Some(Some(pat)) => {
                        return parse_error(ParseErrorType::InvalidTailPattern, pat.location())
                    }
                    Some(None) => Some(Pattern::Discard {
                        location: SrcSpan {
                            start: rsqb_e - 1,
                            end: rsqb_e,
                        },
                        name: "_".into(),
                        type_: (),
                    }),
                    // No tail specified
                    None => None,
                };

                Pattern::List {
                    location: SrcSpan { start, end },
                    elements,
                    tail: tail.map(Box::new),
                    type_: (),
                }
            }

            // No pattern
            t0 => {
                self.tok0 = t0;
                return Ok(None);
            }
        };

        if let Some((_, Token::As, _)) = self.tok0 {
            self.advance();
            let (start, name, end) = self.expect_name()?;
            Ok(Some(Pattern::Assign {
                name,
                location: SrcSpan { start, end },
                pattern: Box::new(pattern),
            }))
        } else {
            Ok(Some(pattern))
        }
    }

    // examples:
    //   pattern -> expr
    //   pattern, pattern if -> expr
    //   pattern, pattern | pattern, pattern if -> expr
    fn parse_case_clause(&mut self) -> Result<Option<UntypedClause>, ParseError> {
        let patterns = self.parse_patterns()?;
        if let Some(lead) = &patterns.first() {
            let mut alternative_patterns = vec![];
            loop {
                if self.maybe_one(&Token::Vbar).is_none() {
                    break;
                }
                alternative_patterns.push(self.parse_patterns()?);
            }
            let guard = self.parse_case_clause_guard(false)?;
            let (arr_s, arr_e) = self.expect_one(&Token::RArrow).map_err(|mut e| {
                if let ParseErrorType::UnexpectedToken { ref mut hint, .. } = e.error {
                    *hint =
                        Some("Did you mean to wrap a multi line clause in curly braces?".into());
                }
                e
            })?;
            let then = self.parse_expression()?;
            if let Some(then) = then {
                Ok(Some(Clause {
                    location: SrcSpan {
                        start: lead.location().start,
                        end: then.location().end,
                    },
                    pattern: patterns,
                    alternative_patterns,
                    guard,
                    then,
                }))
            } else {
                parse_error(
                    ParseErrorType::ExpectedExpr,
                    SrcSpan {
                        start: arr_s,
                        end: arr_e,
                    },
                )
            }
        } else {
            Ok(None)
        }
    }
    fn parse_patterns(&mut self) -> Result<Vec<UntypedPattern>, ParseError> {
        Parser::series_of(self, &Parser::parse_pattern, Some(&Token::Comma))
    }

    // examples:
    //   if a
    //   if a < b
    //   if a < b || b < c
    fn parse_case_clause_guard(
        &mut self,
        nested: bool,
    ) -> Result<Option<UntypedClauseGuard>, ParseError> {
        if self.maybe_one(&Token::If).is_some() || nested {
            let mut opstack = vec![];
            let mut estack = vec![];
            let mut last_op_start = 0;
            let mut last_op_end = 0;
            loop {
                if let Some(unit) = self.parse_case_clause_guard_unit()? {
                    estack.push(unit)
                } else if estack.is_empty() {
                    return Ok(None);
                } else {
                    return parse_error(
                        ParseErrorType::OpNakedRight,
                        SrcSpan {
                            start: last_op_start,
                            end: last_op_end,
                        },
                    );
                }

                if let Some((op_s, t, op_e)) = self.tok0.take() {
                    if let Some(p) = t.guard_precedence() {
                        // Is Op
                        self.advance();
                        last_op_start = op_s;
                        last_op_end = op_e;
                        let _ = handle_op(
                            Some(((op_s, t, op_e), p)),
                            &mut opstack,
                            &mut estack,
                            &do_reduce_clause_guard,
                        );
                    } else {
                        // Is not Op
                        self.tok0 = Some((op_s, t, op_e));
                        break;
                    }
                }
            }

            Ok(handle_op(
                None,
                &mut opstack,
                &mut estack,
                &do_reduce_clause_guard,
            ))
        } else {
            Ok(None)
        }
    }

    // examples
    // a
    // 1
    // a.1
    // { a }
    // a || b
    // a < b || b < c
    fn parse_case_clause_guard_unit(&mut self) -> Result<Option<UntypedClauseGuard>, ParseError> {
        match self.tok0.take() {
            Some((start, Token::Bang, _)) => {
                self.advance();
                match self.parse_case_clause_guard_unit()? {
                    Some(unit) => Ok(Some(ClauseGuard::Not {
                        location: SrcSpan {
                            start,
                            end: unit.location().end,
                        },
                        expression: Box::new(unit),
                    })),
                    None => {
                        parse_error(ParseErrorType::ExpectedValue, SrcSpan { start, end: start })
                    }
                }
            }

            Some((start, Token::Name { name }, end)) => {
                self.advance();
                let mut unit = ClauseGuard::Var {
                    location: SrcSpan { start, end },
                    type_: (),
                    name,
                };

                loop {
                    let dot_s = match self.maybe_one(&Token::Dot) {
                        Some((dot_s, _)) => dot_s,
                        None => return Ok(Some(unit)),
                    };

                    match self.next_tok() {
                        Some((_, Token::Int { value }, int_e)) => {
                            let v = value.replace("_", "");
                            if let Ok(index) = u64::from_str(&v) {
                                unit = ClauseGuard::TupleIndex {
                                    location: SrcSpan {
                                        start: dot_s,
                                        end: int_e,
                                    },
                                    index,
                                    type_: (),
                                    tuple: Box::new(unit),
                                };
                            } else {
                                return parse_error(
                                    ParseErrorType::InvalidTupleAccess,
                                    SrcSpan { start, end },
                                );
                            }
                        }

                        Some((_, Token::Name { name: label }, int_e)) => {
                            unit = ClauseGuard::FieldAccess {
                                location: SrcSpan {
                                    start: dot_s,
                                    end: int_e,
                                },
                                index: None,
                                label,
                                type_: (),
                                container: Box::new(unit),
                            };
                        }

                        Some((start, _, end)) => {
                            return parse_error(
                                ParseErrorType::IncorrectName,
                                SrcSpan { start, end },
                            )
                        }

                        _ => return self.next_tok_unexpected(vec!["A positive integer".into()]),
                    }
                }
            }
            Some((_, Token::LeftBrace, _)) => {
                // Nested guard expression
                self.advance();
                let guard = self.parse_case_clause_guard(true);
                let _ = self.expect_one(&Token::RightBrace)?;
                guard
            }
            t0 => {
                self.tok0 = t0;
                if let Some(const_val) = self.parse_const_value()? {
                    // Constant
                    Ok(Some(ClauseGuard::Constant(const_val)))
                } else {
                    Ok(None)
                }
            }
        }
    }

    // examples:
    //   UpName( args )
    fn expect_constructor_pattern(
        &mut self,
        module: Option<(u32, EcoString, u32)>,
    ) -> Result<UntypedPattern, ParseError> {
        let (mut start, name, end) = self.expect_upname()?;
        let (args, with_spread, end) = self.parse_constructor_pattern_args(end)?;
        if let Some((s, _, _)) = module {
            start = s;
        }
        Ok(Pattern::Constructor {
            location: SrcSpan { start, end },
            arguments: args,
            module: module.map(|(_, n, _)| n),
            name,
            with_spread,
            constructor: Inferred::Unknown,
            type_: (),
        })
    }

    // examples:
    //   ( args )
    fn parse_constructor_pattern_args(
        &mut self,
        upname_end: u32,
    ) -> Result<(Vec<CallArg<UntypedPattern>>, bool, u32), ParseError> {
        if self.maybe_one(&Token::LeftParen).is_some() {
            let args = Parser::series_of(
                self,
                &Parser::parse_constructor_pattern_arg,
                Some(&Token::Comma),
            )?;
            let with_spread = self.maybe_one(&Token::DotDot).is_some();
            if with_spread {
                let _ = self.maybe_one(&Token::Comma);
            }
            let (_, end) = self.expect_one(&Token::RightParen)?;
            Ok((args, with_spread, end))
        } else {
            Ok((vec![], false, upname_end))
        }
    }

    // examples:
    //   a: <pattern>
    //   <pattern>
    fn parse_constructor_pattern_arg(
        &mut self,
    ) -> Result<Option<CallArg<UntypedPattern>>, ParseError> {
        match (self.tok0.take(), self.tok1.take()) {
            // named arg
            (Some((start, Token::Name { name }, _)), Some((col_s, Token::Colon, col_e))) => {
                self.advance();
                self.advance();
                if let Some(value) = self.parse_pattern()? {
                    Ok(Some(CallArg {
                        implicit: false,
                        location: SrcSpan {
                            start,
                            end: value.location().end,
                        },
                        label: Some(name),
                        value,
                    }))
                } else {
                    parse_error(
                        ParseErrorType::ExpectedPattern,
                        SrcSpan {
                            start: col_s,
                            end: col_e,
                        },
                    )
                }
            }
            // unnamed arg
            (t0, t1) => {
                self.tok0 = t0;
                self.tok1 = t1;
                if let Some(value) = self.parse_pattern()? {
                    Ok(Some(CallArg {
                        implicit: false,
                        location: value.location(),
                        label: None,
                        value,
                    }))
                } else {
                    Ok(None)
                }
            }
        }
    }

    // examples:
    //   a: expr
    fn parse_record_update_arg(&mut self) -> Result<Option<UntypedRecordUpdateArg>, ParseError> {
        if let Some((start, label, _)) = self.maybe_name() {
            let _ = self.expect_one(&Token::Colon)?;
            let value = self.parse_expression()?;
            if let Some(value) = value {
                Ok(Some(UntypedRecordUpdateArg {
                    label,
                    location: SrcSpan {
                        start,
                        end: value.location().end,
                    },
                    value,
                }))
            } else {
                self.next_tok_unexpected(vec!["An expression".into()])
            }
        } else {
            Ok(None)
        }
    }

    //
    // Parse Functions
    //

    // Starts after "fn"
    //
    // examples:
    //   fn a(name: String) -> String { .. }
    //   pub fn a(name name: String) -> String { .. }
    fn parse_function(
        &mut self,
        start: u32,
        public: bool,
        is_anon: bool,
        attributes: &mut Attributes,
    ) -> Result<Option<UntypedDefinition>, ParseError> {
        let documentation = if is_anon {
            None
        } else {
            self.take_documentation(start)
        };
        let mut name = EcoString::from("");
        if !is_anon {
            let (_, n, _) = self.expect_name()?;
            name = n;
        }
        let _ = self.expect_one(&Token::LeftParen)?;
        let args = Parser::series_of(
            self,
            &|parser| Parser::parse_fn_param(parser, is_anon),
            Some(&Token::Comma),
        )?;
        let (_, rpar_e) = self.expect_one(&Token::RightParen)?;
        let return_annotation = self.parse_type_annotation(&Token::RArrow)?;

        let (body, end, end_position) = match self.maybe_one(&Token::LeftBrace) {
            Some(_) => {
                let some_body = self.parse_statement_seq()?;
                let (_, rbr_e) = self.expect_one(&Token::RightBrace)?;
                let end = return_annotation
                    .as_ref()
                    .map(|l| l.location().end)
                    .unwrap_or_else(|| if is_anon { rbr_e } else { rpar_e });
                let body = match some_body {
                    None => vec1![Statement::Expression(UntypedExpr::Todo {
                        kind: TodoKind::EmptyFunction,
                        location: SrcSpan { start, end },
                        message: None,
                    })],
                    Some((body, _)) => body,
                };

                (body, end, rbr_e)
            }

            None if is_anon => {
                return parse_error(
                    ParseErrorType::ExpectedFunctionBody,
                    SrcSpan { start, end: rpar_e },
                );
            }

            None => {
                let body = vec1![Statement::Expression(UntypedExpr::Placeholder {
                    location: SrcSpan::new(start, rpar_e)
                })];
                (body, rpar_e, rpar_e)
            }
        };

        Ok(Some(Definition::Function(Function {
            documentation,
            location: SrcSpan { start, end },
            end_position,
            public,
            name,
            arguments: args,
            body,
            return_type: (),
            return_annotation,
            deprecation: std::mem::take(&mut attributes.deprecated),
            external_erlang: attributes.external_erlang.take(),
            external_javascript: attributes.external_javascript.take(),
            implementations: Implementations {
                gleam: true,
                uses_erlang_externals: false,
                uses_javascript_externals: false,
            },
        })))
    }

    // Parse a single function definition param
    //
    // examples:
    //   _
    //   a
    //   a a
    //   a _
    //   a _:A
    //   a a:A
    fn parse_fn_param(&mut self, is_anon: bool) -> Result<Option<UntypedArg>, ParseError> {
        let (start, names, mut end) = match (self.tok0.take(), self.tok1.take()) {
            // labeled discard
            (
                Some((start, Token::Name { name: label }, tok0_end)),
                Some((_, Token::DiscardName { name }, end)),
            ) => {
                if is_anon {
                    return parse_error(
                        ParseErrorType::UnexpectedLabel,
                        SrcSpan {
                            start,
                            end: tok0_end,
                        },
                    );
                }

                self.advance();
                self.advance();
                (start, ArgNames::LabelledDiscard { name, label }, end)
            }
            // discard
            (Some((start, Token::DiscardName { name }, end)), t1) => {
                self.tok1 = t1;
                self.advance();
                (start, ArgNames::Discard { name }, end)
            }
            // labeled name
            (
                Some((start, Token::Name { name: label }, tok0_end)),
                Some((_, Token::Name { name }, end)),
            ) => {
                if is_anon {
                    return parse_error(
                        ParseErrorType::UnexpectedLabel,
                        SrcSpan {
                            start,
                            end: tok0_end,
                        },
                    );
                }

                self.advance();
                self.advance();
                (start, ArgNames::NamedLabelled { name, label }, end)
            }
            // name
            (Some((start, Token::Name { name }, end)), t1) => {
                self.tok1 = t1;
                self.advance();
                (start, ArgNames::Named { name }, end)
            }
            (t0, t1) => {
                self.tok0 = t0;
                self.tok1 = t1;
                return Ok(None);
            }
        };
        let annotation = if let Some(a) = self.parse_type_annotation(&Token::Colon)? {
            end = a.location().end;
            Some(a)
        } else {
            None
        };
        Ok(Some(Arg {
            location: SrcSpan { start, end },
            type_: (),
            names,
            annotation,
        }))
    }

    // Parse function call arguments, no parens
    //
    // examples:
    //   _
    //   expr, expr
    //   a: _, expr
    //   a: expr, _, b: _
    fn parse_fn_args(&mut self) -> Result<Vec<ParserArg>, ParseError> {
        let args = Parser::series_of(self, &Parser::parse_fn_arg, Some(&Token::Comma))?;
        Ok(args)
    }

    // Parse a single function call arg
    //
    // examples:
    //   _
    //   expr
    //   a: _
    //   a: expr
    fn parse_fn_arg(&mut self) -> Result<Option<ParserArg>, ParseError> {
        let mut start = 0;
        let label = match (self.tok0.take(), &self.tok1) {
            (Some((s, Token::Name { name }, _)), Some((_, Token::Colon, _))) => {
                self.advance();
                self.advance();
                start = s;
                Some(name)
            }
            (t0, _) => {
                self.tok0 = t0;
                None
            }
        };

        if let Some(value) = self.parse_expression()? {
            let mut location = value.location();
            if label.is_some() {
                location.start = start
            };
            Ok(Some(ParserArg::Arg(Box::new(CallArg {
                implicit: false,
                label,
                location,
                value,
            }))))
        } else if let Some((start, name, end)) = self.maybe_discard_name() {
            let mut location = SrcSpan { start, end };
            if label.is_some() {
                location.start = start
            };
            Ok(Some(ParserArg::Hole {
                location,
                name,
                label,
            }))
        } else {
            Ok(None)
        }
    }

    //
    // Parse Custom Types
    //

    // examples:
    //   type A { A }
    //   type A { A(String) }
    //   type Box(inner_type) { Box(inner: inner_type) }
    //   type NamedBox(inner_type) { Box(String, inner: inner_type) }
    fn parse_custom_type(
        &mut self,
        start: u32,
        public: bool,
        opaque: bool,
        attributes: &mut Attributes,
    ) -> Result<Option<UntypedDefinition>, ParseError> {
        let documentation = self.take_documentation(start);
        let (_, name, parameters, end) = self.expect_type_name()?;
        let (constructors, end_position) = if self.maybe_one(&Token::LeftBrace).is_some() {
            // Custom Type
            let constructors = Parser::series_of(
                self,
                &|p| {
                    if let Some((c_s, c_n, c_e)) = Parser::maybe_upname(p) {
                        let documentation = p.take_documentation(c_s);
                        let (args, args_e) = Parser::parse_type_constructor_args(p)?;
                        let end = args_e.max(c_e);
                        Ok(Some(RecordConstructor {
                            location: SrcSpan { start: c_s, end },
                            name: c_n,
                            arguments: args,
                            documentation,
                        }))
                    } else {
                        Ok(None)
                    }
                },
                // No separator
                None,
            )?;
            let (_, close_end) = self.expect_one(&Token::RightBrace)?;
            (constructors, close_end)
        } else if let Some((eq_s, eq_e)) = self.maybe_one(&Token::Equal) {
            // Type Alias
            if opaque {
                return parse_error(ParseErrorType::OpaqueTypeAlias, SrcSpan { start, end });
            }

            if let Some(t) = self.parse_type()? {
                let type_end = t.location().end;
                return Ok(Some(Definition::TypeAlias(TypeAlias {
                    documentation,
                    location: SrcSpan::new(start, type_end),
                    public,
                    alias: name,
                    parameters,
                    type_ast: t,
                    type_: (),
                    deprecation: std::mem::take(&mut attributes.deprecated),
                })));
            } else {
                return parse_error(ParseErrorType::ExpectedType, SrcSpan::new(eq_s, eq_e));
            }
        } else {
            (vec![], end)
        };
        Ok(Some(Definition::CustomType(CustomType {
            documentation,
            location: SrcSpan { start, end },
            end_position,
            public,
            opaque,
            name,
            parameters,
            constructors,
            typed_parameters: vec![],
            deprecation: std::mem::take(&mut attributes.deprecated),
        })))
    }

    // examples:
    //   A
    //   A(one, two)
    fn expect_type_name(&mut self) -> Result<(u32, EcoString, Vec<EcoString>, u32), ParseError> {
        let (start, upname, end) = self.expect_upname()?;
        if self.maybe_one(&Token::LeftParen).is_some() {
            let args =
                Parser::series_of(self, &|p| Ok(Parser::maybe_name(p)), Some(&Token::Comma))?;
            let (_, par_e) = self.expect_one(&Token::RightParen)?;
            let args2 = args.into_iter().map(|(_, a, _)| a).collect();
            Ok((start, upname, args2, par_e))
        } else {
            Ok((start, upname, vec![], end))
        }
    }

    // examples:
    //   *no args*
    //   ()
    //   (a, b)
    fn parse_type_constructor_args(
        &mut self,
    ) -> Result<(Vec<RecordConstructorArg<()>>, u32), ParseError> {
        if self.maybe_one(&Token::LeftParen).is_some() {
            let args = Parser::series_of(
                self,
                &|p| match (p.tok0.take(), p.tok1.take()) {
                    (Some((start, Token::Name { name }, _)), Some((_, Token::Colon, end))) => {
                        let _ = Parser::next_tok(p);
                        let _ = Parser::next_tok(p);
                        match Parser::parse_type(p)? {
                            Some(type_ast) => Ok(Some(RecordConstructorArg {
                                label: Some(name),
                                ast: type_ast,
                                location: SrcSpan { start, end },
                                type_: (),
                                doc: None,
                            })),
                            None => {
                                parse_error(ParseErrorType::ExpectedType, SrcSpan { start, end })
                            }
                        }
                    }
                    (t0, t1) => {
                        p.tok0 = t0;
                        p.tok1 = t1;
                        match Parser::parse_type(p)? {
                            Some(type_ast) => {
                                let type_location = type_ast.location();
                                Ok(Some(RecordConstructorArg {
                                    label: None,
                                    ast: type_ast,
                                    location: type_location,
                                    type_: (),
                                    doc: None,
                                }))
                            }
                            None => Ok(None),
                        }
                    }
                },
                Some(&Token::Comma),
            )?;
            let (_, end) = self.expect_one(&Token::RightParen)?;
            Ok((args, end))
        } else {
            Ok((vec![], 0))
        }
    }

    //
    // Parse Type Annotations
    //

    // examples:
    //   :a
    //   :Int
    //   :Result(a, _)
    //   :Result(Result(a, e), #(_, String))
    fn parse_type_annotation(&mut self, start_tok: &Token) -> Result<Option<TypeAst>, ParseError> {
        if let Some((start, end)) = self.maybe_one(start_tok) {
            match self.parse_type() {
                Ok(None) => parse_error(ParseErrorType::ExpectedType, SrcSpan { start, end }),
                other => other,
            }
        } else {
            Ok(None)
        }
    }

    // Parse the type part of a type annotation, same as `parse_type_annotation` minus the ":"
    fn parse_type(&mut self) -> Result<Option<TypeAst>, ParseError> {
        match self.tok0.take() {
            // Type hole
            Some((start, Token::DiscardName { name }, end)) => {
                self.advance();
                Ok(Some(TypeAst::Hole(TypeAstHole {
                    location: SrcSpan { start, end },
                    name,
                })))
            }

            // Tuple
            Some((start, Token::Hash, end)) => {
                self.advance();
                let _ = self.expect_one(&Token::LeftParen)?;
                let elems = self.parse_types()?;
                let _ = self.expect_one(&Token::RightParen)?;
                Ok(Some(TypeAst::Tuple(TypeAstTuple {
                    location: SrcSpan { start, end },
                    elems,
                })))
            }

            // Function
            Some((start, Token::Fn, _)) => {
                self.advance();
                let _ = self.expect_one(&Token::LeftParen)?;
                let args =
                    Parser::series_of(self, &|x| Parser::parse_type(x), Some(&Token::Comma))?;
                let _ = self.expect_one(&Token::RightParen)?;
                let (arr_s, arr_e) = self.expect_one(&Token::RArrow)?;
                let retrn = self.parse_type()?;
                if let Some(retrn) = retrn {
                    Ok(Some(TypeAst::Fn(TypeAstFn {
                        location: SrcSpan {
                            start,
                            end: retrn.location().end,
                        },
                        return_: Box::new(retrn),
                        arguments: args,
                    })))
                } else {
                    parse_error(
                        ParseErrorType::ExpectedType,
                        SrcSpan {
                            start: arr_s,
                            end: arr_e,
                        },
                    )
                }
            }

            // Constructor function
            Some((start, Token::UpName { name }, end)) => {
                self.advance();
                self.parse_type_name_finish(start, None, name, end)
            }

            // Constructor Module or type Variable
            Some((start, Token::Name { name: mod_name }, end)) => {
                self.advance();
                if self.maybe_one(&Token::Dot).is_some() {
                    let (_, upname, upname_e) = self.expect_upname()?;
                    self.parse_type_name_finish(start, Some(mod_name), upname, upname_e)
                } else {
                    Ok(Some(TypeAst::Var(TypeAstVar {
                        location: SrcSpan { start, end },
                        name: mod_name,
                    })))
                }
            }

            t0 => {
                self.tok0 = t0;
                Ok(None)
            }
        }
    }

    // Parse the '( ... )' of a type name
    fn parse_type_name_finish(
        &mut self,
        start: u32,
        module: Option<EcoString>,
        name: EcoString,
        end: u32,
    ) -> Result<Option<TypeAst>, ParseError> {
        if self.maybe_one(&Token::LeftParen).is_some() {
            let args = self.parse_types()?;
            let (_, par_e) = self.expect_one(&Token::RightParen)?;
            Ok(Some(TypeAst::Constructor(TypeAstConstructor {
                location: SrcSpan { start, end: par_e },
                module,
                name,
                arguments: args,
            })))
        } else {
            Ok(Some(TypeAst::Constructor(TypeAstConstructor {
                location: SrcSpan { start, end },
                module,
                name,
                arguments: vec![],
            })))
        }
    }

    // For parsing a comma separated "list" of types, for tuple, constructor, and function
    fn parse_types(&mut self) -> Result<Vec<TypeAst>, ParseError> {
        let elems = Parser::series_of(self, &|p| Parser::parse_type(p), Some(&Token::Comma))?;
        Ok(elems)
    }

    //
    // Parse Imports
    //

    // examples:
    //   import a
    //   import a/b
    //   import a/b.{c}
    //   import a/b.{c as d} as e
    fn parse_import(&mut self, import_start: u32) -> Result<Option<UntypedDefinition>, ParseError> {
        let mut start = 0;
        let mut end;
        let mut module = String::new();
        // Gather module names
        loop {
            let (s, name, e) = self.expect_name()?;
            if module.is_empty() {
                start = s;
            } else {
                module.push('/');
            }
            module.push_str(&name);
            end = e;

            // Useful error for : import a/.{b}
            if let Some((s, _)) = self.maybe_one(&Token::SlashDot) {
                return parse_error(
                    ParseErrorType::ExpectedName,
                    SrcSpan {
                        start: s + 1,
                        end: s + 1,
                    },
                );
            }

            // break if there's no trailing slash
            if self.maybe_one(&Token::Slash).is_none() {
                break;
            }
        }

        let documentation = self.take_documentation(start);

        // Gather imports
        let mut unqualified_values = vec![];
        let mut unqualified_types = vec![];

        if self.maybe_one(&Token::Dot).is_some() {
            let _ = self.expect_one(&Token::LeftBrace)?;
            let parsed = self.parse_unqualified_imports()?;
            unqualified_types = parsed.types;
            unqualified_values = parsed.values;
            let (_, e) = self.expect_one(&Token::RightBrace)?;
            end = e;
        }

        // Parse as_name
        let mut as_name = None;
        if let Some((as_start, _)) = self.maybe_one(&Token::As) {
            let (_, name, e) = self.expect_assign_name()?;

            end = e;
            as_name = Some((
                name,
                SrcSpan {
                    start: as_start,
                    end,
                },
            ));
        }

        Ok(Some(Definition::Import(Import {
            documentation,
            location: SrcSpan {
                start: import_start,
                end,
            },
            unqualified_values,
            unqualified_types,
            module: module.into(),
            as_name,
            package: (),
        })))
    }

    // [Name (as Name)? | UpName (as Name)? ](, [Name (as Name)? | UpName (as Name)?])*,?
    fn parse_unqualified_imports(&mut self) -> Result<ParsedUnqualifiedImports, ParseError> {
        let mut imports = ParsedUnqualifiedImports::default();
        loop {
            // parse imports
            match self.tok0.take() {
                Some((start, Token::Name { name }, end)) => {
                    self.advance();
                    let location = SrcSpan { start, end };
                    let mut import = UnqualifiedImport {
                        name,
                        location,
                        as_name: None,
                    };
                    if self.maybe_one(&Token::As).is_some() {
                        let (_, as_name, _) = self.expect_name()?;
                        import.as_name = Some(as_name);
                    }
                    imports.values.push(import)
                }

                Some((start, Token::UpName { name }, end)) => {
                    self.advance();
                    let location = SrcSpan { start, end };
                    let mut import = UnqualifiedImport {
                        name,
                        location,
                        as_name: None,
                    };
                    if self.maybe_one(&Token::As).is_some() {
                        let (_, as_name, _) = self.expect_upname()?;
                        import.as_name = Some(as_name);
                    }
                    imports.values.push(import)
                }

                Some((start, Token::Type, _)) => {
                    self.advance();
                    let (_, name, end) = self.expect_upname()?;
                    let location = SrcSpan { start, end };
                    let mut import = UnqualifiedImport {
                        name,
                        location,
                        as_name: None,
                    };
                    if self.maybe_one(&Token::As).is_some() {
                        let (_, as_name, _) = self.expect_upname()?;
                        import.as_name = Some(as_name);
                    }
                    imports.types.push(import)
                }

                t0 => {
                    self.tok0 = t0;
                    break;
                }
            }
            // parse comma
            match self.tok0 {
                Some((_, Token::Comma, _)) => {
                    self.advance();
                }
                _ => break,
            }
        }
        Ok(imports)
    }

    //
    // Parse Constants
    //

    // examples:
    //   const a = 1
    //   const a:Int = 1
    //   pub const a:Int = 1
    fn parse_module_const(
        &mut self,
        public: bool,
        attributes: &Attributes,
    ) -> Result<Option<UntypedDefinition>, ParseError> {
        let (start, name, end) = self.expect_name()?;
        let documentation = self.take_documentation(start);

        let annotation = self.parse_type_annotation(&Token::Colon)?;

        let (eq_s, eq_e) = self.expect_one(&Token::Equal)?;
        if let Some(value) = self.parse_const_value()? {
            Ok(Some(Definition::ModuleConstant(ModuleConstant {
                documentation,
                location: SrcSpan { start, end },
                public,
                name,
                annotation,
                value: Box::new(value),
                type_: (),
                deprecation: attributes.deprecated.clone(),
                implementations: Implementations {
                    gleam: true,
                    uses_erlang_externals: false,
                    uses_javascript_externals: false,
                },
            })))
        } else {
            parse_error(
                ParseErrorType::NoValueAfterEqual,
                SrcSpan {
                    start: eq_s,
                    end: eq_e,
                },
            )
        }
    }

    // examples:
    //   1
    //   "hi"
    //   True
    //   [1,2,3]
    fn parse_const_value(&mut self) -> Result<Option<UntypedConstant>, ParseError> {
        match self.tok0.take() {
            Some((start, Token::String { value }, end)) => {
                self.advance();
                Ok(Some(Constant::String {
                    value,
                    location: SrcSpan { start, end },
                }))
            }

            Some((start, Token::Float { value }, end)) => {
                self.advance();
                Ok(Some(Constant::Float {
                    value,
                    location: SrcSpan { start, end },
                }))
            }

            Some((start, Token::Int { value }, end)) => {
                self.advance();
                Ok(Some(Constant::Int {
                    value,
                    location: SrcSpan { start, end },
                }))
            }

            Some((start, Token::Hash, _)) => {
                self.advance();
                let _ = self.expect_one(&Token::LeftParen)?;
                let elements =
                    Parser::series_of(self, &Parser::parse_const_value, Some(&Token::Comma))?;
                let (_, end) = self.expect_one(&Token::RightParen)?;
                Ok(Some(Constant::Tuple {
                    elements,
                    location: SrcSpan { start, end },
                }))
            }

            Some((start, Token::LeftSquare, _)) => {
                self.advance();
                let elements =
                    Parser::series_of(self, &Parser::parse_const_value, Some(&Token::Comma))?;
                let (_, end) = self.expect_one(&Token::RightSquare)?;
                Ok(Some(Constant::List {
                    elements,
                    location: SrcSpan { start, end },
                    typ: (),
                }))
            }
            // BitArray
            Some((start, Token::LtLt, _)) => {
                self.advance();
                let segments = Parser::series_of(
                    self,
                    &|s| {
                        Parser::parse_bit_array_segment(
                            s,
                            &Parser::parse_const_value,
                            &Parser::expect_const_int,
                            &bit_array_const_int,
                        )
                    },
                    Some(&Token::Comma),
                )?;
                let (_, end) = self.expect_one(&Token::GtGt)?;
                Ok(Some(Constant::BitArray {
                    location: SrcSpan { start, end },
                    segments,
                }))
            }

            Some((start, Token::UpName { name }, end)) => {
                self.advance();
                self.parse_const_record_finish(start, None, name, end)
            }

            Some((start, Token::Name { name }, _)) if self.peek_tok1() == Some(&Token::Dot) => {
                self.advance(); // name
                self.advance(); // dot

                match self.tok0.take() {
                    Some((_, Token::UpName { name: upname }, end)) => {
                        self.advance(); // upname
                        self.parse_const_record_finish(start, Some(name), upname, end)
                    }
                    Some((_, Token::Name { name: end_name }, end)) => {
                        self.advance(); // name

                        match self.tok0 {
                            Some((_, Token::LeftParen, _)) => parse_error(
                                ParseErrorType::UnexpectedFunction,
                                SrcSpan {
                                    start,
                                    end: end + 1,
                                },
                            ),
                            _ => Ok(Some(Constant::Var {
                                location: SrcSpan { start, end },
                                module: Some(name),
                                name: end_name,
                                constructor: None,
                                typ: (),
                            })),
                        }
                    }
                    Some((start, _, end)) => parse_error(
                        ParseErrorType::UnexpectedToken {
                            expected: vec!["UpName".into(), "Name".into()],
                            hint: None,
                        },
                        SrcSpan { start, end },
                    ),
                    None => {
                        parse_error(ParseErrorType::UnexpectedEof, SrcSpan { start: 0, end: 0 })
                    }
                }
            }

            Some((start, Token::Name { name }, end)) => {
                self.advance(); // name
                Ok(Some(Constant::Var {
                    location: SrcSpan { start, end },
                    module: None,
                    name,
                    constructor: None,
                    typ: (),
                }))
            }

            // Helpful error for fn
            Some((start, Token::Fn, end)) => {
                parse_error(ParseErrorType::NotConstType, SrcSpan { start, end })
            }

            t0 => {
                self.tok0 = t0;
                Ok(None)
            }
        }
    }

    // Parse the '( .. )' of a const type constructor
    fn parse_const_record_finish(
        &mut self,
        start: u32,
        module: Option<EcoString>,
        name: EcoString,
        end: u32,
    ) -> Result<Option<UntypedConstant>, ParseError> {
        if self.maybe_one(&Token::LeftParen).is_some() {
            let args =
                Parser::series_of(self, &Parser::parse_const_record_arg, Some(&Token::Comma))?;
            let (_, par_e) = self.expect_one(&Token::RightParen)?;
            Ok(Some(Constant::Record {
                location: SrcSpan { start, end: par_e },
                module,
                name,
                args,
                tag: (),
                typ: (),
                field_map: None,
            }))
        } else {
            Ok(Some(Constant::Record {
                location: SrcSpan { start, end },
                module,
                name,
                args: vec![],
                tag: (),
                typ: (),
                field_map: None,
            }))
        }
    }

    // examples:
    //  name: const
    //  const
    fn parse_const_record_arg(&mut self) -> Result<Option<CallArg<UntypedConstant>>, ParseError> {
        let name = match (self.tok0.take(), &self.tok1) {
            // Named arg
            (Some((start, Token::Name { name }, end)), Some((_, Token::Colon, _))) => {
                self.advance();
                self.advance();
                Some((start, name, end))
            }

            // Unnamed arg
            (t0, _) => {
                self.tok0 = t0;
                None
            }
        };

        if let Some(value) = self.parse_const_value()? {
            if let Some((start, label, _)) = name {
                Ok(Some(CallArg {
                    implicit: false,
                    location: SrcSpan {
                        start,
                        end: value.location().end,
                    },
                    value,
                    label: Some(label),
                }))
            } else {
                Ok(Some(CallArg {
                    implicit: false,
                    location: value.location(),
                    value,
                    label: None,
                }))
            }
        } else if name.is_some() {
            self.next_tok_unexpected(vec!["a constant value".into()])?
        } else {
            Ok(None)
        }
    }

    //
    // Bit String parsing
    //

    // The structure is roughly the same for pattern, const, and expr
    // that's why these functions take functions
    //
    // pattern (: option)?
    fn parse_bit_array_segment<A>(
        &mut self,
        value_parser: &impl Fn(&mut Self) -> Result<Option<A>, ParseError>,
        arg_parser: &impl Fn(&mut Self) -> Result<A, ParseError>,
        to_int_segment: &impl Fn(EcoString, u32, u32) -> A,
    ) -> Result<Option<BitArraySegment<A, ()>>, ParseError>
    where
        A: HasLocation + std::fmt::Debug,
    {
        if let Some(value) = value_parser(self)? {
            let options = if self.maybe_one(&Token::Colon).is_some() {
                Parser::series_of(
                    self,
                    &|s| Parser::parse_bit_array_option(s, &arg_parser, &to_int_segment),
                    Some(&Token::Minus),
                )?
            } else {
                vec![]
            };
            let end = options
                .last()
                .map(|o| o.location().end)
                .unwrap_or_else(|| value.location().end);
            Ok(Some(BitArraySegment {
                location: SrcSpan {
                    start: value.location().start,
                    end,
                },
                value: Box::new(value),
                type_: (),
                options,
            }))
        } else {
            Ok(None)
        }
    }

    // examples:
    //   1
    //   size(1)
    //   size(five)
    //   utf8
    fn parse_bit_array_option<A: std::fmt::Debug>(
        &mut self,
        arg_parser: &impl Fn(&mut Self) -> Result<A, ParseError>,
        to_int_segment: &impl Fn(EcoString, u32, u32) -> A,
    ) -> Result<Option<BitArrayOption<A>>, ParseError> {
        match self.next_tok() {
            // named segment
            Some((start, Token::Name { name }, end)) => {
                if self.maybe_one(&Token::LeftParen).is_some() {
                    // named function segment
                    match name.as_str() {
                        "unit" => {
                            if let Some((int_s, Token::Int { value, .. }, int_e)) = self.next_tok()
                            {
                                let (_, end) = self.expect_one(&Token::RightParen)?;
                                let v = value.replace("_", "");
                                match u8::from_str(&v) {
                                    Ok(units) if units > 0 => Ok(Some(BitArrayOption::Unit {
                                        location: SrcSpan { start, end },
                                        value: units,
                                    })),

                                    _ => Err(ParseError {
                                        error: ParseErrorType::InvalidBitArrayUnit,
                                        location: SrcSpan {
                                            start: int_s,
                                            end: int_e,
                                        },
                                    }),
                                }
                            } else {
                                self.next_tok_unexpected(vec!["positive integer".into()])
                            }
                        }

                        "size" => {
                            let value = arg_parser(self)?;
                            let (_, end) = self.expect_one(&Token::RightParen)?;
                            Ok(Some(BitArrayOption::Size {
                                location: SrcSpan { start, end },
                                value: Box::new(value),
                                short_form: false,
                            }))
                        }
                        _ => parse_error(
                            ParseErrorType::InvalidBitArraySegment,
                            SrcSpan { start, end },
                        ),
                    }
                } else {
                    str_to_bit_array_option(&name, SrcSpan { start, end })
                        .ok_or(ParseError {
                            error: ParseErrorType::InvalidBitArraySegment,
                            location: SrcSpan { start, end },
                        })
                        .map(Some)
                }
            }
            // int segment
            Some((start, Token::Int { value }, end)) => Ok(Some(BitArrayOption::Size {
                location: SrcSpan { start, end },
                value: Box::new(to_int_segment(value, start, end)),
                short_form: true,
            })),
            // invalid
            _ => self.next_tok_unexpected(vec![
                "A valid bit array segment type".into(),
                "See: https://gleam.run/book/tour/bit-arrays.html".into(),
            ]),
        }
    }

    fn expect_bit_array_pattern_segment_arg(&mut self) -> Result<UntypedPattern, ParseError> {
        match self.next_tok() {
            Some((start, Token::Name { name }, end)) => Ok(Pattern::VarUsage {
                location: SrcSpan { start, end },
                name,
                constructor: None,
                type_: (),
            }),
            Some((start, Token::Int { value }, end)) => Ok(Pattern::Int {
                location: SrcSpan { start, end },
                value,
            }),
            _ => self.next_tok_unexpected(vec!["A variable name or an integer".into()]),
        }
    }

    fn expect_const_int(&mut self) -> Result<UntypedConstant, ParseError> {
        match self.next_tok() {
            Some((start, Token::Int { value }, end)) => Ok(Constant::Int {
                location: SrcSpan { start, end },
                value,
            }),
            _ => self.next_tok_unexpected(vec!["A variable name or an integer".into()]),
        }
    }

    fn expect_expression(&mut self) -> Result<UntypedExpr, ParseError> {
        if let Some(e) = self.parse_expression()? {
            Ok(e)
        } else {
            self.next_tok_unexpected(vec!["An expression".into()])
        }
    }

    fn expect_expression_unit(&mut self) -> Result<UntypedExpr, ParseError> {
        if let Some(e) = self.parse_expression_unit()? {
            Ok(e)
        } else {
            self.next_tok_unexpected(vec!["An expression".into()])
        }
    }

    //
    // Parse Helpers
    //

    // Expect a particular token, advances the token stream
    fn expect_one(&mut self, wanted: &Token) -> Result<(u32, u32), ParseError> {
        match self.maybe_one(wanted) {
            Some((start, end)) => Ok((start, end)),
            None => self.next_tok_unexpected(vec![wanted.to_string().into()]),
        }
    }

    // Expect a Name else a token dependent helpful error
    fn expect_name(&mut self) -> Result<(u32, EcoString, u32), ParseError> {
        let (start, token, end) = self.expect_assign_name()?;
        match token {
            AssignName::Variable(name) => Ok((start, name, end)),
            AssignName::Discard(_) => {
                parse_error(ParseErrorType::IncorrectName, SrcSpan { start, end })
            }
        }
    }

    fn expect_assign_name(&mut self) -> Result<(u32, AssignName, u32), ParseError> {
        let t = self.next_tok();
        match t {
            Some((start, tok, end)) => match tok {
                Token::Name { name } => Ok((start, AssignName::Variable(name), end)),
                Token::DiscardName { name, .. } => Ok((start, AssignName::Discard(name), end)),
                Token::UpName { .. } => {
                    parse_error(ParseErrorType::IncorrectName, SrcSpan { start, end })
                }
                _ if is_reserved_word(tok) => parse_error(
                    ParseErrorType::UnexpectedReservedWord,
                    SrcSpan { start, end },
                ),
                _ => parse_error(ParseErrorType::ExpectedName, SrcSpan { start, end }),
            },
            None => parse_error(ParseErrorType::UnexpectedEof, SrcSpan { start: 0, end: 0 }),
        }
    }

    // Expect an UpName else a token dependent helpful error
    fn expect_upname(&mut self) -> Result<(u32, EcoString, u32), ParseError> {
        let t = self.next_tok();
        match t {
            Some((start, tok, end)) => {
                if let Token::Name { .. } = tok {
                    parse_error(ParseErrorType::IncorrectUpName, SrcSpan { start, end })
                } else if let Token::UpName { name } = tok {
                    Ok((start, name, end))
                } else if let Token::DiscardName { .. } = tok {
                    parse_error(ParseErrorType::IncorrectUpName, SrcSpan { start, end })
                } else {
                    parse_error(ParseErrorType::ExpectedUpName, SrcSpan { start, end })
                }
            }
            None => parse_error(ParseErrorType::UnexpectedEof, SrcSpan { start: 0, end: 0 }),
        }
    }

    // Expect a target name. e.g. `javascript` or `erlang`
    fn expect_target(&mut self) -> Result<Target, ParseError> {
        let (_, t, _) = match self.next_tok() {
            Some(t) => t,
            None => {
                return parse_error(ParseErrorType::UnexpectedEof, SrcSpan { start: 0, end: 0 })
            }
        };
        match t {
            Token::Name { name } => match Target::from_str(&name) {
                Ok(target) => Ok(target),
                Err(_) => self.next_tok_unexpected(Target::variant_strings()),
            },
            _ => self.next_tok_unexpected(Target::variant_strings()),
        }
    }

    // Expect a String else error
    fn expect_string(&mut self) -> Result<(u32, EcoString, u32), ParseError> {
        match self.next_tok() {
            Some((start, Token::String { value }, end)) => Ok((start, value, end)),
            _ => self.next_tok_unexpected(vec!["a string".into()]),
        }
    }

    fn peek_tok1(&mut self) -> Option<&Token> {
        self.tok1.as_ref().map(|(_, token, _)| token)
    }

    // If the next token matches the requested, consume it and return (start, end)
    fn maybe_one(&mut self, tok: &Token) -> Option<(u32, u32)> {
        match self.tok0.take() {
            Some((s, t, e)) if t == *tok => {
                self.advance();
                Some((s, e))
            }

            t0 => {
                self.tok0 = t0;
                None
            }
        }
    }

    // Parse a series by repeating a parser, and possibly a separator
    fn series_of<A>(
        &mut self,
        parser: &impl Fn(&mut Self) -> Result<Option<A>, ParseError>,
        sep: Option<&Token>,
    ) -> Result<Vec<A>, ParseError> {
        let mut results = vec![];
        while let Some(result) = parser(self)? {
            results.push(result);
            if let Some(sep) = sep {
                if self.maybe_one(sep).is_none() {
                    break;
                }
                // Helpful error if extra separator
                if let Some((start, end)) = self.maybe_one(sep) {
                    return parse_error(ParseErrorType::ExtraSeparator, SrcSpan { start, end });
                }
            }
        }

        Ok(results)
    }

    // If next token is a Name, consume it and return relevant info, otherwise, return none
    fn maybe_name(&mut self) -> Option<(u32, EcoString, u32)> {
        match self.tok0.take() {
            Some((s, Token::Name { name }, e)) => {
                self.advance();
                Some((s, name, e))
            }
            t0 => {
                self.tok0 = t0;
                None
            }
        }
    }

    // if next token is an UpName, consume it and return relevant info, otherwise, return none
    fn maybe_upname(&mut self) -> Option<(u32, EcoString, u32)> {
        match self.tok0.take() {
            Some((s, Token::UpName { name }, e)) => {
                self.advance();
                Some((s, name, e))
            }
            t0 => {
                self.tok0 = t0;
                None
            }
        }
    }

    // if next token is a DiscardName, consume it and return relevant info, otherwise, return none
    fn maybe_discard_name(&mut self) -> Option<(u32, EcoString, u32)> {
        match self.tok0.take() {
            Some((s, Token::DiscardName { name }, e)) => {
                self.advance();
                Some((s, name, e))
            }
            t0 => {
                self.tok0 = t0;
                None
            }
        }
    }

    // Error on the next token or EOF
    fn next_tok_unexpected<A>(&mut self, expected: Vec<EcoString>) -> Result<A, ParseError> {
        match self.next_tok() {
            None => parse_error(ParseErrorType::UnexpectedEof, SrcSpan { start: 0, end: 0 }),

            Some((start, _, end)) => parse_error(
                ParseErrorType::UnexpectedToken {
                    expected,
                    hint: None,
                },
                SrcSpan { start, end },
            ),
        }
    }

    // Moves the token stream forward
    fn advance(&mut self) {
        let _ = self.next_tok();
    }

    // Moving the token stream forward
    // returns old tok0
    fn next_tok(&mut self) -> Option<Spanned> {
        let t = self.tok0.take();
        let mut nxt;
        loop {
            match self.tokens.next() {
                // gather and skip extra
                Some(Ok((s, Token::EmptyLine, _))) => {
                    self.extra.empty_lines.push(s);
                }
                Some(Ok((start, Token::CommentNormal, end))) => {
                    self.extra.comments.push(SrcSpan { start, end });
                }
                Some(Ok((start, Token::CommentDoc { content }, end))) => {
                    self.extra.doc_comments.push(SrcSpan::new(start, end));
                    self.doc_comments.push_back((start, content));
                }
                Some(Ok((start, Token::CommentModule, end))) => {
                    self.extra.module_comments.push(SrcSpan { start, end });
                }

                // die on lex error
                Some(Err(err)) => {
                    nxt = None;
                    self.lex_errors.push(err);
                    break;
                }

                Some(Ok(tok)) => {
                    nxt = Some(tok);
                    break;
                }
                None => {
                    nxt = None;
                    break;
                }
            }
        }
        self.tok0 = self.tok1.take();
        self.tok1 = nxt.take();
        t
    }

    fn take_documentation(&mut self, until: u32) -> Option<EcoString> {
        let mut content = String::new();
        while let Some((start, line)) = self.doc_comments.front() {
            if *start >= until {
                break;
            }
            content.push_str(line);
            content.push('\n');
            _ = self.doc_comments.pop_front();
        }
        if content.is_empty() {
            None
        } else {
            Some(content.into())
        }
    }

    fn parse_attributes(
        &mut self,
        attributes: &mut Attributes,
    ) -> Result<Option<SrcSpan>, ParseError> {
        let mut attributes_span = None;

        while let Some((start, end)) = self.maybe_one(&Token::At) {
            if attributes_span.is_none() {
                attributes_span = Some(SrcSpan { start, end });
            }

            let end = self.parse_attribute(start, attributes)?;
            attributes_span = attributes_span.map(|span| SrcSpan {
                start: span.start,
                end,
            });
        }

        Ok(attributes_span)
    }

    fn parse_attribute(
        &mut self,
        start: u32,
        attributes: &mut Attributes,
    ) -> Result<u32, ParseError> {
        // Parse the name of the attribute.

        let (_, name, end) = self.expect_name()?;
        let _ = self.expect_one(&Token::LeftParen)?;

        let end = match name.as_str() {
            "external" => self.parse_external_attribute(start, end, attributes),
            "target" => self.parse_target_attribute(start, end, attributes),
            "deprecated" => self.parse_deprecated_attribute(start, end, attributes),
            _ => parse_error(ParseErrorType::UnknownAttribute, SrcSpan { start, end }),
        }?;

        Ok(end)
    }

    fn parse_target_attribute(
        &mut self,
        start: u32,
        end: u32,
        attributes: &mut Attributes,
    ) -> Result<u32, ParseError> {
        let target = self.expect_target()?;
        if attributes.target.is_some() {
            return parse_error(ParseErrorType::DuplicateAttribute, SrcSpan { start, end });
        }
        let (_, end) = self.expect_one(&Token::RightParen)?;
        if attributes.target.is_some() {
            return parse_error(ParseErrorType::DuplicateAttribute, SrcSpan { start, end });
        }
        attributes.target = Some(target);
        Ok(end)
    }

    fn parse_external_attribute(
        &mut self,
        start: u32,
        end: u32,
        attributes: &mut Attributes,
    ) -> Result<u32, ParseError> {
        let (_, name, _) = self.expect_name()?;

        match name.as_str() {
            "erlang" => {
                let _ = self.expect_one(&Token::Comma)?;
                let (_, module, _) = self.expect_string()?;
                let _ = self.expect_one(&Token::Comma)?;
                let (_, function, _) = self.expect_string()?;
                let _ = self.maybe_one(&Token::Comma);
                let (_, end) = self.expect_one(&Token::RightParen)?;
                if attributes.external_erlang.is_some() {
                    return parse_error(ParseErrorType::DuplicateAttribute, SrcSpan { start, end });
                }
                attributes.external_erlang = Some((module, function));
                Ok(end)
            }

            "javascript" => {
                let _ = self.expect_one(&Token::Comma)?;
                let (_, module, _) = self.expect_string()?;
                let _ = self.expect_one(&Token::Comma)?;
                let (_, function, _) = self.expect_string()?;
                let _ = self.maybe_one(&Token::Comma);
                let _ = self.expect_one(&Token::RightParen)?;
                if attributes.external_javascript.is_some() {
                    return parse_error(ParseErrorType::DuplicateAttribute, SrcSpan { start, end });
                }
                attributes.external_javascript = Some((module, function));
                Ok(end)
            }

            _ => parse_error(ParseErrorType::UnknownAttribute, SrcSpan::new(start, end)),
        }
    }

    fn parse_deprecated_attribute(
        &mut self,
        start: u32,
        end: u32,
        attributes: &mut Attributes,
    ) -> Result<u32, ParseError> {
        if attributes.deprecated.is_deprecated() {
            return parse_error(ParseErrorType::DuplicateAttribute, SrcSpan::new(start, end));
        }
        let (_, message, _) = self.expect_string()?;
        let (_, end) = self.expect_one(&Token::RightParen)?;
        attributes.deprecated = Deprecation::Deprecated { message };
        Ok(end)
    }
}

fn concat_pattern_variable_left_hand_side_error<T>(start: u32, end: u32) -> Result<T, ParseError> {
    Err(ParseError {
        error: ParseErrorType::ConcatPatternVariableLeftHandSide,
        location: SrcSpan::new(start, end),
    })
}

// Operator Precedence Parsing
//
// Higher number means higher precedence.
// All operators are left associative.

/// Simple-Precedence-Parser, handle seeing an operator or end
fn handle_op<A>(
    next_op: Option<(Spanned, u8)>,
    opstack: &mut Vec<(Spanned, u8)>,
    estack: &mut Vec<A>,
    do_reduce: &impl Fn(Spanned, &mut Vec<A>),
) -> Option<A> {
    let mut next_op = next_op;
    loop {
        match (opstack.pop(), next_op.take()) {
            (None, None) => {
                if let Some(fin) = estack.pop() {
                    if estack.is_empty() {
                        return Some(fin);
                    } else {
                        panic!("Expression not fully reduced.")
                    }
                } else {
                    return None;
                }
            }

            (None, Some(op)) => {
                opstack.push(op);
                break;
            }

            (Some((op, _)), None) => do_reduce(op, estack),

            (Some((opl, pl)), Some((opr, pr))) => {
                match pl.cmp(&pr) {
                    // all ops are left associative
                    Ordering::Greater | Ordering::Equal => {
                        do_reduce(opl, estack);
                        next_op = Some((opr, pr));
                    }
                    Ordering::Less => {
                        opstack.push((opl, pl));
                        opstack.push((opr, pr));
                        break;
                    }
                }
            }
        }
    }
    None
}

fn precedence(t: &Token) -> Option<u8> {
    if t == &Token::Pipe {
        return Some(6);
    };
    tok_to_binop(t).map(|op| op.precedence())
}

fn tok_to_binop(t: &Token) -> Option<BinOp> {
    match t {
        Token::VbarVbar => Some(BinOp::Or),
        Token::AmperAmper => Some(BinOp::And),
        Token::EqualEqual => Some(BinOp::Eq),
        Token::NotEqual => Some(BinOp::NotEq),
        Token::Less => Some(BinOp::LtInt),
        Token::LessEqual => Some(BinOp::LtEqInt),
        Token::Greater => Some(BinOp::GtInt),
        Token::GreaterEqual => Some(BinOp::GtEqInt),
        Token::LessDot => Some(BinOp::LtFloat),
        Token::LessEqualDot => Some(BinOp::LtEqFloat),
        Token::GreaterDot => Some(BinOp::GtFloat),
        Token::GreaterEqualDot => Some(BinOp::GtEqFloat),
        Token::Plus => Some(BinOp::AddInt),
        Token::Minus => Some(BinOp::SubInt),
        Token::PlusDot => Some(BinOp::AddFloat),
        Token::MinusDot => Some(BinOp::SubFloat),
        Token::Percent => Some(BinOp::RemainderInt),
        Token::Star => Some(BinOp::MultInt),
        Token::StarDot => Some(BinOp::MultFloat),
        Token::Slash => Some(BinOp::DivInt),
        Token::SlashDot => Some(BinOp::DivFloat),
        Token::LtGt => Some(BinOp::Concatenate),
        _ => None,
    }
}
/// Simple-Precedence-Parser, perform reduction for expression
fn do_reduce_expression(op: Spanned, estack: &mut Vec<UntypedExpr>) {
    match (estack.pop(), estack.pop()) {
        (Some(er), Some(el)) => {
            let new_e = expr_op_reduction(op, el, er);
            estack.push(new_e);
        }
        _ => panic!("Tried to reduce without 2 expressions"),
    }
}

/// Simple-Precedence-Parser, perform reduction for clause guard
fn do_reduce_clause_guard(op: Spanned, estack: &mut Vec<UntypedClauseGuard>) {
    match (estack.pop(), estack.pop()) {
        (Some(er), Some(el)) => {
            let new_e = clause_guard_reduction(op, el, er);
            estack.push(new_e);
        }
        _ => panic!("Tried to reduce without 2 guards"),
    }
}

fn expr_op_reduction((_, token, _): Spanned, l: UntypedExpr, r: UntypedExpr) -> UntypedExpr {
    if token == Token::Pipe {
        let expressions = if let UntypedExpr::PipeLine { mut expressions } = l {
            expressions.push(r);
            expressions
        } else {
            vec1![l, r]
        };
        UntypedExpr::PipeLine { expressions }
    } else if let Some(bin_op) = tok_to_binop(&token) {
        UntypedExpr::BinOp {
            location: SrcSpan {
                start: l.location().start,
                end: r.location().end,
            },
            name: bin_op,
            left: Box::new(l),
            right: Box::new(r),
        }
    } else {
        panic!("Token could not be converted to binop.")
    }
}

fn clause_guard_reduction(
    (_, token, _): Spanned,
    l: UntypedClauseGuard,
    r: UntypedClauseGuard,
) -> UntypedClauseGuard {
    let location = SrcSpan {
        start: l.location().start,
        end: r.location().end,
    };
    let left = Box::new(l);
    let right = Box::new(r);
    match token {
        Token::VbarVbar => ClauseGuard::Or {
            location,
            left,
            right,
        },

        Token::AmperAmper => ClauseGuard::And {
            location,
            left,
            right,
        },

        Token::EqualEqual => ClauseGuard::Equals {
            location,
            left,
            right,
        },

        Token::NotEqual => ClauseGuard::NotEquals {
            location,
            left,
            right,
        },

        Token::Greater => ClauseGuard::GtInt {
            location,
            left,
            right,
        },

        Token::GreaterEqual => ClauseGuard::GtEqInt {
            location,
            left,
            right,
        },

        Token::Less => ClauseGuard::LtInt {
            location,
            left,
            right,
        },

        Token::LessEqual => ClauseGuard::LtEqInt {
            location,
            left,
            right,
        },

        Token::GreaterDot => ClauseGuard::GtFloat {
            location,
            left,
            right,
        },

        Token::GreaterEqualDot => ClauseGuard::GtEqFloat {
            location,
            left,
            right,
        },

        Token::LessDot => ClauseGuard::LtFloat {
            location,
            left,
            right,
        },

        Token::LessEqualDot => ClauseGuard::LtEqFloat {
            location,
            left,
            right,
        },

        _ => panic!("Token could not be converted to Guard Op."),
    }
}

// BitArray Parse Helpers
//
// BitArrays in patterns, guards, and expressions have a very similar structure
// but need specific types. These are helpers for that. There is probably a
// rustier way to do this :)
fn bit_array_pattern_int(value: EcoString, start: u32, end: u32) -> UntypedPattern {
    Pattern::Int {
        location: SrcSpan { start, end },
        value,
    }
}

fn bit_array_expr_int(value: EcoString, start: u32, end: u32) -> UntypedExpr {
    UntypedExpr::Int {
        location: SrcSpan { start, end },
        value,
    }
}

fn bit_array_const_int(value: EcoString, start: u32, end: u32) -> UntypedConstant {
    Constant::Int {
        location: SrcSpan { start, end },
        value,
    }
}

fn str_to_bit_array_option<A>(lit: &str, location: SrcSpan) -> Option<BitArrayOption<A>> {
    match lit {
        "bytes" => Some(BitArrayOption::Bytes { location }),
        "int" => Some(BitArrayOption::Int { location }),
        "float" => Some(BitArrayOption::Float { location }),
        "bits" => Some(BitArrayOption::Bits { location }),
        "utf8" => Some(BitArrayOption::Utf8 { location }),
        "utf16" => Some(BitArrayOption::Utf16 { location }),
        "utf32" => Some(BitArrayOption::Utf32 { location }),
        "utf8_codepoint" => Some(BitArrayOption::Utf8Codepoint { location }),
        "utf16_codepoint" => Some(BitArrayOption::Utf16Codepoint { location }),
        "utf32_codepoint" => Some(BitArrayOption::Utf32Codepoint { location }),
        "signed" => Some(BitArrayOption::Signed { location }),
        "unsigned" => Some(BitArrayOption::Unsigned { location }),
        "big" => Some(BitArrayOption::Big { location }),
        "little" => Some(BitArrayOption::Little { location }),
        "native" => Some(BitArrayOption::Native { location }),
        _ => None,
    }
}

//
// Error Helpers
//
fn parse_error<T>(error: ParseErrorType, location: SrcSpan) -> Result<T, ParseError> {
    Err(ParseError { error, location })
}

//
// Misc Helpers
//

/// Returns whether the given token is a reserved word.
///
/// Useful for checking if a user tried to enter a reserved word as a name.
fn is_reserved_word(tok: Token) -> bool {
    matches![
        tok,
        Token::As
            | Token::Assert
            | Token::Case
            | Token::Const
            | Token::Fn
            | Token::If
            | Token::Import
            | Token::Let
            | Token::Opaque
            | Token::Pub
            | Token::Todo
            | Token::Type
            | Token::Use
    ]
}

// Parsing a function call into the appropriate structure
#[derive(Debug)]
pub enum ParserArg {
    Arg(Box<CallArg<UntypedExpr>>),
    Hole {
        name: EcoString,
        location: SrcSpan,
        label: Option<EcoString>,
    },
}

pub fn make_call(
    fun: UntypedExpr,
    args: Vec<ParserArg>,
    start: u32,
    end: u32,
) -> Result<UntypedExpr, ParseError> {
    let mut num_holes = 0;
    let args = args
        .into_iter()
        .map(|a| match a {
            ParserArg::Arg(arg) => Ok(*arg),
            ParserArg::Hole {
                location,
                name,
                label,
            } => {
                num_holes += 1;

                if name != "_" {
                    return parse_error(
                        ParseErrorType::UnexpectedToken {
                            expected: vec!["An expression".into(), "An underscore".into()],
                            hint: None,
                        },
                        location,
                    );
                }

                Ok(CallArg {
                    implicit: false,
                    label,
                    location,
                    value: UntypedExpr::Var {
                        location,
                        name: CAPTURE_VARIABLE.into(),
                    },
                })
            }
        })
        .collect::<Result<_, _>>()?;
    let call = UntypedExpr::Call {
        location: SrcSpan { start, end },
        fun: Box::new(fun),
        arguments: args,
    };
    match num_holes {
        // A normal call
        0 => Ok(call),

        // An anon function using the capture syntax run(_, 1, 2)
        1 => Ok(UntypedExpr::Fn {
            location: call.location(),
            is_capture: true,
            arguments: vec![Arg {
                location: SrcSpan { start: 0, end: 0 },
                annotation: None,
                names: ArgNames::Named {
                    name: CAPTURE_VARIABLE.into(),
                },
                type_: (),
            }],
            body: vec1![Statement::Expression(call)],
            return_annotation: None,
        }),

        _ => parse_error(ParseErrorType::TooManyArgHoles, SrcSpan { start, end }),
    }
}

#[derive(Debug, Default)]
struct ParsedUnqualifiedImports {
    types: Vec<UnqualifiedImport>,
    values: Vec<UnqualifiedImport>,
}
