//// We are using the following grammar for CSV from rfc4180
////
//// file = [header CRLF] record *(CRLF record) [CRLF]
////   header = name *(COMMA name)
////  record = field *(COMMA field)
////  name = field
////  field = (escaped / non-escaped)
////  escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE
////  non-escaped = *TEXTDATA

import gleam/list
import gleam/result
import gsv/internal/token.{CR, Comma, CsvToken, Doublequote, LF, Textdata}

type ParseState {
  Beginning
  JustParsedField
  JustParsedComma
  JustParsedNewline
  JustParsedCR
  InsideEscapedString
}

pub fn parse(input: List(CsvToken)) -> Result(List(List(String)), Nil) {
  let inner_rev = {
    use llf <- result.try(parse_p(input, Beginning, []))
    use lf <- list.try_map(llf)
    Ok(list.reverse(lf))
  }
  use ir <- result.try(inner_rev)
  Ok(list.reverse(ir))
}

fn parse_p(
  input: List(CsvToken),
  parse_state: ParseState,
  llf: List(List(String)),
) -> Result(List(List(String)), Nil) {
  case input, parse_state, llf {
    // Error Case: An empty list should produce an Error
    [], Beginning, _ -> Error(Nil)

    // BASE CASE: We are done parsing tokens
    [], _, llf -> Ok(llf)

    // File should begin with either Escaped or Nonescaped string
    [Textdata(str), ..remaining_tokens], Beginning, [] ->
      parse_p(remaining_tokens, JustParsedField, [[str]])

    [Doublequote, ..remaining_tokens], Beginning, [] ->
      parse_p(remaining_tokens, InsideEscapedString, [[""]])

    _, Beginning, _ -> Error(Nil)

    // If we just parsed a field, we're expecting either a comma or a CRLF
    [Comma, ..remaining_tokens], JustParsedField, llf ->
      parse_p(remaining_tokens, JustParsedComma, llf)

    [LF, ..remaining_tokens], JustParsedField, llf ->
      parse_p(remaining_tokens, JustParsedNewline, llf)

    [CR, ..remaining_tokens], JustParsedField, llf ->
      parse_p(remaining_tokens, JustParsedCR, llf)

    _, JustParsedField, _ -> Error(Nil)

    // If we just parsed a CR, we're expecting an LF
    [LF, ..remaining_tokens], JustParsedCR, llf ->
      parse_p(remaining_tokens, JustParsedNewline, llf)

    _, JustParsedCR, _ -> Error(Nil)

    // If we just parsed a comma, we're expecting an Escaped or Non-Escaped string
    [Textdata(str), ..remaining_tokens], JustParsedComma, [
      curr_line,
      ..previously_parsed_lines
    ] ->
      parse_p(
        remaining_tokens,
        JustParsedField,
        [[str, ..curr_line], ..previously_parsed_lines],
      )

    [Doublequote, ..remaining_tokens], JustParsedComma, [
      curr_line,
      ..previously_parsed_lines
    ] ->
      parse_p(
        remaining_tokens,
        InsideEscapedString,
        [["", ..curr_line], ..previously_parsed_lines],
      )

    _, JustParsedComma, _ -> Error(Nil)

    // If we just parsed a new line, we're expecting an escaped or non-escaped string
    [Textdata(str), ..remaining_tokens], JustParsedNewline, llf ->
      parse_p(remaining_tokens, JustParsedField, [[str], ..llf])

    [Doublequote, ..remaining_tokens], JustParsedNewline, [
      curr_line,
      ..previously_parsed_lines
    ] ->
      parse_p(
        remaining_tokens,
        InsideEscapedString,
        [["", ..curr_line], ..previously_parsed_lines],
      )

    _, JustParsedNewline, _ -> Error(Nil)

    // If we're inside an escaped string, we can take anything until we get a double quote,
    // but a double double quote "" escapes the double quote and we keep parsing
    [Doublequote, Doublequote, ..remaining_tokens], InsideEscapedString, [
      [str, ..rest_curr_line],
      ..previously_parsed_lines
    ] ->
      parse_p(
        remaining_tokens,
        InsideEscapedString,
        [[str <> "\"", ..rest_curr_line], ..previously_parsed_lines],
      )

    [Doublequote, ..remaining_tokens], InsideEscapedString, llf ->
      parse_p(remaining_tokens, JustParsedField, llf)

    [other_token, ..remaining_tokens], InsideEscapedString, [
      [str, ..rest_curr_line],
      ..previously_parsed_lines
    ] ->
      parse_p(
        remaining_tokens,
        InsideEscapedString,
        [
          [str <> token.to_lexeme(other_token), ..rest_curr_line],
          ..previously_parsed_lines
        ],
      )
  }
}
