//// We are using the following grammar for CSV from rfc4180
////
//// file = [header CRLF] record *(CRLF record) [CRLF]
////   header = name *(COMMA name)
////  record = field *(COMMA field)
////  name = field
////  field = (escaped / non-escaped)
////  escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE
////  non-escaped = *TEXTDATA

import gleam/string
import gleam/list

pub type CsvToken {
  Comma
  LF
  CR
  Doublequote
  Textdata(inner: String)
}

pub fn to_lexeme(token: CsvToken) -> String {
  case token {
    Comma -> ","
    LF -> "\n"
    CR -> "\r"
    Doublequote -> "\""
    Textdata(str) -> str
  }
}

pub fn scan(input: String) -> List(CsvToken) {
  input
  |> string.to_utf_codepoints
  |> list.fold(
    [],
    fn(acc, x) {
      case string.utf_codepoint_to_int(x) {
        0x2c -> [Comma, ..acc]
        0x22 -> [Doublequote, ..acc]
        0x0a -> [LF, ..acc]
        0x0D -> [CR, ..acc]
        _ -> {
          let cp = string.from_utf_codepoints([x])
          case acc {
            [Textdata(str), ..rest] -> [Textdata(str <> cp), ..rest]
            _ -> [Textdata(cp), ..acc]
          }
        }
      }
    },
  )
  |> list.reverse
}
