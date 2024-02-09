import gleam/bit_builder.{BitBuilder}
import gleam/http.{Header}
import gleam/http/response.{Response}
import gleam/int
import gleam/list

/// Turns an HTTP response into a TCP message
pub fn to_bit_builder(resp: Response(BitBuilder)) -> BitBuilder {
  resp.status
  |> response_builder(resp.headers)
  |> bit_builder.append_builder(resp.body)
}

pub fn response_builder(status: Int, headers: List(Header)) -> BitBuilder {
  let status_string =
    status
    |> int.to_string
    |> bit_builder.from_string
    |> bit_builder.append(<<" ":utf8>>)
    |> bit_builder.append(status_to_bit_string(status))

  bit_builder.new()
  |> bit_builder.append(<<"HTTP/1.1 ":utf8>>)
  |> bit_builder.append_builder(status_string)
  |> bit_builder.append(<<"\r\n":utf8>>)
  |> bit_builder.append_builder(encode_headers(headers))
  |> bit_builder.append(<<"\r\n":utf8>>)
}

pub fn status_to_bit_string(status: Int) -> BitString {
  // Obviously nowhere near exhaustive...
  case status {
    100 -> <<"Continue":utf8>>
    101 -> <<"Switching Protocols":utf8>>
    103 -> <<"Early Hints":utf8>>
    200 -> <<"OK":utf8>>
    201 -> <<"Created":utf8>>
    202 -> <<"Accepted":utf8>>
    203 -> <<"Non-Authoritative Information":utf8>>
    204 -> <<"No Content":utf8>>
    205 -> <<"Reset Content":utf8>>
    206 -> <<"Partial Content":utf8>>
    300 -> <<"Multiple Choices":utf8>>
    301 -> <<"Moved Permanently":utf8>>
    302 -> <<"Found":utf8>>
    303 -> <<"See Other":utf8>>
    304 -> <<"Not Modified":utf8>>
    307 -> <<"Temporary Redirect":utf8>>
    308 -> <<"Permanent Redirect":utf8>>
    400 -> <<"Bad Request":utf8>>
    401 -> <<"Unauthorized":utf8>>
    402 -> <<"Payment Required":utf8>>
    403 -> <<"Forbidden":utf8>>
    404 -> <<"Not Found":utf8>>
    405 -> <<"Method Not Allowed":utf8>>
    406 -> <<"Not Acceptable":utf8>>
    407 -> <<"Proxy Authentication Required":utf8>>
    408 -> <<"Request Timeout":utf8>>
    409 -> <<"Conflict":utf8>>
    410 -> <<"Gone":utf8>>
    411 -> <<"Length Required":utf8>>
    412 -> <<"Precondition Failed":utf8>>
    413 -> <<"Payload Too Large":utf8>>
    414 -> <<"URI Too Long":utf8>>
    415 -> <<"Unsupported Media Type":utf8>>
    416 -> <<"Range Not Satisfiable":utf8>>
    417 -> <<"Expectation Failed":utf8>>
    418 -> <<"I'm a teapot":utf8>>
    422 -> <<"Unprocessable Entity":utf8>>
    425 -> <<"Too Early":utf8>>
    426 -> <<"Upgrade Required":utf8>>
    428 -> <<"Precondition Required":utf8>>
    429 -> <<"Too Many Requests":utf8>>
    431 -> <<"Request Header Fields Too Large":utf8>>
    451 -> <<"Unavailable For Legal Reasons":utf8>>
    500 -> <<"Internal Server Error":utf8>>
    501 -> <<"Not Implemented":utf8>>
    502 -> <<"Bad Gateway":utf8>>
    503 -> <<"Service Unavailable":utf8>>
    504 -> <<"Gateway Timeout":utf8>>
    505 -> <<"HTTP Version Not Supported":utf8>>
    506 -> <<"Variant Also Negotiates":utf8>>
    507 -> <<"Insufficient Storage":utf8>>
    508 -> <<"Loop Detected":utf8>>
    510 -> <<"Not Extended":utf8>>
    511 -> <<"Network Authentication Required":utf8>>
  }
}

pub fn encode_headers(headers: List(Header)) -> BitBuilder {
  list.fold(
    headers,
    bit_builder.new(),
    fn(builder, tup) {
      let #(header, value) = tup

      builder
      |> bit_builder.append_string(header)
      |> bit_builder.append(<<": ":utf8>>)
      |> bit_builder.append_string(value)
      |> bit_builder.append(<<"\r\n":utf8>>)
    },
  )
}
