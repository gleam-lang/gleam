//// Utilities for working with URIs
////
//// This module provides functions for working with URIs (for example, parsing
//// URIs or encoding query strings). The functions in this module are implemented
//// according to [RFC 3986](https://tools.ietf.org/html/rfc3986).
////
//// Query encoding (Form encoding) is defined in the
//// [W3C specification](https://www.w3.org/TR/html52/sec-forms.html#urlencoded-form-data).

import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/regex
import gleam/result
import gleam/string
import gleam/string_builder.{type StringBuilder}

/// Type representing holding the parsed components of an URI.
/// All components of a URI are optional, except the path.
///
pub type Uri {
  Uri(
    scheme: Option(String),
    userinfo: Option(String),
    host: Option(String),
    port: Option(Int),
    path: String,
    query: Option(String),
    fragment: Option(String),
  )
}

/// Parses a compliant URI string into the `Uri` Type.
/// If the string is not a valid URI string then an error is returned.
///
/// The opposite operation is `uri.to_string`.
///
/// ## Examples
///
/// ```gleam
/// parse("https://example.com:1234/a/b?query=true#fragment")
/// // -> Ok(
/// //   Uri(
/// //     scheme: Some("https"),
/// //     userinfo: None,
/// //     host: Some("example.com"),
/// //     port: Some(1234),
/// //     path: "/a/b",
/// //     query: Some("query=true"),
/// //     fragment: Some("fragment")
/// //   )
/// // )
/// ```
///
pub fn parse(uri_string: String) -> Result(Uri, Nil) {
  do_parse(uri_string)
}

@external(erlang, "gleam_stdlib", "uri_parse")
fn do_parse(uri_string: String) -> Result(Uri, Nil) {
  // From https://tools.ietf.org/html/rfc3986#appendix-B
  let pattern =
    //    12                        3  4          5       6  7        8
    "^(([a-z][a-z0-9\\+\\-\\.]*):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#.*)?"
  let matches =
    pattern
    |> regex_submatches(uri_string)
    |> pad_list(8)

  let #(scheme, authority, path, query, fragment) = case matches {
    [
      _scheme_with_colon,
      scheme,
      authority_with_slashes,
      _authority,
      path,
      query_with_question_mark,
      _query,
      fragment,
    ] -> #(
      scheme,
      authority_with_slashes,
      path,
      query_with_question_mark,
      fragment,
    )
    _ -> #(None, None, None, None, None)
  }

  let scheme = noneify_empty_string(scheme)
  let path = option.unwrap(path, "")
  let query = noneify_query(query)
  let #(userinfo, host, port) = split_authority(authority)
  let fragment =
    fragment
    |> option.to_result(Nil)
    |> result.try(string.pop_grapheme)
    |> result.map(pair.second)
    |> option.from_result
  let scheme =
    scheme
    |> noneify_empty_string
    |> option.map(string.lowercase)
  Ok(Uri(
    scheme: scheme,
    userinfo: userinfo,
    host: host,
    port: port,
    path: path,
    query: query,
    fragment: fragment,
  ))
}

fn regex_submatches(pattern: String, string: String) -> List(Option(String)) {
  pattern
  |> regex.compile(regex.Options(case_insensitive: True, multi_line: False))
  |> result.nil_error
  |> result.map(regex.scan(_, string))
  |> result.try(list.first)
  |> result.map(fn(m: regex.Match) { m.submatches })
  |> result.unwrap([])
}

fn noneify_query(x: Option(String)) -> Option(String) {
  case x {
    None -> None
    Some(x) ->
      case string.pop_grapheme(x) {
        Ok(#("?", query)) -> Some(query)
        _ -> None
      }
  }
}

fn noneify_empty_string(x: Option(String)) -> Option(String) {
  case x {
    Some("") | None -> None
    Some(_) -> x
  }
}

// Split an authority into its userinfo, host and port parts.
fn split_authority(
  authority: Option(String),
) -> #(Option(String), Option(String), Option(Int)) {
  case option.unwrap(authority, "") {
    "" -> #(None, None, None)
    "//" -> #(None, Some(""), None)
    authority -> {
      let matches =
        "^(//)?((.*)@)?(\\[[a-zA-Z0-9:.]*\\]|[^:]*)(:(\\d*))?"
        |> regex_submatches(authority)
        |> pad_list(6)
      case matches {
        [_, _, userinfo, host, _, port] -> {
          let userinfo = noneify_empty_string(userinfo)
          let host = noneify_empty_string(host)
          let port =
            port
            |> option.unwrap("")
            |> int.parse
            |> option.from_result
          #(userinfo, host, port)
        }
        _ -> #(None, None, None)
      }
    }
  }
}

fn pad_list(list: List(Option(a)), size: Int) -> List(Option(a)) {
  list
  |> list.append(list.repeat(None, extra_required(list, size)))
}

fn extra_required(list: List(a), remaining: Int) -> Int {
  case list {
    _ if remaining == 0 -> 0
    [] -> remaining
    [_, ..rest] -> extra_required(rest, remaining - 1)
  }
}

/// Parses an urlencoded query string into a list of key value pairs.
/// Returns an error for invalid encoding.
///
/// The opposite operation is `uri.query_to_string`.
///
/// ## Examples
///
/// ```gleam
/// parse_query("a=1&b=2")
/// // -> Ok([#("a", "1"), #("b", "2")])
/// ```
///
pub fn parse_query(query: String) -> Result(List(#(String, String)), Nil) {
  do_parse_query(query)
}

@external(erlang, "gleam_stdlib", "parse_query")
@external(javascript, "../gleam_stdlib.mjs", "parse_query")
fn do_parse_query(a: String) -> Result(List(#(String, String)), Nil)

/// Encodes a list of key value pairs as a URI query string.
///
/// The opposite operation is `uri.parse_query`.
///
/// ## Examples
///
/// ```gleam
/// query_to_string([#("a", "1"), #("b", "2")])
/// // -> "a=1&b=2"
/// ```
///
pub fn query_to_string(query: List(#(String, String))) -> String {
  query
  |> list.map(query_pair)
  |> list.intersperse(string_builder.from_string("&"))
  |> string_builder.concat
  |> string_builder.to_string
}

fn query_pair(pair: #(String, String)) -> StringBuilder {
  string_builder.from_strings([
    percent_encode(pair.0),
    "=",
    percent_encode(pair.1),
  ])
}

/// Encodes a string into a percent encoded representation.
///
/// ## Examples
///
/// ```gleam
/// percent_encode("100% great")
/// // -> "100%25%20great"
/// ```
///
pub fn percent_encode(value: String) -> String {
  do_percent_encode(value)
}

@external(erlang, "gleam_stdlib", "percent_encode")
@external(javascript, "../gleam_stdlib.mjs", "percent_encode")
fn do_percent_encode(a: String) -> String

/// Decodes a percent encoded string.
///
/// ## Examples
///
/// ```gleam
/// percent_decode("100%25%20great+fun")
/// // -> Ok("100% great+fun")
/// ```
///
pub fn percent_decode(value: String) -> Result(String, Nil) {
  do_percent_decode(value)
}

@external(erlang, "gleam_stdlib", "percent_decode")
@external(javascript, "../gleam_stdlib.mjs", "percent_decode")
fn do_percent_decode(a: String) -> Result(String, Nil)

fn do_remove_dot_segments(
  input: List(String),
  accumulator: List(String),
) -> List(String) {
  case input {
    [] -> list.reverse(accumulator)
    [segment, ..rest] -> {
      let accumulator = case segment, accumulator {
        "", accumulator -> accumulator
        ".", accumulator -> accumulator
        "..", [] -> []
        "..", [_, ..accumulator] -> accumulator
        segment, accumulator -> [segment, ..accumulator]
      }
      do_remove_dot_segments(rest, accumulator)
    }
  }
}

fn remove_dot_segments(input: List(String)) -> List(String) {
  do_remove_dot_segments(input, [])
}

/// Splits the path section of a URI into it's constituent segments.
///
/// Removes empty segments and resolves dot-segments as specified in
/// [section 5.2](https://www.ietf.org/rfc/rfc3986.html#section-5.2) of the RFC.
///
/// ## Examples
///
/// ```gleam
/// path_segments("/users/1")
/// // -> ["users" ,"1"]
/// ```
///
pub fn path_segments(path: String) -> List(String) {
  remove_dot_segments(string.split(path, "/"))
}

/// Encodes a `Uri` value as a URI string.
///
/// The opposite operation is `uri.parse`.
///
/// ## Examples
///
/// ```gleam
/// let uri = Uri(Some("http"), None, Some("example.com"), ...)
/// to_string(uri)
/// // -> "http://example.com"
/// ```
///
pub fn to_string(uri: Uri) -> String {
  let parts = case uri.fragment {
    Some(fragment) -> ["#", fragment]
    _ -> []
  }
  let parts = case uri.query {
    Some(query) -> ["?", query, ..parts]
    _ -> parts
  }
  let parts = [uri.path, ..parts]
  let parts = case uri.host, string.starts_with(uri.path, "/") {
    Some(host), False if host != "" -> ["/", ..parts]
    _, _ -> parts
  }
  let parts = case uri.host, uri.port {
    Some(_), Some(port) -> [":", int.to_string(port), ..parts]
    _, _ -> parts
  }
  let parts = case uri.scheme, uri.userinfo, uri.host {
    Some(s), Some(u), Some(h) -> [s, "://", u, "@", h, ..parts]
    Some(s), None, Some(h) -> [s, "://", h, ..parts]
    Some(s), Some(_), None | Some(s), None, None -> [s, ":", ..parts]
    None, None, Some(h) -> ["//", h, ..parts]
    _, _, _ -> parts
  }
  string.concat(parts)
}

/// Fetches the origin of a URI.
///
/// Returns the origin of a uri as defined in
/// [RFC 6454](https://tools.ietf.org/html/rfc6454)
///
/// The supported URI schemes are `http` and `https`.
/// URLs without a scheme will return `Error`.
///
/// ## Examples
///
/// ```gleam
/// let assert Ok(uri) = parse("http://example.com/path?foo#bar")
/// origin(uri)
/// // -> Ok("http://example.com")
/// ```
///
pub fn origin(uri: Uri) -> Result(String, Nil) {
  let Uri(scheme: scheme, host: host, port: port, ..) = uri
  case host, scheme {
    Some(h), Some("https") if port == Some(443) ->
      Ok(string.concat(["https://", h]))
    Some(h), Some("http") if port == Some(80) ->
      Ok(string.concat(["http://", h]))
    Some(h), Some(s) if s == "http" || s == "https" -> {
      case port {
        Some(p) -> Ok(string.concat([s, "://", h, ":", int.to_string(p)]))
        None -> Ok(string.concat([s, "://", h]))
      }
    }
    _, _ -> Error(Nil)
  }
}

fn drop_last(elements: List(a)) -> List(a) {
  list.take(from: elements, up_to: list.length(elements) - 1)
}

fn join_segments(segments: List(String)) -> String {
  string.join(["", ..segments], "/")
}

/// Resolves a URI with respect to the given base URI.
///
/// The base URI must be an absolute URI or this function will return an error.
/// The algorithm for merging uris is described in
/// [RFC 3986](https://tools.ietf.org/html/rfc3986#section-5.2).
///
pub fn merge(base: Uri, relative: Uri) -> Result(Uri, Nil) {
  case base {
    Uri(scheme: Some(_), host: Some(_), ..) ->
      case relative {
        Uri(host: Some(_), ..) -> {
          let path =
            string.split(relative.path, "/")
            |> remove_dot_segments()
            |> join_segments()
          let resolved =
            Uri(
              option.or(relative.scheme, base.scheme),
              None,
              relative.host,
              option.or(relative.port, base.port),
              path,
              relative.query,
              relative.fragment,
            )
          Ok(resolved)
        }
        _ -> {
          let #(new_path, new_query) = case relative.path {
            "" -> #(base.path, option.or(relative.query, base.query))
            _ -> {
              let path_segments = case string.starts_with(relative.path, "/") {
                True -> string.split(relative.path, "/")
                False ->
                  string.split(base.path, "/")
                  |> drop_last()
                  |> list.append(string.split(relative.path, "/"))
              }
              let path =
                path_segments
                |> remove_dot_segments()
                |> join_segments()
              #(path, relative.query)
            }
          }
          let resolved =
            Uri(
              base.scheme,
              None,
              base.host,
              base.port,
              new_path,
              new_query,
              relative.fragment,
            )
          Ok(resolved)
        }
      }
    _ -> Error(Nil)
  }
}
