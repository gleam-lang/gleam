import gleam/result
import gleam/int
import gleam/list
import gleam/regex
import gleam/string
import gleam/option.{Option, Some}
import gleam/http.{Scheme}

/// Policy options for the SameSite cookie attribute
///
/// https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie/SameSite
pub type SameSitePolicy {
  Lax
  Strict
  None
}

fn same_site_to_string(policy) {
  case policy {
    Lax -> "Lax"
    Strict -> "Strict"
    None -> "None"
  }
}

/// Attributes of a cookie when sent to a client in the `set-cookie` header.
pub type Attributes {
  Attributes(
    max_age: Option(Int),
    domain: Option(String),
    path: Option(String),
    secure: Bool,
    http_only: Bool,
    same_site: Option(SameSitePolicy),
  )
}

/// Helper to create sensible default attributes for a set cookie.
///
/// https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie#Attributes
pub fn defaults(scheme: Scheme) {
  Attributes(
    max_age: option.None,
    domain: option.None,
    path: option.Some("/"),
    secure: scheme == http.Https,
    http_only: True,
    same_site: Some(Lax),
  )
}

const epoch = "Expires=Thu, 01 Jan 1970 00:00:00 GMT"

fn cookie_attributes_to_list(attributes) {
  let Attributes(
    max_age: max_age,
    domain: domain,
    path: path,
    secure: secure,
    http_only: http_only,
    same_site: same_site,
  ) = attributes
  [
    // Expires is a deprecated attribute for cookies, it has been replaced with MaxAge
    // MaxAge is widely supported and so Expires values are not set.
    // Only when deleting cookies is the exception made to use the old format,
    // to ensure complete clearup of cookies if required by an application.
    case max_age {
      option.Some(0) -> option.Some([epoch])
      _ -> option.None
    },
    option.map(max_age, fn(max_age) { ["Max-Age=", int.to_string(max_age)] }),
    option.map(domain, fn(domain) { ["Domain=", domain] }),
    option.map(path, fn(path) { ["Path=", path] }),
    case secure {
      True -> option.Some(["Secure"])
      False -> option.None
    },
    case http_only {
      True -> option.Some(["HttpOnly"])
      False -> option.None
    },
    option.map(
      same_site,
      fn(same_site) { ["SameSite=", same_site_to_string(same_site)] },
    ),
  ]
  |> list.filter_map(option.to_result(_, Nil))
}

pub fn set_header(name: String, value: String, attributes: Attributes) -> String {
  [[name, "=", value], ..cookie_attributes_to_list(attributes)]
  |> list.map(string.join(_, ""))
  |> string.join("; ")
}

/// Parse a list of cookies from a header string. Any malformed cookies will be
/// discarded.
///
pub fn parse(cookie_string: String) -> List(#(String, String)) {
  let assert Ok(re) = regex.from_string("[,;]")
  regex.split(re, cookie_string)
  |> list.filter_map(fn(pair) {
    case string.split_once(string.trim(pair), "=") {
      Ok(#("", _)) -> Error(Nil)
      Ok(#(key, value)) -> {
        let key = string.trim(key)
        let value = string.trim(value)
        use _ <- result.then(check_token(key))
        use _ <- result.then(check_token(value))
        Ok(#(key, value))
      }
      Error(Nil) -> Error(Nil)
    }
  })
}

fn check_token(token: String) -> Result(Nil, Nil) {
  case string.pop_grapheme(token) {
    Error(Nil) -> Ok(Nil)
    Ok(#(" ", _)) -> Error(Nil)
    Ok(#("\t", _)) -> Error(Nil)
    Ok(#("\r", _)) -> Error(Nil)
    Ok(#("\n", _)) -> Error(Nil)
    Ok(#("\f", _)) -> Error(Nil)
    Ok(#(_, rest)) -> check_token(rest)
  }
}
