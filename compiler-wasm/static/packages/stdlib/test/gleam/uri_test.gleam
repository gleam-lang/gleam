// TODO: IPv6 URI parse tests
// https://github.com/elixir-lang/elixir/blob/2d43b9670f54c4d8e0be1ee4d2ee8f99d7378480/lib/elixir/test/elixir/uri_test.exs
import gleam/uri
import gleam/should
import gleam/option.{None, Some}
import gleam/string
import gleam/list
import gleam/io

pub fn full_parse_test() {
  assert Ok(parsed) =
    uri.parse("https://weebl:bob@example.com:1234/path?query=true#fragment")
  should.equal(parsed.scheme, Some("https"))
  should.equal(parsed.userinfo, Some("weebl:bob"))
  should.equal(parsed.host, Some("example.com"))
  should.equal(parsed.port, Some(1234))
  should.equal(parsed.path, "/path")
  should.equal(parsed.query, Some("query=true"))
  should.equal(parsed.fragment, Some("fragment"))
}

pub fn parse_only_path_test() {
  assert Ok(parsed) = uri.parse("")
  should.equal(parsed.scheme, None)
  should.equal(parsed.userinfo, None)
  should.equal(parsed.host, None)
  should.equal(parsed.port, None)
  should.equal(parsed.path, "")
  should.equal(parsed.query, None)
  should.equal(parsed.fragment, None)
}

pub fn parse_only_host_test() {
  assert Ok(parsed) = uri.parse("//")
  should.equal(parsed.scheme, None)
  should.equal(parsed.userinfo, None)
  should.equal(parsed.host, Some(""))
  should.equal(parsed.port, None)
  should.equal(parsed.path, "")
  should.equal(parsed.query, None)
  should.equal(parsed.fragment, None)
}

pub fn parse_scheme_test() {
  uri.parse("http://one.com/path/to/something?one=two&two=one#fragment")
  |> should.equal(Ok(uri.Uri(
    scheme: Some("http"),
    host: Some("one.com"),
    path: "/path/to/something",
    query: Some("one=two&two=one"),
    fragment: Some("fragment"),
    port: None,
    userinfo: None,
  )))
}

pub fn parse_https_scheme_test() {
  uri.parse("https://foo.com")
  |> should.equal(Ok(uri.Uri(
    scheme: Some("https"),
    host: Some("foo.com"),
    path: "",
    query: None,
    fragment: None,
    port: None,
    userinfo: None,
  )))
}

pub fn parse_file_scheme_test() {
  uri.parse("file:///one/two/three")
  |> should.equal(Ok(uri.Uri(
    scheme: Some("file"),
    host: Some(""),
    path: "/one/two/three",
    query: None,
    fragment: None,
    port: None,
    userinfo: None,
  )))
}

pub fn parse_ftp_scheme_test() {
  "ftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt"
  |> uri.parse
  |> should.equal(Ok(uri.Uri(
    scheme: Some("ftp"),
    host: Some("private.ftp-server.example.com"),
    userinfo: Some("user001:password"),
    path: "/my_directory/my_file.txt",
    query: None,
    fragment: None,
    port: None,
  )))
}

pub fn parse_sftp_scheme_test() {
  "sftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt"
  |> uri.parse
  |> should.equal(Ok(uri.Uri(
    scheme: Some("sftp"),
    host: Some("private.ftp-server.example.com"),
    userinfo: Some("user001:password"),
    path: "/my_directory/my_file.txt",
    query: None,
    fragment: None,
    port: None,
  )))
}

pub fn parse_tftp_scheme_test() {
  "tftp://user001:password@private.ftp-server.example.com/my_directory/my_file.txt"
  |> uri.parse
  |> should.equal(Ok(uri.Uri(
    scheme: Some("tftp"),
    host: Some("private.ftp-server.example.com"),
    userinfo: Some("user001:password"),
    path: "/my_directory/my_file.txt",
    query: None,
    fragment: None,
    port: None,
  )))
}

pub fn parse_ldap_scheme_test() {
  "ldap:///dc=example,dc=com??sub?(givenName=John)"
  |> uri.parse
  |> should.equal(Ok(uri.Uri(
    scheme: Some("ldap"),
    host: Some(""),
    userinfo: None,
    path: "/dc=example,dc=com",
    query: Some("?sub?(givenName=John)"),
    fragment: None,
    port: None,
  )))
}

pub fn parse_ldap_2_scheme_test() {
  "ldap://ldap.example.com/cn=John%20Doe,dc=foo,dc=com"
  |> uri.parse
  |> should.equal(Ok(uri.Uri(
    scheme: Some("ldap"),
    host: Some("ldap.example.com"),
    userinfo: None,
    path: "/cn=John%20Doe,dc=foo,dc=com",
    query: None,
    fragment: None,
    port: None,
  )))
}

fn assert_parse(s) {
  assert Ok(u) = uri.parse(s)
  u
}

// TODO: Assert these do not parse
// pub fn parse_bad_uris_test() {
//   uri.parse("")
//   uri.parse("https:??@?F?@#>F//23/")
//   assert ":https" = uri.parse(":https").path
//   assert "https" = uri.parse("https").path
// }
pub fn parse_downcases_scheme() {
  assert Ok(uri) = uri.parse("HTTPS://EXAMPLE.COM")
  assert Some("https") = uri.scheme
  assert Some("EXAMPLE.COM") = uri.host
}

pub fn parse_empty_fragments_test() {
  assert Some("") = assert_parse("http://example.com#").fragment
  assert Some("") = assert_parse("http://example.com/#").fragment
  assert Some("") = assert_parse("http://example.com/test#").fragment
}

pub fn parse_empty_queries_test() {
  assert Some("") = assert_parse("http://example.com?").query
  assert Some("") = assert_parse("http://example.com/?").query
  assert Some("") = assert_parse("http://example.com/test?").query
}

pub fn full_uri_to_string_test() {
  let test_uri =
    uri.Uri(
      Some("https"),
      Some("weebl:bob"),
      Some("example.com"),
      Some(1234),
      "/path",
      Some("query=true"),
      Some("fragment"),
    )
  should.equal(
    uri.to_string(test_uri),
    "https://weebl:bob@example.com:1234/path?query=true#fragment",
  )
}

pub fn path_only_uri_to_string_test() {
  uri.Uri(None, None, None, None, "/", None, None)
  |> uri.to_string
  |> should.equal("/")

  uri.Uri(None, None, None, None, "/teapot", None, None)
  |> uri.to_string
  |> should.equal("/teapot")

  uri.Uri(None, Some("user"), None, None, "/teapot", None, None)
  |> uri.to_string
  |> should.equal("/teapot")

  uri.Uri(None, None, None, None, "", None, None)
  |> uri.to_string
  |> should.equal("")
}

pub fn scheme_to_string_test() {
  uri.Uri(Some("ftp"), None, None, None, "thing.txt", None, None)
  |> uri.to_string
  |> should.equal("ftp:thing.txt")

  uri.Uri(Some("ftp"), None, None, None, "", None, None)
  |> uri.to_string
  |> should.equal("ftp:")

  uri.Uri(Some("ftp"), Some("ignored"), None, None, "", None, None)
  |> uri.to_string
  |> should.equal("ftp:")

  uri.Uri(Some("https"), None, None, None, "/one/two", None, None)
  |> uri.to_string
  |> should.equal("https:/one/two")

  uri.Uri(None, None, None, None, "noslash", None, Some("frag"))
  |> uri.to_string
  |> should.equal("noslash#frag")
}

pub fn host_to_string_test() {
  uri.Uri(Some("ftp"), None, Some("example.com"), None, "", None, None)
  |> uri.to_string
  |> should.equal("ftp://example.com/")

  uri.Uri(None, None, Some("example.com"), None, "", None, None)
  |> uri.to_string
  |> should.equal("//example.com/")

  uri.Uri(None, None, Some("example.com"), None, "/slash", None, None)
  |> uri.to_string
  |> should.equal("//example.com/slash")

  uri.Uri(None, None, Some("example.com"), None, "noslash", None, None)
  |> uri.to_string
  |> should.equal("//example.com/noslash")

  uri.Uri(None, None, Some(""), None, "", None, None)
  |> uri.to_string
  |> should.equal("//")

  uri.Uri(None, None, Some("example.com"), None, "noslash", None, Some("ok"))
  |> uri.to_string
  |> should.equal("//example.com/noslash#ok")

  uri.Uri(None, None, Some(""), None, "", None, Some("ok"))
  |> uri.to_string
  |> should.equal("//#ok")
}

pub fn port_to_string_test() {
  uri.Uri(Some("ftp"), None, Some("example.com"), Some(80), "", None, None)
  |> uri.to_string
  |> should.equal("ftp://example.com:80/")

  uri.Uri(None, None, Some("example.com"), Some(40), "", None, None)
  |> uri.to_string
  |> should.equal("//example.com:40/")

  uri.Uri(None, None, Some("example.com"), Some(80), "/slash", None, None)
  |> uri.to_string
  |> should.equal("//example.com:80/slash")

  uri.Uri(None, None, Some("example.com"), Some(81), "noslash", None, None)
  |> uri.to_string
  |> should.equal("//example.com:81/noslash")

  uri.Uri(None, None, None, Some(81), "noslash", None, None)
  |> uri.to_string
  |> should.equal("noslash")
}

pub fn parse_query_string_test() {
  assert Ok(parsed) = uri.parse_query("weebl+bob=1&city=%C3%B6rebro")
  should.equal(parsed, [#("weebl bob", "1"), #("city", "örebro")])

  // Duplicates keys not overridden
  assert Ok(parsed) = uri.parse_query("a[]=1&a[]=2")

  parsed
  |> should.equal([#("a[]", "1"), #("a[]", "2")])
}

pub fn parse_empty_query_string_test() {
  assert Ok(parsed) = uri.parse_query("")
  should.equal(parsed, [])
}

pub fn parse_query_string_with_empty_test() {
  uri.parse_query("present")
  |> should.equal(Ok([#("present", "")]))
}

pub fn error_parsing_query_test() {
  should.equal(uri.parse_query("%C2"), Error(Nil))
}

pub fn query_to_string_test() {
  let query_string =
    uri.query_to_string([#("weebl bob", "1"), #("city", "örebro")])
  should.equal(query_string, "weebl%20bob=1&city=%C3%B6rebro")
}

pub fn empty_query_to_string_test() {
  let query_string = uri.query_to_string([])
  should.equal(query_string, "")
}

const percent_codec_fixtures = [
  #(" ", "%20"),
  #(",", "%2C"),
  #(";", "%3B"),
  #(":", "%3A"),
  #("!", "!"),
  #("?", "%3F"),
  #("'", "'"),
  #("(", "("),
  #(")", ")"),
  #("[", "%5B"),
  #("@", "%40"),
  #("/", "%2F"),
  #("\\", "%5C"),
  #("&", "%26"),
  #("#", "%23"),
  #("=", "%3D"),
  #("~", "~"),
  #("ñ", "%C3%B1"),
  #("-", "-"),
  #("_", "_"),
  #(".", "."),
  #("*", "*"),
  #("100% great", "100%25%20great"),
]

// Allowed chars
pub fn percent_encode_test() {
  percent_codec_fixtures
  |> list.map(fn(t) {
    let #(a, b) = t
    uri.percent_encode(a)
    |> should.equal(b)
  })
}

pub fn percent_encode_consistency_test() {
  let k = "weebl bob[]"
  let v = "ñaña (,:*~)"

  let query_string = uri.query_to_string([#(k, v)])

  let encoded_key = uri.percent_encode(k)
  let encoded_value = uri.percent_encode(v)
  let manual_query_string = string.concat([encoded_key, "=", encoded_value])

  should.equal(query_string, manual_query_string)
}

pub fn percent_decode_test() {
  percent_codec_fixtures
  |> list.map(fn(t) {
    let #(a, b) = t
    uri.percent_decode(b)
    |> should.equal(Ok(a))
  })
}

pub fn percent_decode_consistency_test() {
  let k = "weebl%20bob[]"
  let v = "%C3%B6rebro"
  let query = string.concat([k, "=", v])
  assert Ok(parsed) = uri.parse_query(query)

  assert Ok(decoded_key) = uri.percent_decode(k)
  assert Ok(decoded_value) = uri.percent_decode(v)

  should.equal(parsed, [#(decoded_key, decoded_value)])
}

pub fn parse_segments_test() {
  should.equal(uri.path_segments("/"), [])
  should.equal(uri.path_segments("/weebl/bob"), ["weebl", "bob"])
  should.equal(uri.path_segments("////"), [])
  should.equal(uri.path_segments("/weebl//bob"), ["weebl", "bob"])

  should.equal(uri.path_segments("/."), [])
  should.equal(uri.path_segments("/.weebl"), [".weebl"])

  should.equal(uri.path_segments("/../bob"), ["bob"])
  should.equal(uri.path_segments("../bob"), ["bob"])
  should.equal(uri.path_segments("/weebl/../bob"), ["bob"])
}

pub fn origin1_test() {
  assert Ok(parsed) = uri.parse("http://example.test/path?weebl#bob")
  uri.origin(parsed)
  |> should.equal(Ok("http://example.test/"))
}

pub fn origin2_test() {
  assert Ok(parsed) = uri.parse("http://example.test:8080")
  uri.origin(parsed)
  |> should.equal(Ok("http://example.test:8080/"))
}

pub fn origin3_test() {
  assert Ok(parsed) = uri.parse("https://example.test")
  uri.origin(parsed)
  |> should.equal(Ok("https://example.test/"))
}

pub fn origin4_test() {
  assert Ok(parsed) = uri.parse("http:///path")
  uri.origin(parsed)
  |> should.equal(Ok("http://"))
}

pub fn origin5_test() {
  assert Ok(parsed) = uri.parse("http://")
  uri.origin(parsed)
  |> should.equal(Ok("http://"))
}

pub fn origin6_test() {
  assert Ok(parsed) = uri.parse("/path")
  uri.origin(parsed)
  |> should.equal(Error(Nil))
}

pub fn origin7_test() {
  assert Ok(parsed) = uri.parse("file:///dev/null")
  uri.origin(parsed)
  |> should.equal(Error(Nil))
}

pub fn merge1_test() {
  assert Ok(a) = uri.parse("/relative")
  assert Ok(b) = uri.parse("")
  uri.merge(a, b)
  |> should.equal(Error(Nil))
}

pub fn merge2_test() {
  assert Ok(a) = uri.parse("http://google.com/weebl")
  assert Ok(b) = uri.parse("http://example.com/baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/baz"))
}

pub fn merge3_test() {
  assert Ok(a) = uri.parse("http://google.com/weebl")
  assert Ok(b) = uri.parse("http://example.com/.././bob/../../baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/baz"))
}

pub fn merge4_test() {
  assert Ok(a) = uri.parse("http://google.com/weebl")
  assert Ok(b) = uri.parse("//example.com/baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/baz"))
}

pub fn merge5_test() {
  assert Ok(a) = uri.parse("http://google.com/weebl")
  assert Ok(b) = uri.parse("//example.com/.././bob/../../../baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/baz"))
}

pub fn merge6_test() {
  assert Ok(a) = uri.parse("http://example.com/weebl/bob")
  assert Ok(b) = uri.parse("/baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/baz"))
}

pub fn merge7_test() {
  assert Ok(a) = uri.parse("http://example.com/weebl/bob")
  assert Ok(b) = uri.parse("baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/weebl/baz"))
}

pub fn merge8_test() {
  assert Ok(a) = uri.parse("http://example.com/weebl/")
  assert Ok(b) = uri.parse("baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/weebl/baz"))
}

pub fn merge9_test() {
  assert Ok(a) = uri.parse("http://example.com")
  assert Ok(b) = uri.parse("baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/baz"))
}

pub fn merge10_test() {
  assert Ok(a) = uri.parse("http://example.com")
  assert Ok(b) = uri.parse("/.././bob/../../../baz")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/baz"))
}

pub fn merge11_test() {
  assert Ok(a) = uri.parse("http://example.com/weebl/bob")
  assert Ok(b) = uri.parse("")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/weebl/bob"))
}

pub fn merge12_test() {
  assert Ok(a) = uri.parse("http://example.com/weebl/bob")
  assert Ok(b) = uri.parse("#fragment")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/weebl/bob#fragment"))
}

pub fn merge13_test() {
  assert Ok(a) = uri.parse("http://example.com/weebl/bob")
  assert Ok(b) = uri.parse("?query")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/weebl/bob?query"))
}

pub fn merge14_test() {
  assert Ok(a) = uri.parse("http://example.com/weebl/bob?query1")
  assert Ok(b) = uri.parse("?query2")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/weebl/bob?query2"))
}

pub fn merge15_test() {
  assert Ok(a) = uri.parse("http://example.com/weebl/bob?query")
  assert Ok(b) = uri.parse("")
  uri.merge(a, b)
  |> should.equal(uri.parse("http://example.com/weebl/bob?query"))
}
