import { Ok, Error, toList, CustomType as $CustomType, makeError, isEqual } from "../gleam.mjs";
import * as $int from "../gleam/int.mjs";
import * as $list from "../gleam/list.mjs";
import * as $option from "../gleam/option.mjs";
import { None, Some } from "../gleam/option.mjs";
import * as $pair from "../gleam/pair.mjs";
import * as $regex from "../gleam/regex.mjs";
import * as $result from "../gleam/result.mjs";
import * as $string from "../gleam/string.mjs";
import * as $string_builder from "../gleam/string_builder.mjs";
import {
  parse_query as do_parse_query,
  percent_encode as do_percent_encode,
  percent_decode as do_percent_decode,
} from "../gleam_stdlib.mjs";

export class Uri extends $CustomType {
  constructor(scheme, userinfo, host, port, path, query, fragment) {
    super();
    this.scheme = scheme;
    this.userinfo = userinfo;
    this.host = host;
    this.port = port;
    this.path = path;
    this.query = query;
    this.fragment = fragment;
  }
}

function regex_submatches(pattern, string) {
  let _pipe = pattern;
  let _pipe$1 = $regex.compile(_pipe, new $regex.Options(true, false));
  let _pipe$2 = $result.nil_error(_pipe$1);
  let _pipe$3 = $result.map(
    _pipe$2,
    (_capture) => { return $regex.scan(_capture, string); },
  );
  let _pipe$4 = $result.try$(_pipe$3, $list.first);
  let _pipe$5 = $result.map(_pipe$4, (m) => { return m.submatches; });
  return $result.unwrap(_pipe$5, toList([]));
}

function noneify_query(x) {
  if (x instanceof None) {
    return new None();
  } else if (x instanceof Some) {
    let x$1 = x[0];
    let $ = $string.pop_grapheme(x$1);
    if ($.isOk() && $[0][0] === "?") {
      let query = $[0][1];
      return new Some(query);
    } else {
      return new None();
    }
  } else {
    throw makeError(
      "case_no_match",
      "gleam/uri",
      136,
      "noneify_query",
      "No case clause matched",
      { values: [x] }
    )
  }
}

function noneify_empty_string(x) {
  if (x instanceof Some && x[0] === "") {
    return new None();
  } else if (x instanceof None) {
    return new None();
  } else if (x instanceof Some) {
    return x;
  } else {
    throw makeError(
      "case_no_match",
      "gleam/uri",
      148,
      "noneify_empty_string",
      "No case clause matched",
      { values: [x] }
    )
  }
}

function extra_required(loop$list, loop$remaining) {
  while (true) {
    let list = loop$list;
    let remaining = loop$remaining;
    if (remaining === 0) {
      return 0;
    } else if (list.hasLength(0)) {
      return remaining;
    } else if (list.atLeastLength(1)) {
      let xs = list.tail;
      loop$list = xs;
      loop$remaining = remaining - 1;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/uri",
        192,
        "extra_required",
        "No case clause matched",
        { values: [list] }
      )
    }
  }
}

function pad_list(list, size) {
  let _pipe = list;
  return $list.append(
    _pipe,
    $list.repeat(new None(), extra_required(list, size)),
  );
}

function split_authority(authority) {
  let $ = $option.unwrap(authority, "");
  if ($ === "") {
    return [new None(), new None(), new None()];
  } else if ($ === "//") {
    return [new None(), new Some(""), new None()];
  } else {
    let authority$1 = $;
    let matches = (() => {
      let _pipe = "^(//)?((.*)@)?(\\[[a-zA-Z0-9:.]*\\]|[^:]*)(:(\\d*))?";
      let _pipe$1 = regex_submatches(_pipe, authority$1);
      return pad_list(_pipe$1, 6);
    })();
    if (matches.hasLength(6)) {
      let userinfo = matches.tail.tail.head;
      let host = matches.tail.tail.tail.head;
      let port = matches.tail.tail.tail.tail.tail.head;
      let userinfo$1 = noneify_empty_string(userinfo);
      let host$1 = noneify_empty_string(host);
      let port$1 = (() => {
        let _pipe = port;
        let _pipe$1 = $option.unwrap(_pipe, "");
        let _pipe$2 = $int.parse(_pipe$1);
        return $option.from_result(_pipe$2);
      })();
      return [userinfo$1, host$1, port$1];
    } else {
      return [new None(), new None(), new None()];
    }
  }
}

function do_parse(uri_string) {
  let pattern = "^(([a-z][a-z0-9\\+\\-\\.]*):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#.*)?";
  let matches = (() => {
    let _pipe = pattern;
    let _pipe$1 = regex_submatches(_pipe, uri_string);
    return pad_list(_pipe$1, 8);
  })();
  let $ = (() => {
    if (matches.hasLength(8)) {
      let scheme = matches.tail.head;
      let authority_with_slashes = matches.tail.tail.head;
      let path = matches.tail.tail.tail.tail.head;
      let query_with_question_mark = matches.tail.tail.tail.tail.tail.head;
      let fragment = matches.tail.tail.tail.tail.tail.tail.tail.head;
      return [
        scheme,
        authority_with_slashes,
        path,
        query_with_question_mark,
        fragment,
      ];
    } else {
      return [new None(), new None(), new None(), new None(), new None()];
    }
  })();
  let scheme = $[0];
  let authority = $[1];
  let path = $[2];
  let query = $[3];
  let fragment = $[4];
  let scheme$1 = noneify_empty_string(scheme);
  let path$1 = $option.unwrap(path, "");
  let query$1 = noneify_query(query);
  let $1 = split_authority(authority);
  let userinfo = $1[0];
  let host = $1[1];
  let port = $1[2];
  let fragment$1 = (() => {
    let _pipe = fragment;
    let _pipe$1 = $option.to_result(_pipe, undefined);
    let _pipe$2 = $result.try$(_pipe$1, $string.pop_grapheme);
    let _pipe$3 = $result.map(_pipe$2, $pair.second);
    return $option.from_result(_pipe$3);
  })();
  let scheme$2 = (() => {
    let _pipe = scheme$1;
    let _pipe$1 = noneify_empty_string(_pipe);
    return $option.map(_pipe$1, $string.lowercase);
  })();
  return new Ok(
    new Uri(scheme$2, userinfo, host, port, path$1, query$1, fragment$1),
  );
}

export function parse(uri_string) {
  return do_parse(uri_string);
}

export function parse_query(query) {
  return do_parse_query(query);
}

export function percent_encode(value) {
  return do_percent_encode(value);
}

function query_pair(pair) {
  return $string_builder.from_strings(
    toList([percent_encode(pair[0]), "=", percent_encode(pair[1])]),
  );
}

export function query_to_string(query) {
  let _pipe = query;
  let _pipe$1 = $list.map(_pipe, query_pair);
  let _pipe$2 = $list.intersperse(_pipe$1, $string_builder.from_string("&"));
  let _pipe$3 = $string_builder.concat(_pipe$2);
  return $string_builder.to_string(_pipe$3);
}

export function percent_decode(value) {
  return do_percent_decode(value);
}

function do_remove_dot_segments(loop$input, loop$accumulator) {
  while (true) {
    let input = loop$input;
    let accumulator = loop$accumulator;
    if (input.hasLength(0)) {
      return $list.reverse(accumulator);
    } else if (input.atLeastLength(1)) {
      let segment = input.head;
      let rest = input.tail;
      let accumulator$1 = (() => {
        if (segment === "") {
          let accumulator$1 = accumulator;
          return accumulator$1;
        } else if (segment === ".") {
          let accumulator$1 = accumulator;
          return accumulator$1;
        } else if (segment === ".." && accumulator.hasLength(0)) {
          return toList([]);
        } else if (segment === ".." && accumulator.atLeastLength(1)) {
          let accumulator$1 = accumulator.tail;
          return accumulator$1;
        } else {
          let segment$1 = segment;
          let accumulator$1 = accumulator;
          return toList([segment$1], accumulator$1);
        }
      })();
      loop$input = rest;
      loop$accumulator = accumulator$1;
    } else {
      throw makeError(
        "case_no_match",
        "gleam/uri",
        284,
        "do_remove_dot_segments",
        "No case clause matched",
        { values: [input] }
      )
    }
  }
}

function remove_dot_segments(input) {
  return do_remove_dot_segments(input, toList([]));
}

export function path_segments(path) {
  return remove_dot_segments($string.split(path, "/"));
}

export function to_string(uri) {
  let parts = (() => {
    let $ = uri.fragment;
    if ($ instanceof Some) {
      let fragment = $[0];
      return toList(["#", fragment]);
    } else {
      return toList([]);
    }
  })();
  let parts$1 = (() => {
    let $ = uri.query;
    if ($ instanceof Some) {
      let query = $[0];
      return toList(["?", query], parts);
    } else {
      return parts;
    }
  })();
  let parts$2 = toList([uri.path], parts$1);
  let parts$3 = (() => {
    let $ = uri.host;
    let $1 = $string.starts_with(uri.path, "/");
    if ($ instanceof Some && !$1 && $[0] !== "") {
      let host = $[0];
      return toList(["/"], parts$2);
    } else {
      return parts$2;
    }
  })();
  let parts$4 = (() => {
    let $ = uri.host;
    let $1 = uri.port;
    if ($ instanceof Some && $1 instanceof Some) {
      let port = $1[0];
      return toList([":", $int.to_string(port)], parts$3);
    } else {
      return parts$3;
    }
  })();
  let parts$5 = (() => {
    let $ = uri.scheme;
    let $1 = uri.userinfo;
    let $2 = uri.host;
    if ($ instanceof Some && $1 instanceof Some && $2 instanceof Some) {
      let s = $[0];
      let u = $1[0];
      let h = $2[0];
      return toList([s, "://", u, "@", h], parts$4);
    } else if ($ instanceof Some && $1 instanceof None && $2 instanceof Some) {
      let s = $[0];
      let h = $2[0];
      return toList([s, "://", h], parts$4);
    } else if ($ instanceof Some && $1 instanceof Some && $2 instanceof None) {
      let s = $[0];
      return toList([s, ":"], parts$4);
    } else if ($ instanceof Some && $1 instanceof None && $2 instanceof None) {
      let s = $[0];
      return toList([s, ":"], parts$4);
    } else if ($ instanceof None && $1 instanceof None && $2 instanceof Some) {
      let h = $2[0];
      return toList(["//", h], parts$4);
    } else {
      return parts$4;
    }
  })();
  return $string.concat(parts$5);
}

export function origin(uri) {
  if (!(uri instanceof Uri)) {
    throw makeError(
      "assignment_no_match",
      "gleam/uri",
      376,
      "origin",
      "Assignment pattern did not match",
      { value: uri }
    )
  }
  let scheme = uri.scheme;
  let host = uri.host;
  let port = uri.port;
  if (scheme instanceof Some &&
  scheme[0] === "https" &&
  isEqual(port, new Some(443))) {
    let origin$1 = new Uri(
      scheme,
      new None(),
      host,
      new None(),
      "",
      new None(),
      new None(),
    );
    return new Ok(to_string(origin$1));
  } else if (scheme instanceof Some &&
  scheme[0] === "http" &&
  isEqual(port, new Some(80))) {
    let origin$1 = new Uri(
      scheme,
      new None(),
      host,
      new None(),
      "",
      new None(),
      new None(),
    );
    return new Ok(to_string(origin$1));
  } else if (scheme instanceof Some &&
  (scheme[0] === "http") || (scheme[0] === "https")) {
    let s = scheme[0];
    let origin$1 = new Uri(
      scheme,
      new None(),
      host,
      port,
      "",
      new None(),
      new None(),
    );
    return new Ok(to_string(origin$1));
  } else {
    return new Error(undefined);
  }
}

function drop_last(elements) {
  return $list.take(elements, $list.length(elements) - 1);
}

function join_segments(segments) {
  return $string.join(toList([""], segments), "/");
}

export function merge(base, relative) {
  if (base instanceof Uri &&
  base.scheme instanceof Some &&
  base.host instanceof Some) {
    if (relative instanceof Uri && relative.host instanceof Some) {
      let path = (() => {
        let _pipe = $string.split(relative.path, "/");
        let _pipe$1 = remove_dot_segments(_pipe);
        return join_segments(_pipe$1);
      })();
      let resolved = new Uri(
        $option.or(relative.scheme, base.scheme),
        new None(),
        relative.host,
        $option.or(relative.port, base.port),
        path,
        relative.query,
        relative.fragment,
      );
      return new Ok(resolved);
    } else {
      let $ = (() => {
        let $1 = relative.path;
        if ($1 === "") {
          return [base.path, $option.or(relative.query, base.query)];
        } else {
          let path_segments$1 = (() => {
            let $2 = $string.starts_with(relative.path, "/");
            if ($2) {
              return $string.split(relative.path, "/");
            } else if (!$2) {
              let _pipe = $string.split(base.path, "/");
              let _pipe$1 = drop_last(_pipe);
              return $list.append(_pipe$1, $string.split(relative.path, "/"));
            } else {
              throw makeError(
                "case_no_match",
                "gleam/uri",
                433,
                "merge",
                "No case clause matched",
                { values: [$2] }
              )
            }
          })();
          let path = (() => {
            let _pipe = path_segments$1;
            let _pipe$1 = remove_dot_segments(_pipe);
            return join_segments(_pipe$1);
          })();
          return [path, relative.query];
        }
      })();
      let new_path = $[0];
      let new_query = $[1];
      let resolved = new Uri(
        base.scheme,
        new None(),
        base.host,
        base.port,
        new_path,
        new_query,
        relative.fragment,
      );
      return new Ok(resolved);
    }
  } else {
    return new Error(undefined);
  }
}
