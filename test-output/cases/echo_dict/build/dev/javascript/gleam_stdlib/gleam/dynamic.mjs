import { Ok, Error, toList, prepend as listPrepend, CustomType as $CustomType } from "../gleam.mjs";
import * as $bit_array from "../gleam/bit_array.mjs";
import * as $dict from "../gleam/dict.mjs";
import * as $int from "../gleam/int.mjs";
import * as $list from "../gleam/list.mjs";
import * as $option from "../gleam/option.mjs";
import { Some } from "../gleam/option.mjs";
import * as $result from "../gleam/result.mjs";
import * as $string_builder from "../gleam/string_builder.mjs";
import {
  identity as from,
  decode_bit_array,
  classify_dynamic as do_classify,
  decode_int,
  decode_float,
  decode_bool,
  decode_list,
  decode_result,
  decode_option as decode_optional,
  decode_field,
  decode_tuple,
  decode_tuple2,
  decode_tuple3,
  decode_tuple4,
  decode_tuple5,
  decode_tuple6,
  tuple_get,
  length as tuple_size,
  decode_map,
  decode_string,
} from "../gleam_stdlib.mjs";

export { from };

export class DecodeError extends $CustomType {
  constructor(expected, found, path) {
    super();
    this.expected = expected;
    this.found = found;
    this.path = path;
  }
}

export function dynamic(value) {
  return new Ok(value);
}

export function bit_array(data) {
  return decode_bit_array(data);
}

function put_expected(error, expected) {
  return error.withFields({ expected: expected });
}

export function classify(data) {
  return do_classify(data);
}

export function int(data) {
  return decode_int(data);
}

export function float(data) {
  return decode_float(data);
}

export function bool(data) {
  return decode_bool(data);
}

export function shallow_list(value) {
  return decode_list(value);
}

export function optional(decode) {
  return (value) => { return decode_optional(value, decode); };
}

function at_least_decode_tuple_error(size, data) {
  let s = (() => {
    if (size === 1) {
      return "";
    } else {
      return "s";
    }
  })();
  let error = (() => {
    let _pipe = toList([
      "Tuple of at least ",
      $int.to_string(size),
      " element",
      s,
    ]);
    let _pipe$1 = $string_builder.from_strings(_pipe);
    let _pipe$2 = $string_builder.to_string(_pipe$1);
    return new DecodeError(_pipe$2, classify(data), toList([]));
  })();
  return new Error(toList([error]));
}

export function any(decoders) {
  return (data) => {
    if (decoders.hasLength(0)) {
      return new Error(
        toList([new DecodeError("another type", classify(data), toList([]))]),
      );
    } else {
      let decoder = decoders.head;
      let decoders$1 = decoders.tail;
      let $ = decoder(data);
      if ($.isOk()) {
        let decoded = $[0];
        return new Ok(decoded);
      } else {
        return any(decoders$1)(data);
      }
    }
  };
}

function all_errors(result) {
  if (result.isOk()) {
    return toList([]);
  } else {
    let errors = result[0];
    return errors;
  }
}

export function decode1(constructor, t1) {
  return (value) => {
    let $ = t1(value);
    if ($.isOk()) {
      let a = $[0];
      return new Ok(constructor(a));
    } else {
      let a = $;
      return new Error(all_errors(a));
    }
  };
}

function push_path(error, name) {
  let name$1 = from(name);
  let decoder = any(
    toList([string, (x) => { return $result.map(int(x), $int.to_string); }]),
  );
  let name$2 = (() => {
    let $ = decoder(name$1);
    if ($.isOk()) {
      let name$2 = $[0];
      return name$2;
    } else {
      let _pipe = toList(["<", classify(name$1), ">"]);
      let _pipe$1 = $string_builder.from_strings(_pipe);
      return $string_builder.to_string(_pipe$1);
    }
  })();
  return error.withFields({ path: listPrepend(name$2, error.path) });
}

export function result(decode_ok, decode_error) {
  return (value) => {
    return $result.try$(
      decode_result(value),
      (inner_result) => {
        if (inner_result.isOk()) {
          let raw = inner_result[0];
          return $result.try$(
            (() => {
              let _pipe = decode_ok(raw);
              return map_errors(
                _pipe,
                (_capture) => { return push_path(_capture, "ok"); },
              );
            })(),
            (value) => { return new Ok(new Ok(value)); },
          );
        } else {
          let raw = inner_result[0];
          return $result.try$(
            (() => {
              let _pipe = decode_error(raw);
              return map_errors(
                _pipe,
                (_capture) => { return push_path(_capture, "error"); },
              );
            })(),
            (value) => { return new Ok(new Error(value)); },
          );
        }
      },
    );
  };
}

export function list(decoder_type) {
  return (dynamic) => {
    return $result.try$(
      shallow_list(dynamic),
      (list) => {
        let _pipe = list;
        let _pipe$1 = $list.try_map(_pipe, decoder_type);
        return map_errors(
          _pipe$1,
          (_capture) => { return push_path(_capture, "*"); },
        );
      },
    );
  };
}

function map_errors(result, f) {
  return $result.map_error(
    result,
    (_capture) => { return $list.map(_capture, f); },
  );
}

export function string(data) {
  return decode_string(data);
}

export function field(name, inner_type) {
  return (value) => {
    let missing_field_error = new DecodeError("field", "nothing", toList([]));
    return $result.try$(
      decode_field(value, name),
      (maybe_inner) => {
        let _pipe = maybe_inner;
        let _pipe$1 = $option.to_result(_pipe, toList([missing_field_error]));
        let _pipe$2 = $result.try$(_pipe$1, inner_type);
        return map_errors(
          _pipe$2,
          (_capture) => { return push_path(_capture, name); },
        );
      },
    );
  };
}

export function optional_field(name, inner_type) {
  return (value) => {
    return $result.try$(
      decode_field(value, name),
      (maybe_inner) => {
        if (maybe_inner instanceof $option.None) {
          return new Ok(new $option.None());
        } else {
          let dynamic_inner = maybe_inner[0];
          let _pipe = inner_type(dynamic_inner);
          let _pipe$1 = $result.map(_pipe, (var0) => { return new Some(var0); });
          return map_errors(
            _pipe$1,
            (_capture) => { return push_path(_capture, name); },
          );
        }
      },
    );
  };
}

export function element(index, inner_type) {
  return (data) => {
    return $result.try$(
      decode_tuple(data),
      (tuple) => {
        let size = tuple_size(tuple);
        return $result.try$(
          (() => {
            let $ = index >= 0;
            if ($) {
              let $1 = index < size;
              if ($1) {
                return tuple_get(tuple, index);
              } else {
                return at_least_decode_tuple_error(index + 1, data);
              }
            } else {
              let $1 = $int.absolute_value(index) <= size;
              if ($1) {
                return tuple_get(tuple, size + index);
              } else {
                return at_least_decode_tuple_error(
                  $int.absolute_value(index),
                  data,
                );
              }
            }
          })(),
          (data) => {
            let _pipe = inner_type(data);
            return map_errors(
              _pipe,
              (_capture) => { return push_path(_capture, index); },
            );
          },
        );
      },
    );
  };
}

function tuple_errors(result, name) {
  if (result.isOk()) {
    return toList([]);
  } else {
    let errors = result[0];
    return $list.map(
      errors,
      (_capture) => { return push_path(_capture, name); },
    );
  }
}

export function tuple2(decode1, decode2) {
  return (value) => {
    return $result.try$(
      decode_tuple2(value),
      (_use0) => {
        let a = _use0[0];
        let b = _use0[1];
        let $ = decode1(a);
        let $1 = decode2(b);
        if ($.isOk() && $1.isOk()) {
          let a$1 = $[0];
          let b$1 = $1[0];
          return new Ok([a$1, b$1]);
        } else {
          let a$1 = $;
          let b$1 = $1;
          let _pipe = tuple_errors(a$1, "0");
          let _pipe$1 = $list.append(_pipe, tuple_errors(b$1, "1"));
          return new Error(_pipe$1);
        }
      },
    );
  };
}

export function tuple3(decode1, decode2, decode3) {
  return (value) => {
    return $result.try$(
      decode_tuple3(value),
      (_use0) => {
        let a = _use0[0];
        let b = _use0[1];
        let c = _use0[2];
        let $ = decode1(a);
        let $1 = decode2(b);
        let $2 = decode3(c);
        if ($.isOk() && $1.isOk() && $2.isOk()) {
          let a$1 = $[0];
          let b$1 = $1[0];
          let c$1 = $2[0];
          return new Ok([a$1, b$1, c$1]);
        } else {
          let a$1 = $;
          let b$1 = $1;
          let c$1 = $2;
          let _pipe = tuple_errors(a$1, "0");
          let _pipe$1 = $list.append(_pipe, tuple_errors(b$1, "1"));
          let _pipe$2 = $list.append(_pipe$1, tuple_errors(c$1, "2"));
          return new Error(_pipe$2);
        }
      },
    );
  };
}

export function tuple4(decode1, decode2, decode3, decode4) {
  return (value) => {
    return $result.try$(
      decode_tuple4(value),
      (_use0) => {
        let a = _use0[0];
        let b = _use0[1];
        let c = _use0[2];
        let d = _use0[3];
        let $ = decode1(a);
        let $1 = decode2(b);
        let $2 = decode3(c);
        let $3 = decode4(d);
        if ($.isOk() && $1.isOk() && $2.isOk() && $3.isOk()) {
          let a$1 = $[0];
          let b$1 = $1[0];
          let c$1 = $2[0];
          let d$1 = $3[0];
          return new Ok([a$1, b$1, c$1, d$1]);
        } else {
          let a$1 = $;
          let b$1 = $1;
          let c$1 = $2;
          let d$1 = $3;
          let _pipe = tuple_errors(a$1, "0");
          let _pipe$1 = $list.append(_pipe, tuple_errors(b$1, "1"));
          let _pipe$2 = $list.append(_pipe$1, tuple_errors(c$1, "2"));
          let _pipe$3 = $list.append(_pipe$2, tuple_errors(d$1, "3"));
          return new Error(_pipe$3);
        }
      },
    );
  };
}

export function tuple5(decode1, decode2, decode3, decode4, decode5) {
  return (value) => {
    return $result.try$(
      decode_tuple5(value),
      (_use0) => {
        let a = _use0[0];
        let b = _use0[1];
        let c = _use0[2];
        let d = _use0[3];
        let e = _use0[4];
        let $ = decode1(a);
        let $1 = decode2(b);
        let $2 = decode3(c);
        let $3 = decode4(d);
        let $4 = decode5(e);
        if ($.isOk() && $1.isOk() && $2.isOk() && $3.isOk() && $4.isOk()) {
          let a$1 = $[0];
          let b$1 = $1[0];
          let c$1 = $2[0];
          let d$1 = $3[0];
          let e$1 = $4[0];
          return new Ok([a$1, b$1, c$1, d$1, e$1]);
        } else {
          let a$1 = $;
          let b$1 = $1;
          let c$1 = $2;
          let d$1 = $3;
          let e$1 = $4;
          let _pipe = tuple_errors(a$1, "0");
          let _pipe$1 = $list.append(_pipe, tuple_errors(b$1, "1"));
          let _pipe$2 = $list.append(_pipe$1, tuple_errors(c$1, "2"));
          let _pipe$3 = $list.append(_pipe$2, tuple_errors(d$1, "3"));
          let _pipe$4 = $list.append(_pipe$3, tuple_errors(e$1, "4"));
          return new Error(_pipe$4);
        }
      },
    );
  };
}

export function tuple6(decode1, decode2, decode3, decode4, decode5, decode6) {
  return (value) => {
    return $result.try$(
      decode_tuple6(value),
      (_use0) => {
        let a = _use0[0];
        let b = _use0[1];
        let c = _use0[2];
        let d = _use0[3];
        let e = _use0[4];
        let f = _use0[5];
        let $ = decode1(a);
        let $1 = decode2(b);
        let $2 = decode3(c);
        let $3 = decode4(d);
        let $4 = decode5(e);
        let $5 = decode6(f);
        if ($.isOk() &&
        $1.isOk() &&
        $2.isOk() &&
        $3.isOk() &&
        $4.isOk() &&
        $5.isOk()) {
          let a$1 = $[0];
          let b$1 = $1[0];
          let c$1 = $2[0];
          let d$1 = $3[0];
          let e$1 = $4[0];
          let f$1 = $5[0];
          return new Ok([a$1, b$1, c$1, d$1, e$1, f$1]);
        } else {
          let a$1 = $;
          let b$1 = $1;
          let c$1 = $2;
          let d$1 = $3;
          let e$1 = $4;
          let f$1 = $5;
          let _pipe = tuple_errors(a$1, "0");
          let _pipe$1 = $list.append(_pipe, tuple_errors(b$1, "1"));
          let _pipe$2 = $list.append(_pipe$1, tuple_errors(c$1, "2"));
          let _pipe$3 = $list.append(_pipe$2, tuple_errors(d$1, "3"));
          let _pipe$4 = $list.append(_pipe$3, tuple_errors(e$1, "4"));
          let _pipe$5 = $list.append(_pipe$4, tuple_errors(f$1, "5"));
          return new Error(_pipe$5);
        }
      },
    );
  };
}

export function dict(key_type, value_type) {
  return (value) => {
    return $result.try$(
      decode_map(value),
      (map) => {
        return $result.try$(
          (() => {
            let _pipe = map;
            let _pipe$1 = $dict.to_list(_pipe);
            return $list.try_map(
              _pipe$1,
              (pair) => {
                let k = pair[0];
                let v = pair[1];
                return $result.try$(
                  (() => {
                    let _pipe$2 = key_type(k);
                    return map_errors(
                      _pipe$2,
                      (_capture) => { return push_path(_capture, "keys"); },
                    );
                  })(),
                  (k) => {
                    return $result.try$(
                      (() => {
                        let _pipe$2 = value_type(v);
                        return map_errors(
                          _pipe$2,
                          (_capture) => { return push_path(_capture, "values"); },
                        );
                      })(),
                      (v) => { return new Ok([k, v]); },
                    );
                  },
                );
              },
            );
          })(),
          (pairs) => { return new Ok($dict.from_list(pairs)); },
        );
      },
    );
  };
}

export function decode2(constructor, t1, t2) {
  return (value) => {
    let $ = t1(value);
    let $1 = t2(value);
    if ($.isOk() && $1.isOk()) {
      let a = $[0];
      let b = $1[0];
      return new Ok(constructor(a, b));
    } else {
      let a = $;
      let b = $1;
      return new Error($list.flatten(toList([all_errors(a), all_errors(b)])));
    }
  };
}

export function decode3(constructor, t1, t2, t3) {
  return (value) => {
    let $ = t1(value);
    let $1 = t2(value);
    let $2 = t3(value);
    if ($.isOk() && $1.isOk() && $2.isOk()) {
      let a = $[0];
      let b = $1[0];
      let c = $2[0];
      return new Ok(constructor(a, b, c));
    } else {
      let a = $;
      let b = $1;
      let c = $2;
      return new Error(
        $list.flatten(toList([all_errors(a), all_errors(b), all_errors(c)])),
      );
    }
  };
}

export function decode4(constructor, t1, t2, t3, t4) {
  return (x) => {
    let $ = t1(x);
    let $1 = t2(x);
    let $2 = t3(x);
    let $3 = t4(x);
    if ($.isOk() && $1.isOk() && $2.isOk() && $3.isOk()) {
      let a = $[0];
      let b = $1[0];
      let c = $2[0];
      let d = $3[0];
      return new Ok(constructor(a, b, c, d));
    } else {
      let a = $;
      let b = $1;
      let c = $2;
      let d = $3;
      return new Error(
        $list.flatten(
          toList([all_errors(a), all_errors(b), all_errors(c), all_errors(d)]),
        ),
      );
    }
  };
}

export function decode5(constructor, t1, t2, t3, t4, t5) {
  return (x) => {
    let $ = t1(x);
    let $1 = t2(x);
    let $2 = t3(x);
    let $3 = t4(x);
    let $4 = t5(x);
    if ($.isOk() && $1.isOk() && $2.isOk() && $3.isOk() && $4.isOk()) {
      let a = $[0];
      let b = $1[0];
      let c = $2[0];
      let d = $3[0];
      let e = $4[0];
      return new Ok(constructor(a, b, c, d, e));
    } else {
      let a = $;
      let b = $1;
      let c = $2;
      let d = $3;
      let e = $4;
      return new Error(
        $list.flatten(
          toList([
            all_errors(a),
            all_errors(b),
            all_errors(c),
            all_errors(d),
            all_errors(e),
          ]),
        ),
      );
    }
  };
}

export function decode6(constructor, t1, t2, t3, t4, t5, t6) {
  return (x) => {
    let $ = t1(x);
    let $1 = t2(x);
    let $2 = t3(x);
    let $3 = t4(x);
    let $4 = t5(x);
    let $5 = t6(x);
    if ($.isOk() &&
    $1.isOk() &&
    $2.isOk() &&
    $3.isOk() &&
    $4.isOk() &&
    $5.isOk()) {
      let a = $[0];
      let b = $1[0];
      let c = $2[0];
      let d = $3[0];
      let e = $4[0];
      let f = $5[0];
      return new Ok(constructor(a, b, c, d, e, f));
    } else {
      let a = $;
      let b = $1;
      let c = $2;
      let d = $3;
      let e = $4;
      let f = $5;
      return new Error(
        $list.flatten(
          toList([
            all_errors(a),
            all_errors(b),
            all_errors(c),
            all_errors(d),
            all_errors(e),
            all_errors(f),
          ]),
        ),
      );
    }
  };
}

export function decode7(constructor, t1, t2, t3, t4, t5, t6, t7) {
  return (x) => {
    let $ = t1(x);
    let $1 = t2(x);
    let $2 = t3(x);
    let $3 = t4(x);
    let $4 = t5(x);
    let $5 = t6(x);
    let $6 = t7(x);
    if ($.isOk() &&
    $1.isOk() &&
    $2.isOk() &&
    $3.isOk() &&
    $4.isOk() &&
    $5.isOk() &&
    $6.isOk()) {
      let a = $[0];
      let b = $1[0];
      let c = $2[0];
      let d = $3[0];
      let e = $4[0];
      let f = $5[0];
      let g = $6[0];
      return new Ok(constructor(a, b, c, d, e, f, g));
    } else {
      let a = $;
      let b = $1;
      let c = $2;
      let d = $3;
      let e = $4;
      let f = $5;
      let g = $6;
      return new Error(
        $list.flatten(
          toList([
            all_errors(a),
            all_errors(b),
            all_errors(c),
            all_errors(d),
            all_errors(e),
            all_errors(f),
            all_errors(g),
          ]),
        ),
      );
    }
  };
}

export function decode8(constructor, t1, t2, t3, t4, t5, t6, t7, t8) {
  return (x) => {
    let $ = t1(x);
    let $1 = t2(x);
    let $2 = t3(x);
    let $3 = t4(x);
    let $4 = t5(x);
    let $5 = t6(x);
    let $6 = t7(x);
    let $7 = t8(x);
    if ($.isOk() &&
    $1.isOk() &&
    $2.isOk() &&
    $3.isOk() &&
    $4.isOk() &&
    $5.isOk() &&
    $6.isOk() &&
    $7.isOk()) {
      let a = $[0];
      let b = $1[0];
      let c = $2[0];
      let d = $3[0];
      let e = $4[0];
      let f = $5[0];
      let g = $6[0];
      let h = $7[0];
      return new Ok(constructor(a, b, c, d, e, f, g, h));
    } else {
      let a = $;
      let b = $1;
      let c = $2;
      let d = $3;
      let e = $4;
      let f = $5;
      let g = $6;
      let h = $7;
      return new Error(
        $list.flatten(
          toList([
            all_errors(a),
            all_errors(b),
            all_errors(c),
            all_errors(d),
            all_errors(e),
            all_errors(f),
            all_errors(g),
            all_errors(h),
          ]),
        ),
      );
    }
  };
}

export function decode9(constructor, t1, t2, t3, t4, t5, t6, t7, t8, t9) {
  return (x) => {
    let $ = t1(x);
    let $1 = t2(x);
    let $2 = t3(x);
    let $3 = t4(x);
    let $4 = t5(x);
    let $5 = t6(x);
    let $6 = t7(x);
    let $7 = t8(x);
    let $8 = t9(x);
    if ($.isOk() &&
    $1.isOk() &&
    $2.isOk() &&
    $3.isOk() &&
    $4.isOk() &&
    $5.isOk() &&
    $6.isOk() &&
    $7.isOk() &&
    $8.isOk()) {
      let a = $[0];
      let b = $1[0];
      let c = $2[0];
      let d = $3[0];
      let e = $4[0];
      let f = $5[0];
      let g = $6[0];
      let h = $7[0];
      let i = $8[0];
      return new Ok(constructor(a, b, c, d, e, f, g, h, i));
    } else {
      let a = $;
      let b = $1;
      let c = $2;
      let d = $3;
      let e = $4;
      let f = $5;
      let g = $6;
      let h = $7;
      let i = $8;
      return new Error(
        $list.flatten(
          toList([
            all_errors(a),
            all_errors(b),
            all_errors(c),
            all_errors(d),
            all_errors(e),
            all_errors(f),
            all_errors(g),
            all_errors(h),
            all_errors(i),
          ]),
        ),
      );
    }
  };
}
