import { throwError } from "../gleam.js";
import * as $bit_string from "../gleam/bit_string.js";
import * as $string from "../gleam/string.js";
import { encode64 as do_encode64, decode64 as do_decode64 } from "../gleam_stdlib.js";

export function encode64(input, padding) {
  let encoded = do_encode64(input);
  if (padding) {
    return encoded;
  } else if (!padding) {
    return $string.replace(encoded, "=", "");
  } else {
    throwError(
      "case_no_match",
      "gleam/base",
      7,
      "encode64",
      "No case clause matched",
      { values: [padding] }
    );
  }
}

export function decode64(encoded) {
  let padded = (() => {
    let $ = $bit_string.byte_size($bit_string.from_string(encoded)) % 4;
    if ($ === 0) {
      return encoded;
    } else {
      let n = $;
      return $string.append(encoded, $string.repeat("=", 4 - n));
    }
  })();
  return do_decode64(padded);
}

export function url_encode64(input, padding) {
  let _pipe = encode64(input, padding);
  let _pipe$1 = $string.replace(_pipe, "+", "-");
  return $string.replace(_pipe$1, "/", "_");
}

export function url_decode64(encoded) {
  let _pipe = encoded;
  let _pipe$1 = $string.replace(_pipe, "-", "+");
  let _pipe$2 = $string.replace(_pipe$1, "_", "/");
  return decode64(_pipe$2);
}
