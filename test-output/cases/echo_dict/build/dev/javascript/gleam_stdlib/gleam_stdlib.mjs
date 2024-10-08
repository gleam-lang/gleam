import {
  BitArray,
  Error,
  List,
  Ok,
  Result,
  UtfCodepoint,
  stringBits,
  toBitArray,
  NonEmpty,
  CustomType,
} from "./gleam.mjs";
import {
  CompileError as RegexCompileError,
  Match as RegexMatch,
} from "./gleam/regex.mjs";
import { DecodeError } from "./gleam/dynamic.mjs";
import { Some, None } from "./gleam/option.mjs";
import { Eq, Gt, Lt } from "./gleam/order.mjs";
import Dict from "./dict.mjs";

const Nil = undefined;
const NOT_FOUND = {};

export function identity(x) {
  return x;
}

export function parse_int(value) {
  if (/^[-+]?(\d+)$/.test(value)) {
    return new Ok(parseInt(value));
  } else {
    return new Error(Nil);
  }
}

export function parse_float(value) {
  if (/^[-+]?(\d+)\.(\d+)([eE][-+]?\d+)?$/.test(value)) {
    return new Ok(parseFloat(value));
  } else {
    return new Error(Nil);
  }
}

export function to_string(term) {
  return term.toString();
}

export function float_to_string(float) {
  const string = float.toString().replace('+', '');
  if (string.indexOf(".") >= 0) {
    return string;
  } else {
    const index = string.indexOf("e");
    if (index >= 0) {
      return string.slice(0, index) + '.0' + string.slice(index);
    } else {
      return string + ".0";
    }
  }
}

export function int_to_base_string(int, base) {
  return int.toString(base).toUpperCase();
}

const int_base_patterns = {
  2: /[^0-1]/,
  3: /[^0-2]/,
  4: /[^0-3]/,
  5: /[^0-4]/,
  6: /[^0-5]/,
  7: /[^0-6]/,
  8: /[^0-7]/,
  9: /[^0-8]/,
  10: /[^0-9]/,
  11: /[^0-9a]/,
  12: /[^0-9a-b]/,
  13: /[^0-9a-c]/,
  14: /[^0-9a-d]/,
  15: /[^0-9a-e]/,
  16: /[^0-9a-f]/,
  17: /[^0-9a-g]/,
  18: /[^0-9a-h]/,
  19: /[^0-9a-i]/,
  20: /[^0-9a-j]/,
  21: /[^0-9a-k]/,
  22: /[^0-9a-l]/,
  23: /[^0-9a-m]/,
  24: /[^0-9a-n]/,
  25: /[^0-9a-o]/,
  26: /[^0-9a-p]/,
  27: /[^0-9a-q]/,
  28: /[^0-9a-r]/,
  29: /[^0-9a-s]/,
  30: /[^0-9a-t]/,
  31: /[^0-9a-u]/,
  32: /[^0-9a-v]/,
  33: /[^0-9a-w]/,
  34: /[^0-9a-x]/,
  35: /[^0-9a-y]/,
  36: /[^0-9a-z]/,
};

export function int_from_base_string(string, base) {
  if (int_base_patterns[base].test(string.replace(/^-/, "").toLowerCase())) {
    return new Error(Nil);
  }

  const result = parseInt(string, base);

  if (isNaN(result)) {
    return new Error(Nil);
  }

  return new Ok(result);
}

export function string_replace(string, target, substitute) {
  if (typeof string.replaceAll !== "undefined") {
    return string.replaceAll(target, substitute);
  }
  // Fallback for older Node.js versions:
  // 1. <https://stackoverflow.com/a/1144788>
  // 2. <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#escaping>
  // TODO: This fallback could be remove once Node.js 14 is EOL
  // aka <https://nodejs.org/en/about/releases/> on or after 2024-04-30
  return string.replace(
    // $& means the whole matched string
    new RegExp(target.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"), "g"),
    substitute,
  );
}

export function string_reverse(string) {
  return [...string].reverse().join("");
}

export function string_length(string) {
  if (string === "") {
    return 0;
  }
  const iterator = graphemes_iterator(string);
  if (iterator) {
    let i = 0;
    for (const _ of iterator) {
      i++;
    }
    return i;
  } else {
    return string.match(/./gsu).length;
  }
}

export function graphemes(string) {
  const iterator = graphemes_iterator(string);
  if (iterator) {
    return List.fromArray(Array.from(iterator).map((item) => item.segment));
  } else {
    return List.fromArray(string.match(/./gsu));
  }
}

let segmenter = undefined;

function graphemes_iterator(string) {
  if (globalThis.Intl && Intl.Segmenter) {
    segmenter ||= new Intl.Segmenter();
    return segmenter.segment(string)[Symbol.iterator]();
  }
}

export function pop_grapheme(string) {
  let first;
  const iterator = graphemes_iterator(string);
  if (iterator) {
    first = iterator.next().value?.segment;
  } else {
    first = string.match(/./su)?.[0];
  }
  if (first) {
    return new Ok([first, string.slice(first.length)]);
  } else {
    return new Error(Nil);
  }
}

export function lowercase(string) {
  return string.toLowerCase();
}

export function uppercase(string) {
  return string.toUpperCase();
}

export function less_than(a, b) {
  return a < b;
}

export function add(a, b) {
  return a + b;
}

export function split(xs, pattern) {
  return List.fromArray(xs.split(pattern));
}

export function join(xs, separator) {
  const iterator = xs[Symbol.iterator]();
  let result = iterator.next().value || "";
  let current = iterator.next();
  while (!current.done) {
    result = result + separator + current.value;
    current = iterator.next();
  }
  return result;
}

export function concat(xs) {
  let result = "";
  for (const x of xs) {
    result = result + x;
  }
  return result;
}

export function length(data) {
  return data.length;
}

export function string_slice(string, idx, len) {
  if (len <= 0 || idx >= string.length) {
    return "";
  }

  const iterator = graphemes_iterator(string);
  if (iterator) {
    while (idx-- > 0) {
      iterator.next();
    }

    let result = "";

    while (len-- > 0) {
      const v = iterator.next().value;
      if (v === undefined) {
        break;
      }

      result += v.segment;
    }

    return result;
  } else {
    return string.match(/./gsu).slice(idx, idx + len).join("");
  }
}

export function crop_string(string, substring) {
  return string.substring(string.indexOf(substring));
}

export function contains_string(haystack, needle) {
  return haystack.indexOf(needle) >= 0;
}

export function starts_with(haystack, needle) {
  return haystack.startsWith(needle);
}

export function ends_with(haystack, needle) {
  return haystack.endsWith(needle);
}

export function split_once(haystack, needle) {
  const index = haystack.indexOf(needle);
  if (index >= 0) {
    const before = haystack.slice(0, index);
    const after = haystack.slice(index + needle.length);
    return new Ok([before, after]);
  } else {
    return new Error(Nil);
  }
}

const unicode_whitespaces = [
  "\u0020", // Space
  "\u0009", // Horizontal tab
  "\u000A", // Line feed
  "\u000B", // Vertical tab
  "\u000C", // Form feed
  "\u000D", // Carriage return
  "\u0085", // Next line
  "\u2028", // Line separator
  "\u2029", // Paragraph separator
].join("");

const left_trim_regex = new RegExp(`^([${unicode_whitespaces}]*)`, "g");
const right_trim_regex = new RegExp(`([${unicode_whitespaces}]*)$`, "g");

export function trim(string) {
  return trim_left(trim_right(string));
}

export function trim_left(string) {
  return string.replace(left_trim_regex, "");
}

export function trim_right(string) {
  return string.replace(right_trim_regex, "");
}

export function bit_array_from_string(string) {
  return toBitArray([stringBits(string)]);
}

export function bit_array_concat(bit_arrays) {
  return toBitArray(bit_arrays.toArray().map((b) => b.buffer));
}

export function console_log(term) {
  console.log(term);
}

export function console_error(term) {
  console.error(term);
}

export function crash(message) {
  throw new globalThis.Error(message);
}

export function bit_array_to_string(bit_array) {
  try {
    const decoder = new TextDecoder("utf-8", { fatal: true });
    return new Ok(decoder.decode(bit_array.buffer));
  } catch {
    return new Error(Nil);
  }
}

export function print(string) {
  if (typeof process === "object") {
    process.stdout.write(string); // We can write without a trailing newline
  } else if (typeof Deno === "object") {
    Deno.stdout.writeSync(new TextEncoder().encode(string)); // We can write without a trailing newline
  } else {
    console.log(string); // We're in a browser. Newlines are mandated
  }
}

export function print_error(string) {
  if (typeof process === "object" && process.stderr?.write) {
    process.stderr.write(string); // We can write without a trailing newline
  } else if (typeof Deno === "object") {
    Deno.stderr.writeSync(new TextEncoder().encode(string)); // We can write without a trailing newline
  } else {
    console.error(string); // We're in a browser. Newlines are mandated
  }
}

export function print_debug(string) {
  if (typeof process === "object" && process.stderr?.write) {
    process.stderr.write(string + "\n"); // If we're in Node.js, use `stderr`
  } else if (typeof Deno === "object") {
    Deno.stderr.writeSync(new TextEncoder().encode(string + "\n")); // If we're in Deno, use `stderr`
  } else {
    console.log(string); // Otherwise, use `console.log` (so that it doesn't look like an error)
  }
}

export function ceiling(float) {
  return Math.ceil(float);
}

export function floor(float) {
  return Math.floor(float);
}

export function round(float) {
  return Math.round(float);
}

export function truncate(float) {
  return Math.trunc(float);
}

export function power(base, exponent) {
  // It is checked in Gleam that:
  // - The base is non-negative and that the exponent is not fractional.
  // - The base is non-zero and the exponent is non-negative (otherwise
  //   the result will essentially be division by zero).
  // It can thus be assumed that valid input is passed to the Math.pow
  // function and a NaN or Infinity value will not be produced.
  return Math.pow(base, exponent);
}

export function random_uniform() {
  const random_uniform_result = Math.random();
  // With round-to-nearest-even behavior, the ranges claimed for the functions below
  // (excluding the one for Math.random() itself) aren't exact.
  // If extremely large bounds are chosen (2^53 or higher),
  // it's possible in extremely rare cases to calculate the usually-excluded upper bound.
  // Note that as numbers in JavaScript are IEEE 754 floating point numbers
  // See: <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random>
  // Because of this, we just loop 'until' we get a valid result where 0.0 <= x < 1.0:
  if (random_uniform_result === 1.0) {
    return random_uniform();
  }
  return random_uniform_result;
}

export function bit_array_slice(bits, position, length) {
  const start = Math.min(position, position + length);
  const end = Math.max(position, position + length);
  if (start < 0 || end > bits.length) return new Error(Nil);
  const byteOffset = bits.buffer.byteOffset + start;
  const buffer = new Uint8Array(
    bits.buffer.buffer,
    byteOffset,
    Math.abs(length),
  );
  return new Ok(new BitArray(buffer));
}

export function codepoint(int) {
  return new UtfCodepoint(int);
}

export function string_to_codepoint_integer_list(string) {
  return List.fromArray(Array.from(string).map((item) => item.codePointAt(0)));
}

export function utf_codepoint_list_to_string(utf_codepoint_integer_list) {
  return utf_codepoint_integer_list
    .toArray()
    .map((x) => String.fromCodePoint(x.value))
    .join("");
}

export function utf_codepoint_to_int(utf_codepoint) {
  return utf_codepoint.value;
}

export function regex_check(regex, string) {
  regex.lastIndex = 0;
  return regex.test(string);
}

export function compile_regex(pattern, options) {
  try {
    let flags = "gu";
    if (options.case_insensitive) flags += "i";
    if (options.multi_line) flags += "m";
    return new Ok(new RegExp(pattern, flags));
  } catch (error) {
    const number = (error.columnNumber || 0) | 0;
    return new Error(new RegexCompileError(error.message, number));
  }
}

export function regex_split(regex, string) {
  return List.fromArray(
    string.split(regex).map((item) => (item === undefined ? "" : item)),
  );
}

export function regex_scan(regex, string) {
  const matches = Array.from(string.matchAll(regex)).map((match) => {
    const content = match[0];
    const submatches = [];
    for (let n = match.length - 1; n > 0; n--) {
      if (match[n]) {
        submatches[n - 1] = new Some(match[n]);
        continue;
      }
      if (submatches.length > 0) {
        submatches[n - 1] = new None();
      }
    }
    return new RegexMatch(content, List.fromArray(submatches));
  });
  return List.fromArray(matches);
}

export function regex_replace(regex, original_string, replacement) {
  return original_string.replaceAll(regex, replacement);
}

export function new_map() {
  return Dict.new();
}

export function map_size(map) {
  return map.size;
}

export function map_to_list(map) {
  return List.fromArray(map.entries());
}

export function map_remove(key, map) {
  return map.delete(key);
}

export function map_get(map, key) {
  const value = map.get(key, NOT_FOUND);
  if (value === NOT_FOUND) {
    return new Error(Nil);
  }
  return new Ok(value);
}

export function map_insert(key, value, map) {
  return map.set(key, value);
}

function unsafe_percent_decode(string) {
  return decodeURIComponent(string || "");
}

function unsafe_percent_decode_query(string) {
  return decodeURIComponent((string || "").replace("+", " "));
}

export function percent_decode(string) {
  try {
    return new Ok(unsafe_percent_decode(string));
  } catch {
    return new Error(Nil);
  }
}

export function percent_encode(string) {
  return encodeURIComponent(string).replace("%2B", "+");
}

export function parse_query(query) {
  try {
    const pairs = [];
    for (const section of query.split("&")) {
      const [key, value] = section.split("=");
      if (!key) continue;

      const decodedKey = unsafe_percent_decode_query(key);
      const decodedValue = unsafe_percent_decode_query(value);
      pairs.push([decodedKey, decodedValue]);
    }
    return new Ok(List.fromArray(pairs));
  } catch {
    return new Error(Nil);
  }
}

const b64EncodeLookup = [
  65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83,
  84, 85, 86, 87, 88, 89, 90, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106,
  107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121,
  122, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 43, 47,
];

let b64TextDecoder;

// Implementation based on https://github.com/mitschabaude/fast-base64/blob/main/js.js
export function encode64(bit_array, padding) {
  b64TextDecoder ??= new TextDecoder();

  const bytes = bit_array.buffer;

  const m = bytes.length;
  const k = m % 3;
  const n = Math.floor(m / 3) * 4 + (k && k + 1);
  const N = Math.ceil(m / 3) * 4;
  const encoded = new Uint8Array(N);

  for (let i = 0, j = 0; j < m; i += 4, j += 3) {
    const y = (bytes[j] << 16) + (bytes[j + 1] << 8) + (bytes[j + 2] | 0);
    encoded[i] = b64EncodeLookup[y >> 18];
    encoded[i + 1] = b64EncodeLookup[(y >> 12) & 0x3f];
    encoded[i + 2] = b64EncodeLookup[(y >> 6) & 0x3f];
    encoded[i + 3] = b64EncodeLookup[y & 0x3f];
  }

  let base64 = b64TextDecoder.decode(new Uint8Array(encoded.buffer, 0, n));

  if (padding) {
    if (k === 1) {
      base64 += "==";
    } else if (k === 2) {
      base64 += "=";
    }
  }

  return base64;
}

// From https://developer.mozilla.org/en-US/docs/Glossary/Base64
export function decode64(sBase64) {
  try {
    const binString = atob(sBase64);
    const length = binString.length;
    const array = new Uint8Array(length);
    for (let i = 0; i < length; i++) {
      array[i] = binString.charCodeAt(i);
    }
    return new Ok(new BitArray(array));
  } catch {
    return new Error(Nil);
  }
}

export function classify_dynamic(data) {
  if (typeof data === "string") {
    return "String";
  } else if (typeof data === "boolean") {
    return "Bool";
  } else if (data instanceof Result) {
    return "Result";
  } else if (data instanceof List) {
    return "List";
  } else if (data instanceof BitArray) {
    return "BitArray";
  } else if (data instanceof Dict) {
    return "Dict";
  } else if (Number.isInteger(data)) {
    return "Int";
  } else if (Array.isArray(data)) {
    return `Tuple of ${data.length} elements`;
  } else if (typeof data === "number") {
    return "Float";
  } else if (data === null) {
    return "Null";
  } else if (data === undefined) {
    return "Nil";
  } else {
    const type = typeof data;
    return type.charAt(0).toUpperCase() + type.slice(1);
  }
}

function decoder_error(expected, got) {
  return decoder_error_no_classify(expected, classify_dynamic(got));
}

function decoder_error_no_classify(expected, got) {
  return new Error(
    List.fromArray([new DecodeError(expected, got, List.fromArray([]))]),
  );
}

export function decode_string(data) {
  return typeof data === "string"
    ? new Ok(data)
    : decoder_error("String", data);
}

export function decode_int(data) {
  return Number.isInteger(data) ? new Ok(data) : decoder_error("Int", data);
}

export function decode_float(data) {
  return typeof data === "number" ? new Ok(data) : decoder_error("Float", data);
}

export function decode_bool(data) {
  return typeof data === "boolean" ? new Ok(data) : decoder_error("Bool", data);
}

export function decode_bit_array(data) {
  if (data instanceof BitArray) {
    return new Ok(data);
  }
  if (data instanceof Uint8Array) {
    return new Ok(new BitArray(data));
  }
  return decoder_error("BitArray", data);
}

export function decode_tuple(data) {
  return Array.isArray(data) ? new Ok(data) : decoder_error("Tuple", data);
}

export function decode_tuple2(data) {
  return decode_tupleN(data, 2);
}

export function decode_tuple3(data) {
  return decode_tupleN(data, 3);
}

export function decode_tuple4(data) {
  return decode_tupleN(data, 4);
}

export function decode_tuple5(data) {
  return decode_tupleN(data, 5);
}

export function decode_tuple6(data) {
  return decode_tupleN(data, 6);
}

function decode_tupleN(data, n) {
  if (Array.isArray(data) && data.length == n) {
    return new Ok(data);
  }

  const list = decode_exact_length_list(data, n);
  if (list) return new Ok(list);

  return decoder_error(`Tuple of ${n} elements`, data);
}

function decode_exact_length_list(data, n) {
  if (!(data instanceof List)) return;

  const elements = [];
  let current = data;

  for (let i = 0; i < n; i++) {
    if (!(current instanceof NonEmpty)) break;
    elements.push(current.head);
    current = current.tail;
  }

  if (elements.length === n && !(current instanceof NonEmpty)) return elements;
}

export function tuple_get(data, index) {
  return index >= 0 && data.length > index
    ? new Ok(data[index])
    : new Error(Nil);
}

export function decode_list(data) {
  if (Array.isArray(data)) {
    return new Ok(List.fromArray(data));
  }
  return data instanceof List ? new Ok(data) : decoder_error("List", data);
}

export function decode_result(data) {
  return data instanceof Result ? new Ok(data) : decoder_error("Result", data);
}

export function decode_map(data) {
  if (data instanceof Dict) {
    return new Ok(data);
  }
  if (data instanceof Map || data instanceof WeakMap) {
    return new Ok(Dict.fromMap(data));
  }
  if (data == null) {
    return decoder_error("Dict", data);
  }
  if (typeof data !== "object") {
    return decoder_error("Dict", data);
  }
  const proto = Object.getPrototypeOf(data);
  if (proto === Object.prototype || proto === null) {
    return new Ok(Dict.fromObject(data));
  }
  return decoder_error("Dict", data);
}

export function decode_option(data, decoder) {
  if (data === null || data === undefined || data instanceof None)
    return new Ok(new None());
  if (data instanceof Some) data = data[0];
  const result = decoder(data);
  if (result.isOk()) {
    return new Ok(new Some(result[0]));
  } else {
    return result;
  }
}

export function decode_field(value, name) {
  const not_a_map_error = () => decoder_error("Dict", value);

  if (
    value instanceof Dict ||
    value instanceof WeakMap ||
    value instanceof Map
  ) {
    const entry = map_get(value, name);
    return new Ok(entry.isOk() ? new Some(entry[0]) : new None());
  } else if (value === null) {
    return not_a_map_error();
  } else if (Object.getPrototypeOf(value) == Object.prototype) {
    return try_get_field(value, name, () => new Ok(new None()));
  } else {
    return try_get_field(value, name, not_a_map_error);
  }
}

function try_get_field(value, field, or_else) {
  try {
    return field in value ? new Ok(new Some(value[field])) : or_else();
  } catch {
    return or_else();
  }
}

export function byte_size(string) {
  return new TextEncoder().encode(string).length;
}

// In Javascript bitwise operations convert numbers to a sequence of 32 bits
// while Erlang uses arbitrary precision.
// To get around this problem and get consistent results use BigInt and then
// downcast the value back to a Number value.

export function bitwise_and(x, y) {
  return Number(BigInt(x) & BigInt(y));
}

export function bitwise_not(x) {
  return Number(~BigInt(x));
}

export function bitwise_or(x, y) {
  return Number(BigInt(x) | BigInt(y));
}

export function bitwise_exclusive_or(x, y) {
  return Number(BigInt(x) ^ BigInt(y));
}

export function bitwise_shift_left(x, y) {
  return Number(BigInt(x) << BigInt(y));
}

export function bitwise_shift_right(x, y) {
  return Number(BigInt(x) >> BigInt(y));
}

export function inspect(v) {
  const t = typeof v;
  if (v === true) return "True";
  if (v === false) return "False";
  if (v === null) return "//js(null)";
  if (v === undefined) return "Nil";
  if (t === "string") return inspectString(v);
  if (t === "bigint" || Number.isInteger(v)) return v.toString();
  if (t === "number") return float_to_string(v);
  if (Array.isArray(v)) return `#(${v.map(inspect).join(", ")})`;
  if (v instanceof List) return inspectList(v);
  if (v instanceof UtfCodepoint) return inspectUtfCodepoint(v);
  if (v instanceof BitArray) return inspectBitArray(v);
  if (v instanceof CustomType) return inspectCustomType(v);
  if (v instanceof Dict) return inspectDict(v);
  if (v instanceof Set) return `//js(Set(${[...v].map(inspect).join(", ")}))`;
  if (v instanceof RegExp) return `//js(${v})`;
  if (v instanceof Date) return `//js(Date("${v.toISOString()}"))`;
  if (v instanceof Function) {
    const args = [];
    for (const i of Array(v.length).keys())
      args.push(String.fromCharCode(i + 97));
    return `//fn(${args.join(", ")}) { ... }`;
  }
  return inspectObject(v);
}

function inspectString(str) {
  let new_str = '"';
  for (let i = 0; i < str.length; i++) {
    let char = str[i];
    switch (char) {
      case "\n":
        new_str += "\\n";
        break;
      case "\r":
        new_str += "\\r";
        break;
      case "\t":
        new_str += "\\t";
        break;
      case "\f":
        new_str += "\\f";
        break;
      case "\\":
        new_str += "\\\\";
        break;
      case '"':
        new_str += '\\"';
        break;
      default:
        if (char < " " || (char > "~" && char < "\u{00A0}")) {
          new_str +=
            "\\u{" +
            char.charCodeAt(0).toString(16).toUpperCase().padStart(4, "0") +
            "}";
        } else {
          new_str += char;
        }
    }
  }
  new_str += '"';
  return new_str;
}

function inspectDict(map) {
  let body = "dict.from_list([";
  let first = true;
  map.forEach((value, key) => {
    if (!first) body = body + ", ";
    body = body + "#(" + inspect(key) + ", " + inspect(value) + ")";
    first = false;
  });
  return body + "])";
}

function inspectObject(v) {
  const name = Object.getPrototypeOf(v)?.constructor?.name || "Object";
  const props = [];
  for (const k of Object.keys(v)) {
    props.push(`${inspect(k)}: ${inspect(v[k])}`);
  }
  const body = props.length ? " " + props.join(", ") + " " : "";
  const head = name === "Object" ? "" : name + " ";
  return `//js(${head}{${body}})`;
}

function inspectCustomType(record) {
  const props = Object.keys(record)
    .map((label) => {
      const value = inspect(record[label]);
      return isNaN(parseInt(label)) ? `${label}: ${value}` : value;
    })
    .join(", ");
  return props
    ? `${record.constructor.name}(${props})`
    : record.constructor.name;
}

export function inspectList(list) {
  return `[${list.toArray().map(inspect).join(", ")}]`;
}

export function inspectBitArray(bits) {
  return `<<${Array.from(bits.buffer).join(", ")}>>`;
}

export function inspectUtfCodepoint(codepoint) {
  return `//utfcodepoint(${String.fromCodePoint(codepoint.value)})`;
}

export function base16_encode(bit_array) {
  let result = "";
  for (const byte of bit_array.buffer) {
    result += byte.toString(16).padStart(2, "0").toUpperCase();
  }
  return result;
}

export function base16_decode(string) {
  const bytes = new Uint8Array(string.length / 2);
  for (let i = 0; i < string.length; i += 2) {
    const a = parseInt(string[i], 16);
    const b = parseInt(string[i + 1], 16);
    if (isNaN(a) || isNaN(b)) return new Error(Nil);
    bytes[i / 2] = a * 16 + b;
  }
  return new Ok(new BitArray(bytes));
}

export function bit_array_inspect(bits, acc) {
  return `${acc}${[...bits.buffer].join(", ")}`;
}

export function bit_array_compare(first, second) {
  for (let i = 0; i < first.length; i++) {
    if (i >= second.length) {
      return new Gt(); // first has more items
    }
    const f = first.buffer[i];
    const s = second.buffer[i];
    if (f > s) {
      return new Gt();
    }
    if (f < s) {
      return new Lt();
    }
  }
  // This means that either first did not have any items
  // or all items in first were equal to second.
  if (first.length === second.length) {
    return new Eq();
  }
  return new Lt(); // second has more items
}
