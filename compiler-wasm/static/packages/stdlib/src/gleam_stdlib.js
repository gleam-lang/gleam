import {
  BitString,
  Error,
  List,
  Ok,
  Result,
  UtfCodepoint,
  inspect,
  stringBits,
  toBitString,
} from "./gleam.js";
import {
  CompileError as RegexCompileError,
  Match as RegexMatch,
} from "./gleam/regex.js";
import { DecodeError } from "./gleam/dynamic.js";
import { Some, None } from "./gleam/option.js";

const HASHCODE_CACHE = new WeakMap();

const Nil = undefined;

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
  if (/^[-+]?(\d+)\.(\d+)$/.test(value)) {
    return new Ok(parseFloat(value));
  } else {
    return new Error(Nil);
  }
}

export function to_string(term) {
  return term.toString();
}

export function float_to_string(float) {
  let string = float.toString();
  if (string.indexOf(".") >= 0) {
    return string;
  } else {
    return string + ".0";
  }
}

export function int_to_base_string(int, base) {
  return int.toString(base).toUpperCase();
}

export function string_replace(string, target, substitute) {
  return string.replaceAll(target, substitute);
}

export function string_reverse(string) {
  return [...string].reverse().join("");
}

export function string_length(string) {
  let iterator = graphemes_iterator(string);
  if (iterator) {
    let i = 0;
    for (let _ of iterator) {
      i++;
    }
    return i;
  } else {
    return string.match(/./gu).length;
  }
}

function graphemes_iterator(string) {
  if (Intl && Intl.Segmenter) {
    return new Intl.Segmenter("en-gb").segment(string)[Symbol.iterator]();
  }
}

export function pop_grapheme(string) {
  let first;
  let iterator = graphemes_iterator(string);
  if (iterator) {
    first = iterator.next().value?.segment;
  } else {
    first = string.match(/./u)?.[0];
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

export function equal(a, b) {
  return a === b;
}

export function split(xs, pattern) {
  return List.fromArray(xs.split(pattern));
}

export function join(xs) {
  return xs.toArray().join("");
}

export function length(data) {
  return data.length;
}

export function slice_string(string, from, length) {
  return string.slice(from, from + length);
}

export function crop_string(string, substring) {
  return string.substring(string.indexOf(substring));
}

export function index_of(haystack, needle) {
  return haystack.indexOf(needle) | 0;
}

export function starts_with(haystack, needle) {
  return haystack.startsWith(needle);
}

export function ends_with(haystack, needle) {
  return haystack.endsWith(needle);
}

export function split_once(haystack, needle) {
  let index = haystack.indexOf(needle);
  if (index >= 0) {
    let before = haystack.slice(0, index);
    let after = haystack.slice(index + needle.length);
    return new Ok([before, after]);
  } else {
    return new Error(Nil);
  }
}

export function trim(string) {
  return string.trim();
}

export function trim_left(string) {
  return string.trimLeft();
}

export function trim_right(string) {
  return string.trimRight();
}

export function bit_string_from_string(string) {
  return new toBitString([stringBits(string)]);
}

export function bit_string_concat(bit_strings) {
  return toBitString(bit_strings.toArray().map((b) => b.buffer));
}

export function log(term) {
  console.log(term);
}

export function debug(term) {
  console.log(inspect(term));
}

export function crash(message) {
  throw new globalThis.Error(message);
}

export function bit_string_to_string(bit_string) {
  try {
    let decoder = new TextDecoder("utf-8", { fatal: true });
    return new Ok(decoder.decode(bit_string.buffer));
  } catch (_error) {
    return new Error(undefined);
  }
}

export function print(string) {
  if (typeof process === "object") {
    process.stdout.write(string); // We can write without a trailing newline
  } else {
    console.log(string); // We're in a browser. Newlines are mandated
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
  return Math.pow(base, exponent);
}

export function bit_string_slice(bits, position, length) {
  let start = Math.min(position, position + length);
  let end = Math.max(position, position + length);
  if (start < 0 || end > bits.length) return new Error(Nil);
  let buffer = new Uint8Array(bits.buffer.buffer, start, Math.abs(length));
  return new Ok(new BitString(buffer));
}

export function codepoint(int) {
  return new UtfCodepoint(int);
}

export function regex_check(regex, string) {
  return regex.test(string);
}

export function compile_regex(pattern, options) {
  try {
    let flags = "gu";
    if (options.case_insensitive) flags += "i";
    if (options.multi_line) flags += "m";
    return new Ok(new RegExp(pattern, flags));
  } catch (error) {
    let number = (error.columnNumber || 0) | 0;
    return new Error(new RegexCompileError(error.message, number));
  }
}

export function regex_scan(regex, string) {
  let matches = Array.from(string.matchAll(regex)).map((match) => {
    let content = match.shift();
    let submatches = match.map((x) => (x ? new Some(x) : new None()));
    return new RegexMatch(content, List.fromArray(submatches));
  });
  return List.fromArray(matches);
}

class Map {
  static #hashcode_cache = new WeakMap();

  static hash(value) {
    let existing = this.#hashcode_cache.get(value);
    if (existing) {
      return existing;
    } else if (value instanceof Object) {
      let hashcode = JSON.stringify(value);
      HASHCODE_CACHE.set(value, hashcode);
      return hashcode;
    } else {
      return value.toString();
    }
  }

  constructor() {
    this.entries = new globalThis.Map();
  }

  get size() {
    return this.entries.size;
  }

  inspect() {
    let entries = [...this.entries.values()]
      .map((pair) => inspect(pair))
      .join(", ");
    return `map.from_list([${entries}])`;
  }

  copy() {
    let map = new Map();
    map.entries = new globalThis.Map(this.entries);
    return map;
  }

  toList() {
    return List.fromArray([...this.entries.values()]);
  }

  insert(k, v) {
    let map = this.copy();
    map.entries.set(Map.hash(k), [k, v]);
    return map;
  }

  delete(k) {
    let map = this.copy();
    map.entries.delete(Map.hash(k));
    return map;
  }

  get(key) {
    let code = Map.hash(key);
    if (this.entries.has(code)) {
      return new Ok(this.entries.get(code)[1]);
    } else {
      return new Error(Nil);
    }
  }
}

export function new_map() {
  return new Map();
}

export function map_size(map) {
  return map.size;
}

export function map_to_list(map) {
  return map.toList();
}

export function map_remove(k, map) {
  return map.delete(k);
}

export function map_get(map, key) {
  return map.get(key);
}

export function map_insert(key, value, map) {
  return map.insert(key, value);
}

function unsafe_percent_decode(string) {
  return decodeURIComponent((string || "").replace("+", " "));
}

export function percent_decode(string) {
  try {
    return new Ok(unsafe_percent_decode(string));
  } catch (error) {
    return new Error(Nil);
  }
}

export function percent_encode(string) {
  return encodeURIComponent(string);
}

export function parse_query(query) {
  try {
    let pairs = [];
    for (let section of query.split("&")) {
      let [key, value] = section.split("=");
      if (!key) continue;
      pairs.push([unsafe_percent_decode(key), unsafe_percent_decode(value)]);
    }
    return new Ok(List.fromArray(pairs));
  } catch (error) {
    return new Error(Nil);
  }
}

// From https://developer.mozilla.org/en-US/docs/Glossary/Base64#Solution_2_%E2%80%93_rewrite_the_DOMs_atob()_and_btoa()_using_JavaScript's_TypedArrays_and_UTF-8
export function encode64(bit_string) {
  let aBytes = bit_string.buffer;
  let nMod3 = 2,
    sB64Enc = "";

  for (let nLen = aBytes.length, nUint24 = 0, nIdx = 0; nIdx < nLen; nIdx++) {
    nMod3 = nIdx % 3;
    if (nIdx > 0 && ((nIdx * 4) / 3) % 76 === 0) {
      sB64Enc += "\r\n";
    }
    nUint24 |= aBytes[nIdx] << ((16 >>> nMod3) & 24);
    if (nMod3 === 2 || aBytes.length - nIdx === 1) {
      sB64Enc += String.fromCharCode(
        uint6ToB64((nUint24 >>> 18) & 63),
        uint6ToB64((nUint24 >>> 12) & 63),
        uint6ToB64((nUint24 >>> 6) & 63),
        uint6ToB64(nUint24 & 63)
      );
      nUint24 = 0;
    }
  }

  return (
    sB64Enc.substr(0, sB64Enc.length - 2 + nMod3) +
    (nMod3 === 2 ? "" : nMod3 === 1 ? "=" : "==")
  );
}

// From https://developer.mozilla.org/en-US/docs/Glossary/Base64#Solution_2_%E2%80%93_rewrite_the_DOMs_atob()_and_btoa()_using_JavaScript's_TypedArrays_and_UTF-8
function uint6ToB64(nUint6) {
  return nUint6 < 26
    ? nUint6 + 65
    : nUint6 < 52
    ? nUint6 + 71
    : nUint6 < 62
    ? nUint6 - 4
    : nUint6 === 62
    ? 43
    : nUint6 === 63
    ? 47
    : 65;
}

// From https://developer.mozilla.org/en-US/docs/Glossary/Base64#Solution_2_%E2%80%93_rewrite_the_DOMs_atob()_and_btoa()_using_JavaScript's_TypedArrays_and_UTF-8
function b64ToUint6(nChr) {
  return nChr > 64 && nChr < 91
    ? nChr - 65
    : nChr > 96 && nChr < 123
    ? nChr - 71
    : nChr > 47 && nChr < 58
    ? nChr + 4
    : nChr === 43
    ? 62
    : nChr === 47
    ? 63
    : 0;
}

// From https://developer.mozilla.org/en-US/docs/Glossary/Base64#Solution_2_%E2%80%93_rewrite_the_DOMs_atob()_and_btoa()_using_JavaScript's_TypedArrays_and_UTF-8
export function decode64(sBase64) {
  if (sBase64.match(/[^A-Za-z0-9\+\/=]/g)) return new Error(Nil);
  let sB64Enc = sBase64.replace(/=/g, "");
  let nInLen = sB64Enc.length;
  let nOutLen = (nInLen * 3 + 1) >> 2;
  let taBytes = new Uint8Array(nOutLen);

  for (
    let nMod3, nMod4, nUint24 = 0, nOutIdx = 0, nInIdx = 0;
    nInIdx < nInLen;
    nInIdx++
  ) {
    nMod4 = nInIdx & 3;
    nUint24 |= b64ToUint6(sB64Enc.charCodeAt(nInIdx)) << (6 * (3 - nMod4));
    if (nMod4 === 3 || nInLen - nInIdx === 1) {
      for (nMod3 = 0; nMod3 < 3 && nOutIdx < nOutLen; nMod3++, nOutIdx++) {
        taBytes[nOutIdx] = (nUint24 >>> ((16 >>> nMod3) & 24)) & 255;
      }
      nUint24 = 0;
    }
  }

  return new Ok(new BitString(taBytes));
}

export function classify_dynamic(data) {
  if (typeof data === "string") {
    return "String";
  } else if (Result.isResult(data)) {
    return "Result";
  } else if (List.isList(data)) {
    return "List";
  } else if (Number.isInteger(data)) {
    return "Int";
  } else if (Array.isArray(data)) {
    return `Tuple of ${data.length} elements`;
  } else if (BitString.isBitString(data)) {
    return "BitString";
  } else if (data instanceof Map) {
    return "Map";
  } else if (typeof data === "number") {
    return "Float";
  } else {
    let type = typeof data;
    return type.charAt(0).toUpperCase() + type.slice(1);
  }
}

function decoder_error(expected, got) {
  return new Error(new DecodeError(expected, classify_dynamic(got)));
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

export function decode_bit_string(data) {
  return BitString.isBitString(data)
    ? new Ok(data)
    : decoder_error("BitString", data);
}

export function decode_tuple(data) {
  return Array.isArray(data) ? new Ok(data) : decoder_error("Tuple", data);
}

export function tuple_get(data, index) {
  return index >= 0 && data.length > index
    ? new Ok(data[index])
    : new Error(Nil);
}

export function decode_list(data) {
  return List.isList(data) ? new Ok(data) : decoder_error("List", data);
}

export function decode_result(data) {
  return Result.isResult(data) ? new Ok(data) : decoder_error("Result", data);
}

export function decode_map(data) {
  return data instanceof Map ? new Ok(data) : decoder_error("Map", data);
}

export function decode_option(data, decoder) {
  if (data === null || data === undefined || data instanceof None)
    return new Ok(new None());
  if (data instanceof Some) data = data[0];
  let result = decoder(data);
  if (result.isOk()) {
    return new Ok(new Some(result[0]));
  } else {
    return result;
  }
}

export function decode_field(value, name) {
  let error = () => decoder_error(`Value with field ${inspect(name)}`, value);
  if (value instanceof Map) {
    let entry = value.get(name);
    return entry.isOk() ? entry : error();
  }
  try {
    return name in value ? new Ok(value[name]) : error();
  } catch {
    return error();
  }
}
