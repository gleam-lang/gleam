import { CustomType as $CustomType } from "../gleam.mjs";
import * as $option from "../gleam/option.mjs";
import {
  compile_regex as do_compile,
  regex_check as do_check,
  split as js_split,
  regex_scan as do_scan,
} from "../gleam_stdlib.mjs";

export class Match extends $CustomType {
  constructor(content, submatches) {
    super();
    this.content = content;
    this.submatches = submatches;
  }
}

export class CompileError extends $CustomType {
  constructor(error, byte_index) {
    super();
    this.error = error;
    this.byte_index = byte_index;
  }
}

export class Options extends $CustomType {
  constructor(case_insensitive, multi_line) {
    super();
    this.case_insensitive = case_insensitive;
    this.multi_line = multi_line;
  }
}

export function compile(pattern, options) {
  return do_compile(pattern, options);
}

export function from_string(pattern) {
  return compile(pattern, new Options(false, false));
}

export function check(regex, content) {
  return do_check(regex, content);
}

function do_split(regex, string) {
  return js_split(string, regex);
}

export function split(regex, string) {
  return do_split(regex, string);
}

export function scan(regex, string) {
  return do_scan(regex, string);
}
