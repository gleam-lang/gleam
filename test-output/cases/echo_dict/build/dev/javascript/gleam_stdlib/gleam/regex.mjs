import { CustomType as $CustomType } from "../gleam.mjs";
import * as $option from "../gleam/option.mjs";
import {
  compile_regex as do_compile,
  regex_check as do_check,
  regex_split as do_split,
  regex_scan as do_scan,
  regex_replace as replace,
} from "../gleam_stdlib.mjs";

export { replace };

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

export function check(regex, string) {
  return do_check(regex, string);
}

export function split(regex, string) {
  return do_split(regex, string);
}

export function scan(regex, string) {
  return do_scan(regex, string);
}
