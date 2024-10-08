import * as $string from "../gleam/string.mjs";
import {
  print as do_print,
  print_error as do_print_error,
  console_log as do_println,
  console_error as do_println_error,
  print_debug as do_debug_println,
} from "../gleam_stdlib.mjs";

export function print(string) {
  return do_print(string);
}

export function print_error(string) {
  return do_print_error(string);
}

export function println(string) {
  return do_println(string);
}

export function println_error(string) {
  return do_println_error(string);
}

export function debug(term) {
  let _pipe = term;
  let _pipe$1 = $string.inspect(_pipe);
  do_debug_println(_pipe$1)
  return term;
}
