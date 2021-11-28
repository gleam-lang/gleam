import { print as do_print, log as do_println, debug as debug_print } from "../gleam_stdlib.js";

export function print(string) {
  return do_print(string);
}

export function println(string) {
  return do_println(string);
}

export function debug(term) {
  debug_print(term);
  return term;
}
