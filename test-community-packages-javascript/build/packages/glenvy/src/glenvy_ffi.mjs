import { Ok, Error } from "./gleam.mjs";
import { readFileSync } from "node:fs";

const Nil = undefined;

export function get_env(name) {
  if (process.env[name]) {
    return new Ok(process.env[name]);
  } else {
    return new Error(Nil);
  }
}

export function set_env(name, value) {
  process.env[name] = value;

  return Nil;
}

export function unset_env(name) {
  delete process.env[name];

  return Nil;
}

export function read_file(path) {
  try {
    return new Ok(readFileSync(path, "utf-8"));
  } catch (error) {
    return new Error(error.message || "");
  }
}
