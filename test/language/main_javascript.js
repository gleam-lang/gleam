import { main } from "./target-javascript/main.js";

// TODO: remove this. Used by the `test` module's erase function
global.test = { identity: (a) => a };

function print(string) {
  process.stdout.write(string);
  return string;
}

function append(a, b) {
  return `${a}${b}`;
}

let status = main(print, String, append);
process.exit(status);
