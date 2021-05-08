import { main } from "./target-javascript/main.js";

function print(string) {
  process.stdout.write(string);
  return string;
}

function append(a, b) {
  return a + b;
}

let status = main(print, String, append);
process.exit(status);
