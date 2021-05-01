import { main } from "./target-javascript/main.js";

function print(string) {
  process.stdout.write(string);
  return string;
}

let status = main(print);
process.exit(status);
