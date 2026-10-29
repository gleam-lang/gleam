import { List } from "./gleam.mjs";

export function load() {
  if (globalThis.process) {
    const [runtime, program, ...args] = process.argv;
    return [runtime, program, List.fromArray(args)];
  }

  if (globalThis.Deno) {
    const runtime = Deno.execPath();
    const program = new URL(Deno.mainModule).pathname;
    const args = List.fromArray(Deno.args);
    return [runtime, program, args];
  }

  const runtime = "browser";
  const program = document.location.toString();
  const args = List.fromArray([]);
  return [runtime, program, args];
}
