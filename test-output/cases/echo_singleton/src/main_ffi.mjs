import { Thing } from "./thing.mjs";

const it = new Thing();

export function singleton() {
  return it;
}
