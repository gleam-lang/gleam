export function append(a, b) {
  return a + b;
}

export function print(string) {
  process.stdout.write(string);
  return string;
}

export function toString(a) {
  let sanitise = (_k, v) => (typeof v === "bigint" ? `${v}n` : v);
  try {
    return JSON.stringify(a, sanitise);
  } catch (_error) {
    return "//<unprintable-cyclical-data>";
  }
}
