// In Javascript bitwise operations convert numbers to a sequence of 32 bits
// while Erlang uses arbitrary precision.
// To get around this problem and get consistent results use BigInt and then
// downcast the value back to a Number value.

export function and(x, y) {
  return Number(BigInt(x) & BigInt(y));
}

export function not(x) {
  return Number(~BigInt(x));
}

export function or(x, y) {
  return Number(BigInt(x) | BigInt(y));
}

export function exclusive_or(x, y) {
  return Number(BigInt(x) ^ BigInt(y));
}

export function shift_left(x, y) {
  return Number(BigInt(x) << BigInt(y));
}

export function shift_right(x, y) {
  return Number(BigInt(x) >> BigInt(y));
}
