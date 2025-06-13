export function circular_reference() {
  const x = [];
  x.push(x);
  return x;
}
