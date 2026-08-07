import type * as _ from "./gleam.d.mts";

export class AlmostFull<I, K> extends _.CustomType {
  /** @deprecated */
  constructor(argument$0: I, argument$1: K);
  /** @deprecated */
  0: I;
  /** @deprecated */
  1: K;
}
export function Box$AlmostFull<I, J, K>($0: I, $1: K): Box$<I, J, K>;
export function Box$isAlmostFull<I, J, K>(value: Box$<I, J, K>): value is AlmostFull<
  I,
  K
>;
export function Box$isAlmostFull<I, J, K>(
  value: any,
): value is Box$<unknown, unknown, unknown>;
export function Box$AlmostFull$0<I, J, K>(value: Box$<I, J, K>): I;
export function Box$AlmostFull$1<I, J, K>(value: Box$<I, J, K>): K;

export class AlmostEmpty<J> extends _.CustomType {
  /** @deprecated */
  constructor(argument$0: J);
  /** @deprecated */
  0: J;
}
export function Box$AlmostEmpty<I, J, K>($0: J): Box$<I, J, K>;
export function Box$isAlmostEmpty<I, J, K>(value: Box$<I, J, K>): value is AlmostEmpty<
  J
>;
export function Box$isAlmostEmpty<I, J, K>(
  value: any,
): value is Box$<unknown, unknown, unknown>;
export function Box$AlmostEmpty$0<I, J, K>(value: Box$<I, J, K>): J;

export class Empty extends _.CustomType {}
export function Box$Empty<I, J, K>(): Box$<I, J, K>;
export function Box$isEmpty<I, J, K>(value: Box$<I, J, K>): value is Empty;
export function Box$isEmpty<I, J, K>(
  value: any,
): value is Box$<unknown, unknown, unknown>;

export type Box$<I, J, K> = AlmostFull<I, K> | AlmostEmpty<J> | Empty;
