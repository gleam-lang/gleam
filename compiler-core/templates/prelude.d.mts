export class CustomType {
  withFields<K extends keyof this>(fields: { [P in K]: this[P] }): this;
}

export class List<T> implements Iterable<T> {
  head?: T;
  tail?: List<T>;
  static fromArray<T>(array: Array<T>): List<T>;
  toArray(): Array<T>;
  atLeastLength(desired: number): boolean;
  hasLength(desired: number): boolean;
  countLength(): number;

  [Symbol.iterator](): Iterator<T>;
}

export function prepend<T>(element: T, tail: List<T>): List<T>;
export function toList<T>(array: Array<T>): List<T>;

export class Empty<T = never> extends List<T> {}

export class NonEmpty<T> extends List<T> {}

export class BitArray {
  bitSize: number;
  byteSize: number;
  bitOffset: number;
  rawBuffer: Uint8Array;

  constructor(buffer: Uint8Array, bitSize?: number, bitOffset?: number);

  byteAt(index: number): number;

  /** @internal */
  equals(other: BitArray): boolean;

  /** @deprecated */
  get buffer(): Uint8Array;

  /** @deprecated */
  get length(): number;
}

export class UtfCodepoint {
  value: string;
}

export function toBitArray(segments: Array<BitArray | Uint8Array | number>): BitArray;

export function sizedInt(
  value: number,
  size: number,
  isBigEndian: boolean
): Uint8Array | BitArray;

export function stringBits(string: string): Uint8Array;

export function codepointBits(codepoint: UtfCodepoint): Uint8Array;

export function sizedFloat(
  value: number,
  size: number,
  isBigEndian: boolean
): Uint8Array;

export class Result<T, E> extends CustomType {
  static isResult(data: unknown): boolean;
  isOk(): boolean;
}

export class Ok<T, E> extends Result<T, E> {
  0: T;
  constructor(value: T);
}

export class Error<T, E> extends Result<T, E> {
  0: E;
  constructor(value: E);
}

export function isEqual(a: any, b: any): boolean;

export function remainderInt(a: number, b: number): number;

export function divideInt(a: number, b: number): number;

export function divideFloat(a: number, b: number): number;
