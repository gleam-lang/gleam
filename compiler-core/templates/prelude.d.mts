/** @deprecated */
export class CustomType {
  /** @deprecated */
  withFields<K extends keyof this>(fields: { [P in K]: this[P] }): this;
}

export interface List<T> {
  readonly __gleam: unique symbol;

  /** @deprecated */
  head?: T;
  /** @deprecated */
  tail?: List<T>;
  /** @deprecated */
  toArray(): Array<T>;
  /** @deprecated */
  atLeastLength(desired: number): boolean;
  /** @deprecated */
  hasLength(desired: number): boolean;
  /** @deprecated */
  countLength(): number;
  /** @deprecated */
  [Symbol.iterator](): Iterator<T>;
}
/** @deprecated */
export const List: {
  /** @deprecated */
  new <T>(): List<T>;
  /** @deprecated */
  fromArray<T>(array: Array<T>): List<T>;
}
export function List$Empty<T>(): List<T>;
export function List$NonEmpty<T>(head: T, tail: List<T>): List<T>;
export function List$isEmpty<T>(list: List<T>): boolean;
export function List$isNonEmpty<T>(list: List<T>): boolean;
export function List$NonEmpty$first<T>(list: List<T>): T | undefined;
export function List$NonEmpty$rest<T>(list: List<T>): List<T> | undefined;
/** @deprecated */
export class Empty<T = never> extends List<T> { }
/** @deprecated */
export class NonEmpty<T> extends List<T> { }

export interface BitArray {
  readonly __gleam: unique symbol;

  /** @deprecated */
  bitSize: number;
  /** @deprecated */
  byteSize: number;
  /** @deprecated */
  bitOffset: number;
  /** @deprecated */
  rawBuffer: Uint8Array;
  /** @deprecated */
  constructor(buffer: Uint8Array, bitSize?: number, bitOffset?: number): BitArray;
  /** @deprecated */
  byteAt(index: number): number;
  /** @deprecated */
  equals(other: BitArray): boolean;
  /** @deprecated */
  get buffer(): Uint8Array;
  /** @deprecated */
  get length(): number;
}
/** @deprecated */
export const BitArray: {
  /** @deprecated */
  new(buffer: Uint8Array, bitSize?: number, bitOffset?: number);
}
export function BitArray$BitArray(
  buffer: Uint8Array,
  bitSize: number,
  bitOffset: number,
): BitArray;

export interface UtfCodepoint {
  readonly __gleam: unique symbol;
  /** @deprecated */
  value: string;
}
/** @deprecated */
export const UtfCodepoint: {
  /** @deprecated */
  new(value: string): UtfCodepoint
}

export interface Result<T, E> {
  readonly __gleam: unique symbol;
  /** @deprecated */
  isOk(): boolean;
}
/** @deprecated */
export const Result: {
  new <T, E>(): Result<T, E>
}
export function Result$Ok<T, E>(value: T): Result<T, E>;
export function Result$Error<T, E>(error: E): Result<T, E>;
export function Result$isError<T, E>(result: Result<T, E>): boolean;
export function Result$isOk<T, E>(result: Result<T, E>): boolean;
export function Result$Ok$0<T, E>(result: Result<T, E>): T | undefined;
export function Result$Error$0<T, E>(result: Result<T, E>): E | undefined;
/** @deprecated */
export class Ok<T, E> extends Result<T, E> {
  /** @deprecated */
  0: T;
  /** @deprecated */
  constructor(value: T);
  /** @deprecated */
  withFields<K extends keyof this>(fields: { [P in K]: this[P] }): this;
  /** @deprecated */
  static isResult(data: unknown): boolean;
}
/** @deprecated */
export class Error<T, E> extends Result<T, E> {
  /** @deprecated */
  0: E;
  /** @deprecated */
  constructor(value: E);
  /** @deprecated */
  withFields<K extends keyof this>(fields: { [P in K]: this[P] }): this;
  /** @deprecated */
  static isResult(data: unknown): boolean;
}

/** @deprecated */
export function prepend<T>(element: T, tail: List<T>): List<T>;
/** @deprecated */
export function toList<T>(array: Array<T>): List<T>;

/** @deprecated */
export function toBitArray(segments: Array<BitArray | Uint8Array | number>): BitArray;

/** @deprecated */
export function sizedInt(
  /** @deprecated */
  value: number,
  /** @deprecated */
  size: number,
  /** @deprecated */
  isBigEndian: boolean
): Uint8Array | BitArray;

/** @deprecated */
export function stringBits(string: string): Uint8Array;

/** @deprecated */
export function codepointBits(codepoint: UtfCodepoint): Uint8Array;

/** @deprecated */
export function sizedFloat(
  value: number,
  size: number,
  isBigEndian: boolean
): Uint8Array;

/** @deprecated */
export function isEqual(a: any, b: any): boolean;

/** @deprecated */
export function remainderInt(a: number, b: number): number;

/** @deprecated */
export function divideInt(a: number, b: number): number;

/** @deprecated */
export function divideFloat(a: number, b: number): number;
