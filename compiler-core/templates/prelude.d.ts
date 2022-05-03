export interface ListStatic {
  fromArray<T>(array: Array<T>): List<T>;
  isList(value: unknown): boolean;
}

export interface List<T> extends Iterable<T> {
  get __gleam_prelude_variant__(): "EmptyList" | "NonEmptyList";
  head?: T;
  tail?: List<T>;
  inspect(): string;
  toArray(): Array<T>;
  atLeastLength(desired: number): boolean;
  hasLength(desired: number): boolean;
  countLength(): number;
}

export function toList<T>(array: Array<T>): List<T>;

export interface BitStringStatic {
  isBitString(value: unknown): boolean;
}

export interface BitString {
  get __gleam_prelude_variant__(): "BitString";
  get length(): number;
  inspect(): string;
  byteAt(index: number): number;
  floatAt(index: number): number;
  intFromSlice(start: number, end: number): number;
  sliceAfter(index: number): BitString;
}

export interface Utf8Codepoint {
  get __gleam_prelude_variant__(): "UtfCodepoint";
  inspect(): string;
}

export function toBitString(segments: Array<number | Uint8Array>): BitString;

export function sizedInteger(number: number, size: number): BitString;

export function stringBits(string: string): Uint8Array;

export function codepointBits(codepoint: Utf8Codepoint): Uint8Array;

export function float64Bits(float: number): Uint8Array;

export interface Result<T, E> {
  get __gleam_prelude_variant__(): "Ok" | "Error";
  isOk(): boolean;
  inspect(): string;
}

export interface ResultStatic {
  isResult(value: unknown): boolean;
}

export interface OkStatic extends ResultStatic {
  new <T, E>(value: T): Result<T, E>;
}

export interface ErrorStatic extends ResultStatic {
  new <T, E>(value: E): Result<T, E>;
}

export function inspect(value: any): string;

export function isEqual(a: any, b: any): boolean;

export function divideInt(a: number, b: number): number;

export function divideFloat(a: number, b: number): number;
