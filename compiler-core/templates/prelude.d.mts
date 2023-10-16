export class CustomType {
  inspect(): string;
  withFields<K extends keyof this>(fields: { [P in K]: this[P] }): this;
}

export interface ListStatic {
  fromArray<T>(array: Array<T>): List<T>;
}

export interface List<T> extends Iterable<T> {
  head?: T;
  tail?: List<T>;
  inspect(): string;
  toArray(): Array<T>;
  atLeastLength(desired: number): boolean;
  hasLength(desired: number): boolean;
  countLength(): number;
}

export function toList<T>(array: Array<T>): List<T>;

export interface BitArray {
  get length(): number;
  inspect(): string;
  byteAt(index: number): number;
  floatAt(index: number): number;
  intFromSlice(start: number, end: number): number;
  sliceAfter(index: number): BitArray;
}

export interface Utf8Codepoint {
  inspect(): string;
}

export function toBitArray(segments: Array<number | Uint8Array>): BitArray;

export function sizedInt(number: number, size: number): BitArray;

export function stringBits(string: string): Uint8Array;

export function codepointBits(codepoint: Utf8Codepoint): Uint8Array;

export function float64Bits(float: number): Uint8Array;

export interface Result<T, E> {
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

export function remainderInt(a: number, b: number): number;

export function divideInt(a: number, b: number): number;

export function divideFloat(a: number, b: number): number;
