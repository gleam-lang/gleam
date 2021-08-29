export interface Inspect {
  inspect(): string;
}

export interface ListStatic {
  fromArray<T>(array: Array<T>): List<T>;
}

export interface List<T> extends Inspect {
  get __gleam_prelude_variant__(): "EmptyList" | "NonEmptyList";
  head?: T;
  tail?: List<T>;
  toArray(): Array<T>;
  atLeastLength(desired: number): boolean;
  hasLength(desired: number): boolean;
}

export function toList<T>(array: Array<T>): List<T>;

export interface BitString extends Inspect {
  get __gleam_prelude_variant__(): "BitString";
  get length(): number;
}

export interface Utf8Codepoint extends Inspect {
  get __gleam_prelude_variant__(): "UtfCodepoint";
}

export function toBitString(segments: Array<number | Uint8Array>): BitString;

export function stringBits(string: string): Uint8Array;

export function codepointBits(codepoint: Utf8Codepoint): Uint8Array;

export interface Result<T, E> extends Inspect {
  get __gleam_prelude_variant__(): "Ok" | "Error";
  isOk(): boolean;
}

export interface OkStatic {
  new <T, E>(value: T): Result<T, E>;
}

export interface ErrorStatic {
  new <T, E>(value: E): Result<T, E>;
}

export function inspect(value: any): string;

export function isEqual(a: any, b: any): boolean;

export function divideInt(a: number, b: number): number;

export function divideFloat(a: number, b: number): number;
