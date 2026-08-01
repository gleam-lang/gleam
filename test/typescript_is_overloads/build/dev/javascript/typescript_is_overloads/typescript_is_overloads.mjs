/// <reference types="./typescript_is_overloads.d.mts" />
import { CustomType as $CustomType } from "./gleam.mjs";

export class AlmostFull extends $CustomType {
  constructor($0, $1) {
    super();
    this[0] = $0;
    this[1] = $1;
  }
}
export const Box$AlmostFull = ($0, $1) => new AlmostFull($0, $1);
export const Box$isAlmostFull = (value) => value instanceof AlmostFull;
export const Box$AlmostFull$0 = (value) => value[0];
export const Box$AlmostFull$1 = (value) => value[1];

export class AlmostEmpty extends $CustomType {
  constructor($0) {
    super();
    this[0] = $0;
  }
}
export const Box$AlmostEmpty = ($0) => new AlmostEmpty($0);
export const Box$isAlmostEmpty = (value) => value instanceof AlmostEmpty;
export const Box$AlmostEmpty$0 = (value) => value[0];

export class Empty extends $CustomType {}
export const Box$Empty$const = new Empty();
export const Box$Empty = () => Box$Empty$const;
export const Box$isEmpty = (value) => value instanceof Empty;
