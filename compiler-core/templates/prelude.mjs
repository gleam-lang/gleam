// Values marked with @internal are not part of the public API and may change
// without notice.

export class CustomType {
  withFields(fields) {
    let properties = Object.keys(this).map((label) =>
      label in fields ? fields[label] : this[label],
    );
    return new this.constructor(...properties);
  }
}

export class List {
  static fromArray(array, tail) {
    let t = tail || new Empty();
    for (let i = array.length - 1; i >= 0; --i) {
      t = new NonEmpty(array[i], t);
    }
    return t;
  }

  [Symbol.iterator]() {
    return new ListIterator(this);
  }

  toArray() {
    return [...this];
  }

  // @internal
  atLeastLength(desired) {
    let current = this;
    while (desired-- > 0 && current) current = current.tail;
    return current !== undefined;
  }

  // @internal
  hasLength(desired) {
    let current = this;
    while (desired-- > 0 && current) current = current.tail;
    return desired === -1 && current instanceof Empty;
  }

  // @internal
  countLength() {
    let current = this;
    let length = 0;
    while (current) {
      current = current.tail;
      length++;
    }
    return length - 1;
  }
}

// @internal
export function prepend(element, tail) {
  return new NonEmpty(element, tail);
}

export function toList(elements, tail) {
  return List.fromArray(elements, tail);
}

// @internal
class ListIterator {
  #current;

  constructor(current) {
    this.#current = current;
  }

  next() {
    if (this.#current instanceof Empty) {
      return { done: true };
    } else {
      let { head, tail } = this.#current;
      this.#current = tail;
      return { value: head, done: false };
    }
  }
}

export class Empty extends List {}

export class NonEmpty extends List {
  constructor(head, tail) {
    super();
    this.head = head;
    this.tail = tail;
  }
}

/**
 * A bit array is a contiguous sequence of bits similar to Erlang's Binary type.
 */
export class BitArray {
  /**
   * The size in bits of this bit array's data.
   *
   * @type {number}
   */
  bitSize;

  /**
   * The size in bytes of this bit array's data. If this bit array doesn't store
   * a whole number of bytes then this value is rounded up.
   *
   * @type {number}
   */
  byteSize;

  /**
   * The number of unused high bits in the first byte of this bit array's
   * buffer prior to the start of its data. The value of any unused high bits is
   * undefined.
   *
   * The bit offset will be in the range 0-7.
   *
   * @type {number}
   */
  bitOffset;

  /**
   * The raw bytes that hold this bit array's data.
   *
   * If `bitOffset` is not zero then there are unused high bits in the first
   * byte of this buffer.
   *
   * If `bitOffset + bitSize` is not a multiple of 8 then there are unused low
   * bits in the last byte of this buffer.
   *
   * @type {Uint8Array}
   */
  rawBuffer;

  /**
   * Constructs a new bit array from a `Uint8Array`, an optional size in
   * bits, and an optional bit offset.
   *
   * If no bit size is specified it is taken as `buffer.length * 8`, i.e. all
   * bytes in the buffer make up the new bit array's data.
   *
   * If no bit offset is specified it defaults to zero, i.e. there are no unused
   * high bits in the first byte of the buffer.
   *
   * @param {Uint8Array} buffer
   * @param {number} [bitSize]
   * @param {number} [bitOffset]
   */
  constructor(buffer, bitSize, bitOffset) {
    if (!(buffer instanceof Uint8Array)) {
      throw globalThis.Error(
        "BitArray can only be constructed from a Uint8Array",
      );
    }

    this.bitSize = bitSize ?? buffer.length * 8;
    this.byteSize = Math.trunc((this.bitSize + 7) / 8);
    this.bitOffset = bitOffset ?? 0;

    // Validate the bit size
    if (this.bitSize < 0) {
      throw globalThis.Error(`BitArray bit size is invalid: ${this.bitSize}`);
    }

    // Validate the bit offset
    if (this.bitOffset < 0 || this.bitOffset > 7) {
      throw globalThis.Error(
        `BitArray bit offset is invalid: ${this.bitOffset}`,
      );
    }

    // Validate the length of the buffer
    if (buffer.length !== Math.trunc((this.bitOffset + this.bitSize + 7) / 8)) {
      throw globalThis.Error("BitArray buffer length is invalid");
    }

    this.rawBuffer = buffer;
  }

  /**
   * Returns a specific byte in this bit array. If the byte index is out of
   * range then `undefined` is returned.
   *
   * When returning the final byte of a bit array with a bit size that's not a
   * multiple of 8, the content of the unused low bits are undefined.
   *
   * @param {number} index
   * @returns {number | undefined}
   */
  byteAt(index) {
    if (index < 0 || index >= this.byteSize) {
      return undefined;
    }

    return bitArrayByteAt(this.rawBuffer, this.bitOffset, index);
  }

  /** @internal */
  equals(other) {
    if (this.bitSize !== other.bitSize) {
      return false;
    }

    const wholeByteCount = Math.trunc(this.bitSize / 8);

    // If both bit offsets are zero do a byte-aligned equality check which is
    // faster
    if (this.bitOffset === 0 && other.bitOffset === 0) {
      // Compare any whole bytes
      for (let i = 0; i < wholeByteCount; i++) {
        if (this.rawBuffer[i] !== other.rawBuffer[i]) {
          return false;
        }
      }

      // Compare any trailing bits, excluding unused low bits
      const trailingBitsCount = this.bitSize % 8;
      if (trailingBitsCount) {
        const unusedLowBitCount = 8 - trailingBitsCount;
        if (
          this.rawBuffer[wholeByteCount] >> unusedLowBitCount !==
          other.rawBuffer[wholeByteCount] >> unusedLowBitCount
        ) {
          return false;
        }
      }
    } else {
      // Compare any whole bytes
      for (let i = 0; i < wholeByteCount; i++) {
        const a = bitArrayByteAt(this.rawBuffer, this.bitOffset, i);
        const b = bitArrayByteAt(other.rawBuffer, other.bitOffset, i);

        if (a !== b) {
          return false;
        }
      }

      // Compare any trailing bits
      const trailingBitsCount = this.bitSize % 8;
      if (trailingBitsCount) {
        const a = bitArrayByteAt(
          this.rawBuffer,
          this.bitOffset,
          wholeByteCount,
        );
        const b = bitArrayByteAt(
          other.rawBuffer,
          other.bitOffset,
          wholeByteCount,
        );

        const unusedLowBitCount = 8 - trailingBitsCount;
        if (a >> unusedLowBitCount !== b >> unusedLowBitCount) {
          return false;
        }
      }
    }

    return true;
  }

  /**
   * Returns this bit array's internal buffer.
   *
   * @deprecated Use `BitArray.byteAt()` or `BitArray.rawBuffer` instead.
   *
   * @returns {Uint8Array}
   */
  get buffer() {
    bitArrayPrintDeprecationWarning(
      "buffer",
      "Use BitArray.byteAt() or BitArray.rawBuffer instead",
    );

    if (this.bitOffset !== 0 || this.bitSize % 8 !== 0) {
      throw new globalThis.Error(
        "BitArray.buffer does not support unaligned bit arrays",
      );
    }

    return this.rawBuffer;
  }

  /**
   * Returns the length in bytes of this bit array's internal buffer.
   *
   * @deprecated Use `BitArray.bitSize` or `BitArray.byteSize` instead.
   *
   * @returns {number}
   */
  get length() {
    bitArrayPrintDeprecationWarning(
      "length",
      "Use BitArray.bitSize or BitArray.byteSize instead",
    );

    if (this.bitOffset !== 0 || this.bitSize % 8 !== 0) {
      throw new globalThis.Error(
        "BitArray.length does not support unaligned bit arrays",
      );
    }

    return this.rawBuffer.length;
  }
}

/**
 * Returns the nth byte in the given buffer, after applying the specified bit
 * offset. If the index is out of bounds then zero is returned.
 *
 * @param {Uint8Array} buffer
 * @param {number} bitOffset
 * @param {number} index
 * @returns {number}
 */
function bitArrayByteAt(buffer, bitOffset, index) {
  if (bitOffset === 0) {
    return buffer[index] ?? 0;
  } else {
    const a = (buffer[index] << bitOffset) & 0xff;
    const b = buffer[index + 1] >> (8 - bitOffset);

    return a | b;
  }
}

export class UtfCodepoint {
  constructor(value) {
    this.value = value;
  }
}

const isBitArrayDeprecationMessagePrinted = {};
function bitArrayPrintDeprecationWarning(name, message) {
  if (isBitArrayDeprecationMessagePrinted[name]) {
    return;
  }

  console.warn(
    `Deprecated BitArray.${name} property used in JavaScript FFI code. ${message}.`,
  );

  isBitArrayDeprecationMessagePrinted[name] = true;
}

/**
 * @internal
 *
 * Slices a bit array to produce a new bit array. If `end` is not supplied then
 * all bits from `start` onward are returned.
 *
 * If the slice is out of bounds then an exception is thrown.
 *
 * @param {BitArray} bitArray
 * @param {number} start
 * @param {number} [end]
 * @returns {BitArray}
 */
export function bitArraySlice(bitArray, start, end) {
  end ??= bitArray.bitSize;

  bitArrayValidateRange(bitArray, start, end);

  // Handle zero-length slices
  if (start === end) {
    return new BitArray(new Uint8Array());
  }

  // Early return for slices that cover the whole bit array
  if (start === 0 && end === bitArray.bitSize) {
    return bitArray;
  }

  start += bitArray.bitOffset;
  end += bitArray.bitOffset;

  const startByteIndex = Math.trunc(start / 8);
  const endByteIndex = Math.trunc((end + 7) / 8);
  const byteLength = endByteIndex - startByteIndex;

  // Avoid creating a new Uint8Array if the view of the underlying ArrayBuffer
  // is the same. This can occur when slicing off just the first or last bit of
  // a bit array, i.e. when only the bit offset or bit size need to be updated.
  let buffer;
  if (startByteIndex === 0 && byteLength === bitArray.rawBuffer.byteLength) {
    buffer = bitArray.rawBuffer;
  } else {
    buffer = new Uint8Array(
      bitArray.rawBuffer.buffer,
      bitArray.rawBuffer.byteOffset + startByteIndex,
      byteLength,
    );
  }

  return new BitArray(buffer, end - start, start % 8);
}

/**
 * Interprets a slice of this bit array as a floating point number, either
 * 32-bit or 64-bit, with the specified endianness.
 *
 * The value of `end - start` must be exactly 32 or 64, otherwise an exception
 * will be thrown.
 *
 * @param {BitArray} bitArray
 * @param {number} start
 * @param {number} end
 * @param {boolean} isBigEndian
 * @returns {number}
 */
export function bitArraySliceToFloat(bitArray, start, end, isBigEndian) {
  bitArrayValidateRange(bitArray, start, end);

  const floatSize = end - start;

  // Check size is valid
  if (floatSize !== 16 && floatSize !== 32 && floatSize !== 64) {
    const msg =
      `Sized floats must be 16-bit, 32-bit or 64-bit, got size of ` +
      `${floatSize} bits`;
    throw new globalThis.Error(msg);
  }

  start += bitArray.bitOffset;

  const isStartByteAligned = start % 8 === 0;

  // If the bit range is byte aligned then the float can be read directly out
  // of the existing buffer
  if (isStartByteAligned) {
    const view = new DataView(
      bitArray.rawBuffer.buffer,
      bitArray.rawBuffer.byteOffset + start / 8,
    );

    if (floatSize === 64) {
      return view.getFloat64(0, !isBigEndian);
    } else if (floatSize === 32) {
      return view.getFloat32(0, !isBigEndian);
    } else if (floatSize === 16) {
      return fp16UintToNumber(view.getUint16(0, !isBigEndian));
    }
  }

  // Copy the unaligned bytes into an aligned array so a DataView can be used
  const alignedBytes = new Uint8Array(floatSize / 8);
  const byteOffset = Math.trunc(start / 8);
  for (let i = 0; i < alignedBytes.length; i++) {
    alignedBytes[i] = bitArrayByteAt(
      bitArray.rawBuffer,
      start % 8,
      byteOffset + i,
    );
  }

  // Read the float out of the aligned buffer
  const view = new DataView(alignedBytes.buffer);
  if (floatSize === 64) {
    return view.getFloat64(0, !isBigEndian);
  } else if (floatSize === 32) {
    return view.getFloat32(0, !isBigEndian);
  } else {
    return fp16UintToNumber(view.getUint16(0, !isBigEndian));
  }
}

/**
 * Interprets a slice of this bit array as a signed or unsigned integer with the
 * specified endianness.
 *
 * @param {BitArray} bitArray
 * @param {number} start
 * @param {number} end
 * @param {boolean} isBigEndian
 * @param {boolean} isSigned
 * @returns {number}
 */
export function bitArraySliceToInt(bitArray, start, end, isBigEndian, isSigned) {
  bitArrayValidateRange(bitArray, start, end);

  if (start === end) {
    return 0;
  }

  start += bitArray.bitOffset;
  end += bitArray.bitOffset;

  const isStartByteAligned = start % 8 === 0;
  const isEndByteAligned = end % 8 === 0;

  // If the slice is byte-aligned then there is no need to handle unaligned
  // slices, meaning a simpler and faster implementation can be used instead
  if (isStartByteAligned && isEndByteAligned) {
    return intFromAlignedSlice(
      bitArray,
      start / 8,
      end / 8,
      isBigEndian,
      isSigned,
    );
  }

  const size = end - start;

  const startByteIndex = Math.trunc(start / 8);
  const endByteIndex = Math.trunc((end - 1) / 8);

  // Handle the case of the slice being completely contained in a single byte
  if (startByteIndex == endByteIndex) {
    const mask = 0xff >> start % 8;
    const unusedLowBitCount = (8 - (end % 8)) % 8;

    let value =
      (bitArray.rawBuffer[startByteIndex] & mask) >> unusedLowBitCount;

    // For signed integers, if the high bit is set reinterpret as two's
    // complement
    if (isSigned) {
      const highBit = 2 ** (size - 1);
      if (value >= highBit) {
        value -= highBit * 2;
      }
    }

    return value;
  }

  // The integer value to be read is not aligned and crosses at least one byte
  // boundary in the input array

  if (size <= 53) {
    return intFromUnalignedSliceUsingNumber(
      bitArray.rawBuffer,
      start,
      end,
      isBigEndian,
      isSigned,
    );
  } else {
    return intFromUnalignedSliceUsingBigInt(
      bitArray.rawBuffer,
      start,
      end,
      isBigEndian,
      isSigned,
    );
  }
}

/**
 * @internal
 *
 * Joins the given segments into a new bit array, tightly packing them together.
 * Each segment must be one of the following types:
 *
 * - A `number`: A single byte value in the range 0-255. Values outside this
 *   range will be wrapped.
 * - A `Uint8Array`: A sequence of byte values of any length.
 * - A `BitArray`: A sequence of bits of any length, which may not be byte
 *   aligned.
 *
 * The bit size of the returned bit array will be the sum of the size in bits
 * of the input segments.
 *
 * @param {(number | Uint8Array | BitArray)[]} segments
 * @returns {BitArray}
 */
export function toBitArray(segments) {
  if (segments.length === 0) {
    return new BitArray(new Uint8Array());
  }

  if (segments.length === 1) {
    const segment = segments[0];

    // When there is a single BitArray segment it can be returned as-is
    if (segment instanceof BitArray) {
      return segment;
    }

    // When there is a single Uint8Array segment, pass it directly to the bit
    // array constructor to avoid a copy
    if (segment instanceof Uint8Array) {
      return new BitArray(segment);
    }

    return new BitArray(new Uint8Array(/** @type {number[]} */ (segments)));
  }

  // Count the total number of bits and check if all segments are numbers, i.e.
  // single bytes
  let bitSize = 0;
  let areAllSegmentsNumbers = true;
  for (const segment of segments) {
    if (segment instanceof BitArray) {
      bitSize += segment.bitSize;
      areAllSegmentsNumbers = false;
    } else if (segment instanceof Uint8Array) {
      bitSize += segment.byteLength * 8;
      areAllSegmentsNumbers = false;
    } else {
      bitSize += 8;
    }
  }

  // If all segments are numbers then pass the segments array directly to the
  // Uint8Array constructor
  if (areAllSegmentsNumbers) {
    return new BitArray(new Uint8Array(/** @type {number[]} */ (segments)));
  }

  // Pack the segments into a Uint8Array
  const buffer = new Uint8Array(Math.trunc((bitSize + 7) / 8));

  // The current write position in bits into the above array. Byte-aligned
  // segments, i.e. when the cursor is a multiple of 8, are able to be processed
  // faster due to being able to copy bytes directly.
  let cursor = 0;

  for (let segment of segments) {
    const isCursorByteAligned = cursor % 8 === 0;

    if (segment instanceof BitArray) {
      if (isCursorByteAligned && segment.bitOffset === 0) {
        buffer.set(segment.rawBuffer, cursor / 8);
        cursor += segment.bitSize;

        // Zero any unused bits in the last byte of the buffer. Their content is
        // undefined and shouldn't be included in the output.
        const trailingBitsCount = segment.bitSize % 8;
        if (trailingBitsCount !== 0) {
          const lastByteIndex = Math.trunc(cursor / 8);
          buffer[lastByteIndex] >>= 8 - trailingBitsCount;
          buffer[lastByteIndex] <<= 8 - trailingBitsCount;
        }
      } else {
        appendUnalignedBits(
          segment.rawBuffer,
          segment.bitSize,
          segment.bitOffset,
        );
      }
    } else if (segment instanceof Uint8Array) {
      if (isCursorByteAligned) {
        buffer.set(segment, cursor / 8);
        cursor += segment.byteLength * 8;
      } else {
        appendUnalignedBits(segment, segment.byteLength * 8, 0);
      }
    } else {
      if (isCursorByteAligned) {
        buffer[cursor / 8] = segment;
        cursor += 8;
      } else {
        appendUnalignedBits(new Uint8Array([segment]), 8, 0);
      }
    }
  }

  function appendUnalignedBits(unalignedBits, size, offset) {
    if (size === 0) {
      return;
    }

    const byteSize = Math.trunc(size + 7 / 8);

    const highBitsCount = cursor % 8;
    const lowBitsCount = 8 - highBitsCount;

    let byteIndex = Math.trunc(cursor / 8);

    for (let i = 0; i < byteSize; i++) {
      let byte = bitArrayByteAt(unalignedBits, offset, i);

      // If this is a partial byte then zero out the trailing bits as their
      // content is undefined and shouldn't be included in the output
      if (size < 8) {
        byte >>= 8 - size;
        byte <<= 8 - size;
      }

      // Copy the high bits of the input byte to the low bits of the current
      // output byte
      buffer[byteIndex] |= byte >> highBitsCount;

      let appendedBitsCount = size - Math.max(0, size - lowBitsCount);
      size -= appendedBitsCount;
      cursor += appendedBitsCount;

      if (size === 0) {
        break;
      }

      // Copy the low bits of the input byte to the high bits of the next output
      // byte
      buffer[++byteIndex] = byte << lowBitsCount;
      appendedBitsCount = size - Math.max(0, size - highBitsCount);
      size -= appendedBitsCount;
      cursor += appendedBitsCount;
    }
  }

  return new BitArray(buffer, bitSize);
}

/**
 * @internal
 *
 * Encodes a floating point value into a `Uint8Array`. This is used to create
 * float segments that are part of bit array expressions.
 *
 * @param {number} value
 * @param {number} size
 * @param {boolean} isBigEndian
 * @returns {Uint8Array}
 */
export function sizedFloat(value, size, isBigEndian) {
  if (size !== 16 && size !== 32 && size !== 64) {
    const msg =
      `Sized floats must be 16-bit, 32-bit or 64-bit, got size of ${size} bits`;
    throw new globalThis.Error(msg);
  }

  if (size === 16) {
    return numberToFp16Uint(value, isBigEndian);
  }

  const buffer = new Uint8Array(size / 8);

  const view = new DataView(buffer.buffer);

  if (size == 64) {
    view.setFloat64(0, value, !isBigEndian);
  } else {
    view.setFloat32(0, value, !isBigEndian);
  }

  return buffer;
}

/**
 * @internal
 *
 * Encodes an integer value into a `Uint8Array`, or a `BitArray` if the size in
 * bits is not a multiple of 8. This is used to create integer segments used in
 * bit array expressions.
 *
 * @param {number} value
 * @param {number} size
 * @param {boolean} isBigEndian
 * @returns {Uint8Array | BitArray}
 */
export function sizedInt(value, size, isBigEndian) {
  if (size <= 0) {
    return new Uint8Array();
  }

  // Fast path when size is 8 bits. This relies on the rounding behavior of the
  // Uint8Array constructor.
  if (size === 8) {
    return new Uint8Array([value]);
  }

  // Fast path when size is less than 8 bits: shift the value up to the high
  // bits
  if (size < 8) {
    value <<= 8 - size;
    return new BitArray(new Uint8Array([value]), size);
  }

  // Allocate output buffer
  const buffer = new Uint8Array(Math.trunc((size + 7) / 8));

  // The number of trailing bits in the final byte. Will be zero if the size is
  // an exact number of bytes.
  const trailingBitsCount = size % 8;

  // The number of unused bits in the final byte of the buffer
  const unusedBitsCount = 8 - trailingBitsCount;

  // For output sizes not exceeding 32 bits the number type is used. For larger
  // output sizes the BigInt type is needed.
  //
  // The code in each of these two paths must be kept in sync.
  if (size <= 32) {
    if (isBigEndian) {
      let i = buffer.length - 1;

      // Set the trailing bits at the end of the output buffer
      if (trailingBitsCount) {
        buffer[i--] = (value << unusedBitsCount) & 0xff;
        value >>= trailingBitsCount;
      }

      for (; i >= 0; i--) {
        buffer[i] = value;
        value >>= 8;
      }
    } else {
      let i = 0;

      const wholeByteCount = Math.trunc(size / 8);
      for (; i < wholeByteCount; i++) {
        buffer[i] = value;
        value >>= 8;
      }

      // Set the trailing bits at the end of the output buffer
      if (trailingBitsCount) {
        buffer[i] = value << unusedBitsCount;
      }
    }
  } else {
    const bigTrailingBitsCount = BigInt(trailingBitsCount);
    const bigUnusedBitsCount = BigInt(unusedBitsCount);

    let bigValue = BigInt(value);

    if (isBigEndian) {
      let i = buffer.length - 1;

      // Set the trailing bits at the end of the output buffer
      if (trailingBitsCount) {
        buffer[i--] = Number(bigValue << bigUnusedBitsCount);
        bigValue >>= bigTrailingBitsCount;
      }

      for (; i >= 0; i--) {
        buffer[i] = Number(bigValue);
        bigValue >>= 8n;
      }
    } else {
      let i = 0;

      const wholeByteCount = Math.trunc(size / 8);
      for (; i < wholeByteCount; i++) {
        buffer[i] = Number(bigValue);
        bigValue >>= 8n;
      }

      // Set the trailing bits at the end of the output buffer
      if (trailingBitsCount) {
        buffer[i] = Number(bigValue << bigUnusedBitsCount);
      }
    }
  }

  // Integers that aren't a whole number of bytes are returned as a BitArray so
  // their size in bits is tracked
  if (trailingBitsCount) {
    return new BitArray(buffer, size);
  }

  return buffer;
}

/**
 * Reads an aligned slice of any size as an integer.
 *
 * @param {BitArray} bitArray
 * @param {number} start
 * @param {number} end
 * @param {boolean} isBigEndian
 * @param {boolean} isSigned
 * @returns {number}
 */
function intFromAlignedSlice(bitArray, start, end, isBigEndian, isSigned) {
  const byteSize = end - start;

  if (byteSize <= 6) {
    return intFromAlignedSliceUsingNumber(
      bitArray.rawBuffer,
      start,
      end,
      isBigEndian,
      isSigned,
    );
  } else {
    return intFromAlignedSliceUsingBigInt(
      bitArray.rawBuffer,
      start,
      end,
      isBigEndian,
      isSigned,
    );
  }
}

/**
 * Reads an aligned slice up to 48 bits in size as an integer. Uses the
 * JavaScript `number` type internally.
 *
 * @param {Uint8Array} buffer
 * @param {number} start
 * @param {number} end
 * @param {boolean} isBigEndian
 * @param {boolean} isSigned
 * @returns {number}
 */
function intFromAlignedSliceUsingNumber(
  buffer,
  start,
  end,
  isBigEndian,
  isSigned,
) {
  const byteSize = end - start;

  let value = 0;

  // Read bytes as an unsigned integer
  if (isBigEndian) {
    for (let i = start; i < end; i++) {
      value *= 256;
      value += buffer[i];
    }
  } else {
    for (let i = end - 1; i >= start; i--) {
      value *= 256;
      value += buffer[i];
    }
  }

  // For signed integers, if the high bit is set reinterpret as two's
  // complement
  if (isSigned) {
    const highBit = 2 ** (byteSize * 8 - 1);
    if (value >= highBit) {
      value -= highBit * 2;
    }
  }

  return value;
}

/**
 * Reads an aligned slice of any size as an integer. Uses the JavaScript
 * `BigInt` type internally.
 *
 * @param {Uint8Array} buffer
 * @param {number} start
 * @param {number} end
 * @param {boolean} isBigEndian
 * @param {boolean} isSigned
 * @returns {number}
 */
function intFromAlignedSliceUsingBigInt(
  buffer,
  start,
  end,
  isBigEndian,
  isSigned,
) {
  const byteSize = end - start;

  let value = 0n;

  // Read bytes as an unsigned integer value
  if (isBigEndian) {
    for (let i = start; i < end; i++) {
      value *= 256n;
      value += BigInt(buffer[i]);
    }
  } else {
    for (let i = end - 1; i >= start; i--) {
      value *= 256n;
      value += BigInt(buffer[i]);
    }
  }

  // For signed integers, if the high bit is set reinterpret as two's
  // complement
  if (isSigned) {
    const highBit = 1n << BigInt(byteSize * 8 - 1);
    if (value >= highBit) {
      value -= highBit * 2n;
    }
  }

  // Convert the result into a JS number. This may cause quantizing/error on
  // values outside JavaScript's safe integer range.
  return Number(value);
}

/**
 * Reads an unaligned slice up to 53 bits in size as an integer. Uses the
 * JavaScript `number` type internally.
 *
 * This function assumes that the slice crosses at least one byte boundary in
 * the input.
 *
 * @param {Uint8Array} buffer
 * @param {number} start
 * @param {number} end
 * @param {boolean} isBigEndian
 * @param {boolean} isSigned
 * @returns {number}
 */
function intFromUnalignedSliceUsingNumber(
  buffer,
  start,
  end,
  isBigEndian,
  isSigned,
) {
  const isStartByteAligned = start % 8 === 0;

  let size = end - start;
  let byteIndex = Math.trunc(start / 8);

  let value = 0;

  if (isBigEndian) {
    // Read any leading bits
    if (!isStartByteAligned) {
      const leadingBitsCount = 8 - (start % 8);
      value = buffer[byteIndex++] & ((1 << leadingBitsCount) - 1);
      size -= leadingBitsCount;
    }

    // Read any whole bytes
    while (size >= 8) {
      value *= 256;
      value += buffer[byteIndex++];
      size -= 8;
    }

    // Read any trailing bits
    if (size > 0) {
      value *= 2 ** size;
      value += buffer[byteIndex] >> (8 - size);
    }
  } else {
    // For little endian, if the start is aligned then whole bytes can be read
    // directly out of the input array, with the trailing bits handled at the
    // end
    if (isStartByteAligned) {
      let size = end - start;
      let scale = 1;

      // Read whole bytes
      while (size >= 8) {
        value += buffer[byteIndex++] * scale;
        scale *= 256;
        size -= 8;
      }

      // Read trailing bits
      value += (buffer[byteIndex] >> (8 - size)) * scale;
    } else {
      // Read little endian data where the start is not byte-aligned. This is
      // done by reading whole bytes that cross a byte boundary in the input
      // data, then reading any trailing bits.

      const highBitsCount = start % 8;
      const lowBitsCount = 8 - highBitsCount;

      let size = end - start;
      let scale = 1;

      // Extract whole bytes
      while (size >= 8) {
        const byte =
          (buffer[byteIndex] << highBitsCount) |
          (buffer[byteIndex + 1] >> lowBitsCount);

        value += (byte & 0xff) * scale;

        scale *= 256;
        size -= 8;
        byteIndex++;
      }

      // Read any trailing bits. These trailing bits may cross a byte boundary
      // in the input buffer.
      if (size > 0) {
        const lowBitsUsed = size - Math.max(0, size - lowBitsCount);

        let trailingByte =
          (buffer[byteIndex] & ((1 << lowBitsCount) - 1)) >>
          (lowBitsCount - lowBitsUsed);

        size -= lowBitsUsed;

        if (size > 0) {
          trailingByte *= 2 ** size;
          trailingByte += buffer[byteIndex + 1] >> (8 - size);
        }

        value += trailingByte * scale;
      }
    }
  }

  // For signed integers, if the high bit is set reinterpret as two's
  // complement
  if (isSigned) {
    const highBit = 2 ** (end - start - 1);
    if (value >= highBit) {
      value -= highBit * 2;
    }
  }

  return value;
}

/**
 * @internal
 *
 * Reads an unaligned slice of any size as an integer. Uses the JavaScript
 * `BigInt` type internally.
 *
 * This function assumes that the slice crosses at least one byte boundary in
 * the input.
 *
 * @param {Uint8Array} buffer
 * @param {number} start
 * @param {number} end
 * @param {boolean} isBigEndian
 * @param {boolean} isSigned
 * @returns {number}
 */
function intFromUnalignedSliceUsingBigInt(
  buffer,
  start,
  end,
  isBigEndian,
  isSigned,
) {
  const isStartByteAligned = start % 8 === 0;

  let size = end - start;
  let byteIndex = Math.trunc(start / 8);

  let value = 0n;

  if (isBigEndian) {
    // Read any leading bits
    if (!isStartByteAligned) {
      const leadingBitsCount = 8 - (start % 8);
      value = BigInt(buffer[byteIndex++] & ((1 << leadingBitsCount) - 1));
      size -= leadingBitsCount;
    }

    // Read any whole bytes
    while (size >= 8) {
      value *= 256n;
      value += BigInt(buffer[byteIndex++]);
      size -= 8;
    }

    // Read any trailing bits
    if (size > 0) {
      value <<= BigInt(size);
      value += BigInt(buffer[byteIndex] >> (8 - size));
    }
  } else {
    // For little endian, if the start is aligned then whole bytes can be read
    // directly out of the input array, with the trailing bits handled at the
    // end
    if (isStartByteAligned) {
      let size = end - start;
      let shift = 0n;

      // Read whole bytes
      while (size >= 8) {
        value += BigInt(buffer[byteIndex++]) << shift;
        shift += 8n;
        size -= 8;
      }

      // Read trailing bits
      value += BigInt(buffer[byteIndex] >> (8 - size)) << shift;
    } else {
      // Read little endian data where the start is not byte-aligned. This is
      // done by reading whole bytes that cross a byte boundary in the input
      // data, then reading any trailing bits.

      const highBitsCount = start % 8;
      const lowBitsCount = 8 - highBitsCount;

      let size = end - start;
      let shift = 0n;

      // Extract whole bytes
      while (size >= 8) {
        const byte =
          (buffer[byteIndex] << highBitsCount) |
          (buffer[byteIndex + 1] >> lowBitsCount);

        value += BigInt(byte & 0xff) << shift;

        shift += 8n;
        size -= 8;
        byteIndex++;
      }

      // Read any trailing bits. These trailing bits may cross a byte boundary
      // in the input buffer.
      if (size > 0) {
        const lowBitsUsed = size - Math.max(0, size - lowBitsCount);

        let trailingByte =
          (buffer[byteIndex] & ((1 << lowBitsCount) - 1)) >>
          (lowBitsCount - lowBitsUsed);

        size -= lowBitsUsed;

        if (size > 0) {
          trailingByte <<= size;
          trailingByte += buffer[byteIndex + 1] >> (8 - size);
        }

        value += BigInt(trailingByte) << shift;
      }
    }
  }

  // For signed integers, if the high bit is set reinterpret as two's
  // complement
  if (isSigned) {
    const highBit = 2n ** BigInt(end - start - 1);
    if (value >= highBit) {
      value -= highBit * 2n;
    }
  }

  // Convert the result into a JS number. This may cause quantizing/error on
  // values outside JavaScript's safe integer range.
  return Number(value);
}

/**
 * Interprets a 16-bit unsigned integer value as a 16-bit floating point value.
 * 
 * @param {number} intValue
 * @returns {number}
 */
function fp16UintToNumber(intValue) {
  const sign = intValue >= 0x8000 ? -1 : 1;
  const exponent = (intValue & 0x7c00) >> 10;
  const fraction = intValue & 0x03ff;

  let value;
  if (exponent === 0) {
    value = 6.103515625e-5 * (fraction / 0x400);
  } else if (exponent === 0x1f) {
    value = fraction === 0 ? Infinity : NaN;
  } else {
    value = Math.pow(2, exponent - 15) * (1 + fraction / 0x400);
  }

  return sign * value;
}

/**
 * Converts a floating point number to bytes for a 16-bit floating point value.
 *
 * @param {number} intValue
 * @param {boolean} isBigEndian
 * @returns {Uint8Array}
 */
function numberToFp16Uint(value, isBigEndian) {
  const buffer = new Uint8Array(2);

  if (isNaN(value)) {
    buffer[1] = 0x7e;
  } else if (value === Infinity) {
    buffer[1] = 0x7c;
  } else if (value === -Infinity) {
    buffer[1] = 0xfc;
  } else if (value === 0) {
    // Both values are already zero
  } else {
    const sign = value < 0 ? 1 : 0;
    value = Math.abs(value);

    let exponent = Math.floor(Math.log2(value));
    let fraction = value / Math.pow(2, exponent) - 1;

    exponent += 15;

    if (exponent <= 0) {
      exponent = 0;
      fraction = value / Math.pow(2, -14);
    } else if (exponent >= 31) {
      exponent = 31;
      fraction = 0;
    }

    fraction = Math.round(fraction * 1024);

    buffer[1] =
      (sign << 7) | ((exponent & 0x1f) << 2) | ((fraction >> 8) & 0x03);
    buffer[0] = fraction & 0xff;
  }

  if (isBigEndian) {
    const a = buffer[0];
    buffer[0] = buffer[1];
    buffer[1] = a;
  }

  return buffer;
}

/**
 * Throws an exception if the given start and end values are out of bounds for
 * a bit array.
 *
 * @param {BitArray} bitArray
 * @param {number} start
 * @param {number} end
 */
function bitArrayValidateRange(bitArray, start, end) {
  if (
    start < 0 ||
    start > bitArray.bitSize ||
    end < start ||
    end > bitArray.bitSize
  ) {
    const msg =
      `Invalid bit array slice: start = ${start}, end = ${end}, ` +
      `bit size = ${bitArray.bitSize}`;
    throw new globalThis.Error(msg);
  }
}

/** @type {TextEncoder | undefined} */
let utf8Encoder;

/**
 * @internal
 *
 * Returns the UTF-8 bytes for a string.
 *
 * @param {string} string
 * @returns {Uint8Array}
 */
export function stringBits(string) {
  utf8Encoder ??= new TextEncoder();
  return utf8Encoder.encode(string);
}

/**
 * @internal
 *
 * Returns the UTF-8 bytes for a single UTF codepoint.
 *
 * @param {UtfCodepoint} codepoint
 * @returns {Uint8Array}
 */
export function codepointBits(codepoint) {
  return stringBits(String.fromCodePoint(codepoint.value));
}

export class Result extends CustomType {
  // @internal
  static isResult(data) {
    return data instanceof Result;
  }
}

export class Ok extends Result {
  constructor(value) {
    super();
    this[0] = value;
  }

  // @internal
  isOk() {
    return true;
  }
}

export class Error extends Result {
  constructor(detail) {
    super();
    this[0] = detail;
  }

  // @internal
  isOk() {
    return false;
  }
}

export function isEqual(x, y) {
  let values = [x, y];

  while (values.length) {
    let a = values.pop();
    let b = values.pop();
    if (a === b) continue;

    if (!isObject(a) || !isObject(b)) return false;
    let unequal =
      !structurallyCompatibleObjects(a, b) ||
      unequalDates(a, b) ||
      unequalBuffers(a, b) ||
      unequalArrays(a, b) ||
      unequalMaps(a, b) ||
      unequalSets(a, b) ||
      unequalRegExps(a, b);
    if (unequal) return false;

    const proto = Object.getPrototypeOf(a);
    if (proto !== null && typeof proto.equals === "function") {
      try {
        if (a.equals(b)) continue;
        else return false;
      } catch {}
    }

    let [keys, get] = getters(a);
    for (let k of keys(a)) {
      values.push(get(a, k), get(b, k));
    }
  }

  return true;
}

function getters(object) {
  if (object instanceof Map) {
    return [(x) => x.keys(), (x, y) => x.get(y)];
  } else {
    let extra = object instanceof globalThis.Error ? ["message"] : [];
    return [(x) => [...extra, ...Object.keys(x)], (x, y) => x[y]];
  }
}

function unequalDates(a, b) {
  return a instanceof Date && (a > b || a < b);
}

function unequalBuffers(a, b) {
  return (
    !(a instanceof BitArray) &&
    a.buffer instanceof ArrayBuffer &&
    a.BYTES_PER_ELEMENT &&
    !(a.byteLength === b.byteLength && a.every((n, i) => n === b[i]))
  );
}

function unequalArrays(a, b) {
  return Array.isArray(a) && a.length !== b.length;
}

function unequalMaps(a, b) {
  return a instanceof Map && a.size !== b.size;
}

function unequalSets(a, b) {
  return (
    a instanceof Set && (a.size != b.size || [...a].some((e) => !b.has(e)))
  );
}

function unequalRegExps(a, b) {
  return a instanceof RegExp && (a.source !== b.source || a.flags !== b.flags);
}

function isObject(a) {
  return typeof a === "object" && a !== null;
}

function structurallyCompatibleObjects(a, b) {
  if (typeof a !== "object" && typeof b !== "object" && (!a || !b))
    return false;

  let nonstructural = [Promise, WeakSet, WeakMap, Function];
  if (nonstructural.some((c) => a instanceof c)) return false;

  return a.constructor === b.constructor;
}

// @internal
export function remainderInt(a, b) {
  if (b === 0) {
    return 0;
  } else {
    return a % b;
  }
}

// @internal
export function divideInt(a, b) {
  return Math.trunc(divideFloat(a, b));
}

// @internal
export function divideFloat(a, b) {
  if (b === 0) {
    return 0;
  } else {
    return a / b;
  }
}

// @internal
export function makeError(variant, module, line, fn, message, extra) {
  let error = new globalThis.Error(message);
  error.gleam_error = variant;
  error.module = module;
  error.line = line;
  error.function = fn;
  // TODO: Remove this with Gleam v2.0.0
  error.fn = fn;
  for (let k in extra) error[k] = extra[k];
  return error;
}
