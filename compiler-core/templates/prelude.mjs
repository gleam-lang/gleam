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
    for (let _ of this) {
      if (desired <= 0) return true;
      desired--;
    }
    return desired <= 0;
  }

  // @internal
  hasLength(desired) {
    for (let _ of this) {
      if (desired <= 0) return false;
      desired--;
    }
    return desired === 0;
  }

  // @internal
  countLength() {
    let length = 0;
    for (let _ of this) length++;
    return length;
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
 * A bit array is a contiguous sequence of bits equivalent to Erlang's Binary
 * type.
 *
 * When the bit size is not a multiple of 8 some of the low bits in the last
 * byte of the bit array's buffer are unused and their value is undefined.
 */
export class BitArray {
  /** @type {Uint8Array} */
  #buffer;

  /** @type {number} */
  #bitSize;

  /** @type {number} */
  #byteSize;

  /** @type {boolean} */
  #isWholeBytes;

  /**
   * Constructs a new bit array from a `Uint8Array` and an optional size in
   * bits. If no bit size is specified then it is taken as `buffer.length * 8`,
   * i.e. all bytes in the buffer are used.
   *
   * @param {Uint8Array} buffer
   * @param {number} [bitSize]
   */
  constructor(buffer, bitSize) {
    if (!(buffer instanceof Uint8Array)) {
      throw globalThis.Error(
        "BitArray can only be constructed from a Uint8Array",
      );
    }

    this.#bitSize = bitSize ?? buffer.length * 8;
    this.#byteSize = Math.trunc((this.#bitSize + 7) / 8);

    // If no bit size is provided then assume there is a whole number of bytes,
    // and if one is provided check that it is valid for the buffer's size
    if (buffer.length !== this.#byteSize) {
      throw globalThis.Error(
        "BitArray byte size is invalid for the buffer's length",
      );
    }

    this.#buffer = buffer;

    this.#isWholeBytes = this.#bitSize % 8 === 0;
  }

  /**
   * This bit array's raw `Uint8Array` buffer.
   *
   * Code that reads this buffer directly must be able to handle unaligned
   * binary data it may contain. It is often preferable to use other functions
   * such as `BitArray.byteAt()` instead of working directly with the internal
   * buffer.
   *
   * @returns {Uint8Array}
   */
  get rawBuffer() {
    return this.#buffer;
  }

  /**
   * The number of bits in this bit array.
   *
   * @returns {number}
   */
  get bitSize() {
    return this.#bitSize;
  }

  /**
   * The number of bytes in this bit array. for bit arrays with a bit size that
   * is not a multiple of 8, the byte size value is rounded up.
   *
   * E.g. if `bitSize` is 14 then `byteSize` will be 2.
   *
   * @returns {number}
   */
  get byteSize() {
    return this.#byteSize;
  }

  /**
   * Whether this bit array stores a whole number of bytes. This is the same as
   * checking if `bitSize` is a multiple of 8.
   *
   * @returns {boolean}
   */
  get isWholeBytes() {
    return this.#isWholeBytes;
  }

  /**
   * Returns the byte at the given index in this bit array. If this bit array is
   * not a whole number of bytes, or the byte index is out of range, then an
   * error is thrown.
   *
   * @param {number} index
   * @returns {number}
   */
  byteAt(index) {
    if (!this.isWholeBytes) {
      throw new globalThis.Error(
        `BitArray.byteAt() called on a bit array that isn't a whole number of bytes`,
      );
    }

    let byte = this.#buffer[index];

    if (byte === undefined) {
      throw new globalThis.Error(
        `BitArray.byteAt() index out of range: ${index}, byteSize: ${this.byteSize}`,
      );
    }

    return byte;
  }

  /** @internal */
  equals(other) {
    if (this.bitSize !== other.bitSize) {
      return false;
    }

    // Compare any whole bytes
    const wholeByteCount = Math.trunc(this.bitSize / 8);
    for (let i = 0; i < wholeByteCount; i++) {
      if (this.#buffer[i] !== other.#buffer[i]) {
        return false;
      }
    }

    // Compare any trailing bits, excluding unused low bits from the comparison
    const trailingBitsCount = this.bitSize % 8;
    if (trailingBitsCount) {
      const unusedLowBitCount = 8 - trailingBitsCount;
      if (
        this.#buffer[wholeByteCount] >> unusedLowBitCount !==
        other.#buffer[wholeByteCount] >> unusedLowBitCount
      ) {
        return false;
      }
    }

    return true;
  }

  /** @internal */
  slice(start, end) {
    return bitArraySlice(this, start, end);
  }

  /** @internal */
  sliceToFloat(start, end, isBigEndian) {
    return bitArraySliceToFloat(this, start, end, isBigEndian);
  }

  /** @internal */
  sliceToInt(start, end, isBigEndian, isSigned) {
    return bitArraySliceToInt(this, start, end, isBigEndian, isSigned);
  }

  /**
   * @deprecated
   *
   * Returns this bit array's internal buffer.
   *
   * @returns {Uint8Array}
   */
  get buffer() {
    bitArrayPrintDeprecationWarning(
      "buffer",
      "Use `BitArray.byteSize`, `BitArray.byteAt()`, or `BitArray.rawBuffer` " +
        "instead.",
    );

    return this.rawBuffer;
  }

  /**
   * @deprecated Use `BitArray.bitSize` or `BitArray.byteSize` instead.
   *
   * Returns the length in bytes of this bit array's internal buffer.
   *
   * @returns {number}
   */
  get length() {
    bitArrayPrintDeprecationWarning(
      "length",
      "Use `BitArray.bitSize` or `BitArray.byteSize` instead.",
    );

    return this.rawBuffer.length;
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

  console.warn(`Deprecated BitArray property used`);
  console.warn(`  Name: BitArray.${name}`);
  console.warn(`  Message: ${message}`);

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
 * @param {number} start The start offset of the slice in bits.
 * @param {number} [end] The end offset of the slice in bits. If this is not
 * specified then all bits from `start` onward are returned.
 * @returns {BitArray}
 */
export function bitArraySlice(bitArray, start, end) {
  end ??= bitArray.bitSize;

  bitArrayValidateRange(bitArray, start, end);

  // Handle zero-length slices
  if (start === end) {
    return new BitArray(new Uint8Array());
  }

  // Handle slices that cover the whole bit array
  if (start === 0 && end === bitArray.bitSize) {
    return bitArray;
  }

  const isStartByteAligned = start % 8 === 0;

  // When start is byte-aligned the existing ArrayBuffer is reused, avoiding a
  // copy
  if (isStartByteAligned) {
    const startIndex = Math.trunc(start / 8);
    const endIndex = Math.trunc((end + 7) / 8);

    const buffer = new Uint8Array(
      bitArray.rawBuffer.buffer,
      bitArray.rawBuffer.byteOffset + startIndex,
      endIndex - startIndex,
    );

    return new BitArray(buffer, end - start);
  }

  const size = end - start;

  const startIndex = Math.trunc(start / 8);
  const endIndex = Math.trunc((end - 1) / 8);

  // Handle the case of the slice being completely contained in a single byte
  if (startIndex === endIndex) {
    const highBitsCount = start % 8;

    // Shift the value to the high bits
    const byte = bitArray.rawBuffer[startIndex] << highBitsCount;

    return new BitArray(new Uint8Array([byte]), size);
  }

  // The bit slice is unaligned and spans multiple bytes, so accumulate it
  // into a new buffer

  const buffer = new Uint8Array(Math.trunc((size + 7) / 8));
  const highBitsCount = start % 8;
  const lowBitsCount = 8 - highBitsCount;

  let byteIndex = startIndex;

  for (let i = 0; i <= buffer.byteLength; i++, byteIndex++) {
    buffer[i] =
      (bitArray.rawBuffer[byteIndex] << highBitsCount) |
      (bitArray.rawBuffer[byteIndex + 1] >> lowBitsCount);
  }

  return new BitArray(buffer, size);
}

/**
 * @internal
 *
 * Interprets a slice of this bit array as a floating point number, either
 * 32-bit or 64-bit, with the specified endianness.
 *
 * The value of `end - start` must be exactly 32 or 64, otherwise an exception
 * will be thrown.
 *
 * @param {BitArray} bitArray
 * @param {number} start The start offset of the slice in bits.
 * @param {number} start The end offset of the slice in bits.
 * @param {boolean} isBigEndian Whether the slice is encoded in big endian.
 * @returns {number}
 */
export function bitArraySliceToFloat(bitArray, start, end, isBigEndian) {
  bitArrayValidateRange(bitArray, start, end);

  const bitSize = end - start;

  // Check size is valid
  if (bitSize !== 32 && bitSize !== 64) {
    const msg =
      `Sized floats must be 32-bit or 64-bit on JavaScript, ` +
      `got size of ${bitSize} bits`;
    throw new globalThis.Error(msg);
  }

  const isStartByteAligned = start % 8 === 0;

  // If the bit range is byte aligned then the float can be read directly out
  // of the existing buffer
  if (isStartByteAligned) {
    const view = new DataView(
      bitArray.rawBuffer.buffer,
      bitArray.rawBuffer.byteOffset,
    );

    if (bitSize === 64) {
      return view.getFloat64(start / 8, !isBigEndian);
    } else {
      return view.getFloat32(start / 8, !isBigEndian);
    }
  }

  // Copy the unaligned bits into a new bit array
  const alignedBits = bitArraySlice(bitArray, start, end);

  // Read the float out of the aligned buffer
  const view = new DataView(alignedBits.rawBuffer.buffer);
  if (bitSize === 64) {
    return view.getFloat64(0, !isBigEndian);
  } else {
    return view.getFloat32(0, !isBigEndian);
  }
}

/**
 * @internal
 *
 * Interprets a slice of this bit array as a signed or unsigned integer with
 * the specified endianness.
 *
 * @param {BitArray} bitArray
 * @param {number} start The start offset of the slice in bits.
 * @param {number} end The end offset of the slice in bits.
 * @param {boolean} isBigEndian Whether the slice is encoded in big endian.
 * @param {boolean} isSigned Whether to interpret the slice as signed two's
 * complement.
 * @returns {number}
 */
export function bitArraySliceToInt(
  bitArray,
  start,
  end,
  isBigEndian,
  isSigned,
) {
  bitArrayValidateRange(bitArray, start, end);

  if (start === end) {
    return 0;
  }

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
      bitArray,
      start,
      end,
      isBigEndian,
      isSigned,
    );
  } else {
    return intFromUnalignedSliceUsingBigInt(
      bitArray,
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
      if (isCursorByteAligned) {
        buffer.set(segment.rawBuffer, cursor / 8);
        cursor += segment.bitSize;

        // Zero any unused bits in the last byte of the buffer. Their content is
        // undefined and shouldn't be included in the output.
        const trailingBitsCount = segment.bitSize % 8;
        if (trailingBitsCount) {
          const lastByteIndex = Math.trunc(cursor / 8);
          buffer[lastByteIndex] >>= 8 - trailingBitsCount;
          buffer[lastByteIndex] <<= 8 - trailingBitsCount;
        }
      } else {
        appendUnalignedBits(segment.rawBuffer, segment.bitSize);
      }
    } else if (segment instanceof Uint8Array) {
      if (isCursorByteAligned) {
        buffer.set(segment, cursor / 8);
        cursor += segment.byteLength * 8;
      } else {
        appendUnalignedBits(segment, segment.byteLength * 8);
      }
    } else {
      if (isCursorByteAligned) {
        buffer[cursor / 8] = segment;
        cursor += 8;
      } else {
        appendUnalignedBits(new Uint8Array([segment]), 8);
      }
    }
  }

  function appendUnalignedBits(unalignedBits, size) {
    if (size <= 0) {
      return;
    }

    const highBitsCount = cursor % 8;
    const lowBitsCount = 8 - highBitsCount;

    let byteIndex = Math.trunc(cursor / 8);

    for (let byte of unalignedBits) {
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
 * @param {number} size Size in bits of the encoded float. Must be 32 or 64.
 * @param {boolean} isBigEndian Whether to encode as big/little endian.
 * @returns {Uint8Array}
 */
export function sizedFloat(value, size, isBigEndian) {
  if (size !== 32 && size !== 64) {
    const msg =
      `Sized floats must be 32-bit or 64-bit on JavaScript, ` +
      `got size of ${size} bits`;
    throw new globalThis.Error(msg);
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
 * @param {number} size Size of the encoded integer in bits.
 * @param {boolean} isBigEndian Whether to encode as big/little endian.
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
      bitArray,
      start,
      end,
      isBigEndian,
      isSigned,
    );
  } else {
    return intFromAlignedSliceUsingBigInt(
      bitArray,
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
 * @param {BitArray} bitArray
 * @param {number} start
 * @param {number} end
 * @param {boolean} isBigEndian
 * @param {boolean} isSigned
 * @returns {number}
 */
function intFromAlignedSliceUsingNumber(
  bitArray,
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
      value += bitArray.rawBuffer[i];
    }
  } else {
    for (let i = end - 1; i >= start; i--) {
      value *= 256;
      value += bitArray.rawBuffer[i];
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
 * @param {BitArray} bitArray
 * @param {number} start
 * @param {number} end
 * @param {boolean} isBigEndian
 * @param {boolean} isSigned
 * @returns {number}
 */
function intFromAlignedSliceUsingBigInt(
  bitArray,
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
      value += BigInt(bitArray.rawBuffer[i]);
    }
  } else {
    for (let i = end - 1; i >= start; i--) {
      value *= 256n;
      value += BigInt(bitArray.rawBuffer[i]);
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
 * @param {BitArray} bitArray
 * @param {number} start
 * @param {number} end
 * @param {boolean} isBigEndian
 * @param {boolean} isSigned
 * @returns {number}
 */
function intFromUnalignedSliceUsingNumber(
  bitArray,
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
      value = bitArray.rawBuffer[byteIndex++] & ((1 << leadingBitsCount) - 1);
      size -= leadingBitsCount;
    }

    // Read any whole bytes
    while (size >= 8) {
      value *= 256;
      value += bitArray.rawBuffer[byteIndex++];
      size -= 8;
    }

    // Read any trailing bits
    if (size > 0) {
      value *= 2 ** size;
      value += bitArray.rawBuffer[byteIndex] >> (8 - size);
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
        value += bitArray.rawBuffer[byteIndex++] * scale;
        scale *= 256;
        size -= 8;
      }

      // Read trailing bits
      value += (bitArray.rawBuffer[byteIndex] >> (8 - size)) * scale;
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
          (bitArray.rawBuffer[byteIndex] << highBitsCount) |
          (bitArray.rawBuffer[byteIndex + 1] >> lowBitsCount);

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
          (bitArray.rawBuffer[byteIndex] & ((1 << lowBitsCount) - 1)) >>
          (lowBitsCount - lowBitsUsed);

        size -= lowBitsUsed;

        if (size > 0) {
          trailingByte *= 2 ** size;
          trailingByte += bitArray.rawBuffer[byteIndex + 1] >> (8 - size);
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
 * @param {BitArray} bitArray
 * @param {number} start
 * @param {number} end
 * @param {boolean} isBigEndian
 * @param {boolean} isSigned
 * @returns {number}
 */
function intFromUnalignedSliceUsingBigInt(
  bitArray,
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
      value = BigInt(
        bitArray.rawBuffer[byteIndex++] & ((1 << leadingBitsCount) - 1),
      );
      size -= leadingBitsCount;
    }

    // Read any whole bytes
    while (size >= 8) {
      value *= 256n;
      value += BigInt(bitArray.rawBuffer[byteIndex++]);
      size -= 8;
    }

    // Read any trailing bits
    if (size > 0) {
      value <<= BigInt(size);
      value += BigInt(bitArray.rawBuffer[byteIndex] >> (8 - size));
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
        value += BigInt(bitArray.rawBuffer[byteIndex++]) << shift;
        shift += 8n;
        size -= 8;
      }

      // Read trailing bits
      value += BigInt(bitArray.rawBuffer[byteIndex] >> (8 - size)) << shift;
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
          (bitArray.rawBuffer[byteIndex] << highBitsCount) |
          (bitArray.rawBuffer[byteIndex + 1] >> lowBitsCount);

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
          (bitArray.rawBuffer[byteIndex] & ((1 << lowBitsCount) - 1)) >>
          (lowBitsCount - lowBitsUsed);

        size -= lowBitsUsed;

        if (size > 0) {
          trailingByte <<= size;
          trailingByte += bitArray.rawBuffer[byteIndex + 1] >> (8 - size);
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
