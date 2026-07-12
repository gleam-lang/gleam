// SPDX-License-Identifier: Apache-2.0
// SPDX-FileCopyrightText: 2026 The Gleam contributors

#[cfg(test)]
mod tests;

use num_bigint::{BigInt, Sign};
use num_traits::ToPrimitive;

#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

#[must_use]
#[derive(Debug)]
pub struct ListEnder {
    /// This is the index in the term builder buffer to where the number of
    /// elements in the list is written. When we start a list the number is
    /// zeroed as we may not know the number of elements yet, and it is
    /// written when finishing the list using this index.
    size_index: usize,

    /// This field is used by the debug mode `Drop` implementation, to ensure
    /// that lists are correctly closed after being consumed.
    #[cfg(debug_assertions)]
    used: bool,
}

impl ListEnder {
    pub fn new(size_index: usize) -> Self {
        Self {
            size_index,

            #[cfg(debug_assertions)]
            used: false,
        }
    }
}

impl ListEnder {
    #[cfg(debug_assertions)]
    pub fn consume(mut self) {
        self.used = true;
    }

    #[cfg(not(debug_assertions))]
    pub fn consume(self) {}
}

/// When testing we want to make sure that all lists are always correctly closed
/// after being consumed. In production builds this is not implemented, so there
/// is no runtime cost.
#[cfg(debug_assertions)]
impl Drop for ListEnder {
    fn drop(&mut self) {
        assert!(self.used, "list not closed");
    }
}

/// The first byte of ETF data is this version number.
///
const ERLANG_TERM_FORMAT_VERSION_NUMBER: u8 = 131;

/// A data structure used to encode values into the Erlang Term Format:
/// https://www.erlang.org/doc/apps/erts/erl_ext_dist.html.
///
#[derive(Debug)]
pub struct TermBuilder {
    bytes: Vec<u8>,
}

impl Default for TermBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl TermBuilder {
    pub fn new() -> Self {
        Self {
            bytes: vec![ERLANG_TERM_FORMAT_VERSION_NUMBER],
        }
    }

    /// Get the binary representation of the data structure built so far.
    pub fn into_vec(self) -> Vec<u8> {
        self.bytes
    }

    fn push(&mut self, byte: u8) {
        self.bytes.push(byte);
    }

    fn extend(&mut self, bytes: impl IntoIterator<Item = u8>) {
        self.bytes.extend(bytes);
    }

    /// Pushes a single raw byte.
    ///
    pub fn raw_byte(&mut self, byte: u8) {
        self.push(byte);
    }

    /// Start building a list with a number of item that is not known in
    /// advance.
    ///
    /// Once you've then pushed all the items, you _must_ complete the list by
    /// calling `end_list` with the number of items that were pushed.
    ///
    /// ```ignore
    /// // [1, 2, 3]
    /// let list = etf.start_list()
    /// etf.small_integer(1);
    /// etf.small_integer(2);
    /// etf.small_integer(3);
    /// etf.end_list(list, 3)
    /// ```
    ///
    /// https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#list_ext
    pub fn start_list(&mut self) -> ListEnder {
        self.push(108);
        let size_index = self.bytes.len();

        // These 4 bytes store the number of elements (not including the tail).
        // They are set to zero here, and the `end_list` function overwrites them
        // with the actual length, as that number may not be known in advance.
        self.push(0);
        self.push(0);
        self.push(0);
        self.push(0);

        ListEnder::new(size_index)
    }

    pub fn end_list(&mut self, list: ListEnder, number_of_element: u32) {
        self.empty_list();
        self.bytes[list.size_index..list.size_index + 4]
            .copy_from_slice(&number_of_element.to_be_bytes());
        list.consume();
    }

    /// Pushes the most compact representation of the given atom.
    pub fn atom(&mut self, atom: &str) {
        // TODO: we could explore using ATOM_CACHE_REF in future.
        // Might be able to get slightly faster ETF parsing out of erlc if we
        // use it:
        // https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#atom_cache_ref
        if atom.len() <= 255 {
            self.small_atom_utf8(atom);
        } else {
            self.atom_utf8(atom);
        }
    }

    /// Pushes the most compact representation of the given big int.
    pub fn bigint(&mut self, value: BigInt) {
        if let Some(value) = value.to_u8() {
            self.small_integer(value);
        } else if let Some(value) = value.to_i32() {
            self.integer(value);
        } else {
            self.small_big(value);
        }
    }

    /// Pushes the most compact representation of the given usize int.
    ///
    pub fn usize(&mut self, value: usize) {
        if let Some(value) = value.to_u8() {
            self.small_integer(value);
        } else {
            self.integer(value as i32);
        }
    }

    /// https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#small_integer_ext
    fn small_integer(&mut self, value: u8) {
        self.push(97);
        self.push(value);
    }

    /// https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#integer_ext
    fn integer(&mut self, value: i32) {
        self.push(98);
        self.extend(value.to_be_bytes());
    }

    /// https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#new_float_ext
    pub fn new_float(&mut self, value: f64) {
        self.push(70);
        self.extend(value.to_be_bytes());
    }

    /// https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#small_tuple_ext
    pub fn small_tuple(&mut self, arity: u8) {
        self.push(104);
        self.push(arity);
    }

    /// https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#large_tuple_ext
    pub fn large_tuple(&mut self, arity: u32) {
        self.push(105);
        self.extend(arity.to_be_bytes());
    }

    /// https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#nil_ext
    pub fn empty_list(&mut self) {
        self.push(106);
    }

    /// https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#binary_ext
    pub fn binary(&mut self, bytes_count: u32, bytes: impl IntoIterator<Item = u8>) {
        self.push(109);
        self.extend(bytes_count.to_be_bytes());
        self.extend(bytes);
    }

    /// https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#small_big_ext
    fn small_big(&mut self, number: BigInt) {
        let (sign, bytes) = number.to_bytes_le();
        self.push(110);
        self.push(bytes.len() as u8);
        match sign {
            Sign::NoSign | Sign::Plus => self.push(0),
            Sign::Minus => self.push(1),
        }
        self.extend(bytes);
    }

    /// https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#large_big_ext
    pub fn large_big(&mut self, number: BigInt) {
        let (sign, bytes) = number.to_bytes_le();
        self.push(111);
        self.extend((bytes.len() as u32).to_be_bytes());
        match sign {
            Sign::NoSign | Sign::Plus => self.push(0),
            Sign::Minus => self.push(1),
        }
        self.extend(bytes);
    }

    /// https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#small_atom_utf8_ext
    fn small_atom_utf8(&mut self, name: &str) {
        self.push(119);
        self.push(name.len() as u8);
        self.extend(name.bytes());
    }

    /// https://www.erlang.org/doc/apps/erts/erl_ext_dist.html#atom_utf8_ext
    fn atom_utf8(&mut self, name: &str) {
        self.push(118);
        self.extend((name.len() as u16).to_be_bytes());
        self.extend(name.bytes());
    }
}
