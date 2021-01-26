use crate::GleamExpect;
use std::convert::TryInto;

pub fn to_u32<X: TryInto<u32>>(x: X) -> u32 {
    x.try_into()
        .map_err(|_| ())
        .gleam_expect("x was larger than u32::MAX")
}

pub fn to_u16<X: TryInto<u16>>(x: X) -> u16 {
    x.try_into()
        .map_err(|_| ())
        .gleam_expect("x was larger than u16::MAX")
}

pub fn to_u8<X: TryInto<u8>>(x: X) -> u8 {
    x.try_into()
        .map_err(|_| ())
        .gleam_expect("x was larger than u8::MAX")
}

pub fn to_isize<X: TryInto<isize>>(x: X) -> isize {
    x.try_into()
        .map_err(|_| ())
        .gleam_expect("x was larger than isize::MAX")
}

pub fn to_usize<X: TryInto<usize>>(x: X) -> usize {
    x.try_into()
        .map_err(|_| ())
        .gleam_expect("x was larger than usize::MAX")
}
