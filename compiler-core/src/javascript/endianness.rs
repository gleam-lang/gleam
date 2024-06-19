#[derive(Debug, PartialEq)]
pub enum Endianness {
    Big,
    Little,
}

impl Endianness {
    pub fn is_big(&self) -> bool {
        *self == Endianness::Big
    }
}
