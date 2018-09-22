module Timeout

import Foreign

pub external type Timeout

pub fn hibernate() {
  Foreign.unsafeCoerce('hibernate')
}

pub fn infinity() {
  Foreign.unsafeCoerce('infinity')
}

pub fn milliseconds(ms) {
  Foreign.unsafeCoerce(ms)
}
