module Timeout
  exposing Timeout, hibernate/0, infinity/0, milliseconds/1

import Foreign

external type Timeout

fn hibernate() {
  Foreign.unsafeCoerce(:hibernate)
}

fn infinity() {
  Foreign.unsafeCoerce(:infinity)
}

fn milliseconds(ms) {
  Foreign.unsafeCoerce(ms)
}
