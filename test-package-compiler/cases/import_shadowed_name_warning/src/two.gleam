// This should emit no warnings, the imported type is used, the compiler is not
// confused by the local constructor with the same name.
//
// https://github.com/gleam-lang/otp/pull/22
//
import one.{Port}

type Shadowing {
  // This value constructor has the same name as the imported type.
  Port
}

// Here the type is used.
pub external fn use_type(Port) -> Nil =
  "wibble" "wobble"
