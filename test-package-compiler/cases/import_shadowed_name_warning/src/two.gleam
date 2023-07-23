//// https://github.com/gleam-lang/otp/pull/22

// No warning should be emitted about this imported type. The compiler does not
// confuse it for the value constructor defined below.
import one.{Port}

type Shadowing {
  // This value constructor has the same name as the imported type.
  Port
}

// Here the type is used.
@external(erlang, "wibble", "wobble")
pub fn use_type(port: Port) -> Nil
