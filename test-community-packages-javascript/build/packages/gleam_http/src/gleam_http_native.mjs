import { Ok, Error } from "./gleam.mjs";
import {
  Get,
  Post,
  Head,
  Put,
  Delete,
  Trace,
  Connect,
  Options,
  Patch,
} from "./gleam/http.mjs";

export function decode_method(value) {
  try {
    switch (value.toLowerCase()) {
      case "get":
        return new Ok(new Get());
      case "post":
        return new Ok(new Post());
      case "head":
        return new Ok(new Head());
      case "put":
        return new Ok(new Put());
      case "delete":
        return new Ok(new Delete());
      case "trace":
        return new Ok(new Trace());
      case "connect":
        return new Ok(new Connect());
      case "options":
        return new Ok(new Options());
      case "patch":
        return new Ok(new Patch());
    }
  } catch {}
  return new Error(undefined);
}
