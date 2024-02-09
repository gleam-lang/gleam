import * as TOML from "./priv/node_modules/@ltd/j-toml/index.mjs";

import {
    Error,
    Ok,
} from "./gleam.mjs";

export function parse_toml(string) {
    try {
        return new Ok(TOML.parse(string));
    } catch (e) {
        return new Error(e.message)
    }
}
