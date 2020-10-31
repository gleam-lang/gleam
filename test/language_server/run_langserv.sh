#!/bin/bash

cat > /tmp/gleam_sample.gleam <<EOF
pub fn add(a, b) { a+b }

pub fn main() {
  1 |> add()
}
EOF

content='{
    "jsonrpc": "2.0",
    "id": 2,
    "method": "textDocument/formatting",
    "params": {
        "textDocument": {
            "uri": "file:///tmp/gleam_sample.gleam"
        },
        "options": {
            "tabSize": 4,
            "insertSpaces": true
        }
    }
}'

message="Content-Length: ${#content}"$'\r\n\r\n'"$content"

exec 3>&1
(cat "messages/initialize.txt" && cat "messages/initialized.txt" && cat && cat "messages/shutdown.txt" && cat "messages/exit.txt") | (../../target/debug/gleam language-server 1>/tmp/gleam-language-server.log && echo "" && cat /tmp/gleam-language-server.log && rm /tmp/gleam-language-server.log)
