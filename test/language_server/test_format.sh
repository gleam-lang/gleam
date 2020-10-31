#!/bin/bash

cat > /tmp/gleam_sample.gleam <<EOF
pub fn add(a, b) { a+b }

pub fn main() {
  1 |> add()
}
EOF

exec 3>&1
sendMessages() {
    cat "messages/initialize.txt";
    cat "messages/initialized.txt";
    cat "messages/format.txt";
    cat "messages/shutdown.txt";
    sleep 1;
    cat "messages/exit.txt";
}

../../target/debug/gleam language-server < <(sendMessages) >&3
