FROM rust:1.33

LABEL "com.github.actions.name"="Rust"
LABEL "com.github.actions.description"="Build Rust"
LABEL "com.github.actions.icon"="code"
LABEL "com.github.actions.color"="orange"

LABEL "repository"="http://github.com/lpil/gleam"
LABEL "maintainer"="Louis Pilfold <louis@lpil.uk>"

ADD ./gleam/src/Cargo.toml ./gleam/src/Cargo.lock ./gleam/

RUN mkdir -p .gleam/src \
  && echo 'fn main() { println!("Dummy") }' > ./gleam/src/main.rs \
  && cargo test

ENTRYPOINT ["/bin/sh", "-c"]
