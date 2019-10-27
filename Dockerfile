FROM ekidd/rust-musl-builder:1.37.0 as build

# Build
COPY . .
RUN sudo chown -R rust:rust /home/rust && cargo build --release

# Create user
FROM alpine:latest as alpine
RUN addgroup -S gleam-group && adduser -S gleam-user -G gleam-group

# Release
FROM scratch
COPY --from=build /home/rust/src/target/x86_64-unknown-linux-musl/release/gleam .
COPY --from=alpine /etc/passwd /etc/passwd
USER gleam-user
ENTRYPOINT ["./gleam"]
