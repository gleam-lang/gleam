FROM ekidd/rust-musl-builder:1.34.2 as build

# Build
COPY . .
RUN cargo build --release

# Create user
FROM alpine:latest as alpine
RUN addgroup -S gleam-group && adduser -S gleam-user -G gleam-group

# Release
FROM scratch
COPY --from=build /home/rust/src/target/x86_64-unknown-linux-musl/release/gleam .
COPY --from=alpine /etc/passwd /etc/passwd
USER gleam-user
ENTRYPOINT ["./gleam"]
