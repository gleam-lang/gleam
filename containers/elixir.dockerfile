FROM elixir:latest

ARG TARGETARCH
COPY gleam-${TARGETARCH} /bin/gleam

CMD ["gleam"]
