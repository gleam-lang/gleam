FROM elixir:slim

ARG TARGETARCH
COPY gleam-${TARGETARCH} /bin/gleam

CMD ["gleam"]
