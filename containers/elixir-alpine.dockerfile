FROM elixir:alpine

ARG TARGETARCH
COPY gleam-${TARGETARCH} /bin/gleam

CMD ["gleam"]
