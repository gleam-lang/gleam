FROM node:latest

ARG TARGETARCH
COPY gleam-${TARGETARCH} /bin/gleam

CMD ["gleam"]
