FROM node:slim

ARG TARGETARCH
COPY gleam-${TARGETARCH} /bin/gleam

CMD ["gleam"]
