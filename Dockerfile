FROM rust:1.51.0 AS build

COPY . /usr/src/gleam-src

RUN set -xe \
        && cd /usr/src/gleam-src \
        && make install \
        && rm -rf /usr/src/gleam-src

FROM node:15.14.0

COPY --from=build /usr/local/cargo/bin/gleam /bin
RUN gleam --version

WORKDIR /opt/app
CMD ["bash"]