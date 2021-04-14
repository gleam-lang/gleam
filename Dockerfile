FROM rust:1.51.0 

COPY . /usr/src/gleam-src

RUN set -xe \
        && cargo install watchexec \
        && cd /usr/src/gleam-src \
        && make install \
        && rm -rf /usr/src/gleam-src

CMD []
# FROM node:15.14.0

# COPY --from=build /usr/local/cargo/bin/gleam /bin
# RUN gleam --version

# WORKDIR /opt/app
# CMD ["bash"]
# watchexec -i tmp/gen -- gleam compile-package --src tmp --out tmp/gen --name tessa --target javascript