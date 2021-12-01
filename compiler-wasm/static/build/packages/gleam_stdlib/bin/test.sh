set -eu

OUT=gen/javascript
rm -fr $OUT
gleam compile-package --name gleam_stdlib --target javascript --src src --test test --out $OUT
cp src/*.js $OUT/
node bin/run-tests.js
