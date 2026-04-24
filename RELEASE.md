# Release checklist

1. Write release post on website.
2. Update documentation on website (language server, gleam.toml, etc).
3. Update the version in each `Cargo.toml`.
4. Run `make test build`.
5. Git commit, tag, push, push tags.
6. Wait for CI release build to finish.
7. Publish release on GitHub from draft made by CI.
8. Share website release post.
