# Release checklist

1. Update the version in each `Cargo.toml`.
2. Update versions in `src/new.rs` for stdlib etc if required.
3. Run `make test build`.
4. Git commit, tag, push, push tags.
5. Wait for CI release build to finish.
6. Publish release on GitHub from draft made by CI.
7. Update version in `Cargo.toml` to next-dev.
8. Update clone target version in `getting-started.md` in `website`.
