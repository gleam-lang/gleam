# Release checklist

1. Update the version in `Cargo.toml`.
2. Update versions in `src/new.rs` for stdlib etc if required.
3. Run `make test build`.
4. Update CHANGELOG.md with new version and link to blog post (if present)
5. Git commit, tag, push, push tags.
6. Wait for CI release build to finish.
7. Publish release on GitHub from draft made by CI.
8. Update version in `Cargo.toml` to next-dev.
9. Update clone target version in `getting-started.md` in `website`.
10. Update downloaded version in `Dockerfile` in `example-echo-server`.
