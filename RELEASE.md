# Release checklist

1. Update the version in `Cargo.toml`.
2. Update versions in `src/new.rs` for stdlib etc if required.
3. Run `make test build`.
4. Create and test a new project to verify it works.
5. Update CHANGELOG.md with new version and link to blog post (if present)
6. Git commit, tag, push, push tags.
7. Wait for CI release build to finish.
8. Publish release on GitHub from draft made by CI.
9. Update version in `Cargo.toml` to next-dev.
10. Update clone target version in `getting-started.md` in `website`.
11. Update downloaded version in `Dockerfile` in `example-echo-server`.
