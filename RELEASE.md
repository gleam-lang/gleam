# Release checklist

1. Update the version in `Cargo.toml`.
2. Update versions in `src/new.rs` for stdlib etc if required.
3. Run `make test build`.
4. Create and test a new project to verify it works.
5. Update CHANGELOG.md with new version and link to blog post (if present)
6. Git commit, tag, push, push tags.
7. Create a new release on GitHub.
8. Update version in `Cargo.toml` to next-dev.
