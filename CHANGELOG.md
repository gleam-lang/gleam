# Changelog

## Unreleased

- `Version` and `Requirement` structs have been added for representing
  information about requests versions of packages.
- Protobuf code updated to use `cfg_attr` rather than deprecated attributes.
- The `get_repository_versions` method returns `Version`s rather than strings.
- The `get_package` uses the `version::Range` type to represent `requirement`s.

## v1.3.0 - 2020-03-30

- The `get_package_tarball` method has been added.

## v1.2.0 - 2020-01-17

- The `get_package` method has been added.
- Update tokio to 1.0
- Update reqwest to 0.11
- Update url to 2.2
- Update bytes to 1.0
- The generated protobuf code has been regenerated.

## v1.1.1 - 2020-07-20

- Updated generated protobuffer deserialisation code.

## v1.1.0 - 2020-07-20

- The `get_repository_versions` method has been added.
- The `repository_base` field has been added to both authenticated and
  unauthenticated clients.

## v1.0.0 - 2020-05-03

- Initial release
