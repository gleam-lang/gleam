# Gleam public threat model

Last updated: 2026-04-23

For vulnerability reporting please see the [GitHub Security Advisories
page](https://github.com/gleam-lang/gleam/security/advisories).

## In scope

- The Gleam compiler, build tool, language server, formatter, Hex package
  manager client, and Wasm interface, all located in the `gleam-lang/gleam`
  GitHub repository.
- The precompiled binaries for Gleam published to GitHub releases for the
  `gleam-lang/gleam` GitHub repository.
- All Gleam packages within the `gleam-lang` GitHub organisation and their Hex
  publications.
- Gleam's GitHub Actions CI workflows and GitHub code repositories.
- The Gleam websites:
  - <https://gleam.run> (hosted with GitHub pages)
  - <https://tour.gleam.run> (hosted with GitHub pages)
  - <https://playground.gleam.run> (hosted with GitHub pages)
  - <https://packages.gleam.run> (hosted with a Vultr VPS)

## Out of scope

- Security of third-party Gleam packages published to the Hex package manager.
- Security of Hex package management system server-side components.
- Securing of runtimes used by Gleam code, such as Erlang's BEAM and
  JavaScript's V8.
- Security of third-party operating system packages and installers for Gleam.

## Assumptions

- Third party providers such as GitHub, and Vultr meet their security
  obligations and are not compromised.

## Threats

### GitHub account phishing or hijacking

A bad actor could attempt to gain access to a GitHub account.

- MFA is enforced for GitHub accounts.

### Malicious or compromised Gleam GitHub organisation member account

A bad actor with a `gleam-lang` member account could attempt to interfere with
code repositories, actions workflows, and the content of issues, pull requests,
and discussions.

- Pull requests and reviews are required for making changes to code.
- Regular members do not have permissions to perform destructive actions on GitHub.
- Vandalised content within issues, pull requests, and discussions can be
  restored by GitHub support.
- Gleam's GitHub Actions workflows can be re-run without any side effects
  beyond using workflow minutes.

### Hex account phishing or hijacking

A bad actor could attempt to gain access to a Hex account.

- MFA is enforced for Hex accounts.

### Malicious or compromised Hex account

A bad actor with access to a Hex account could attempt to publish malicious
versions of Gleam packages.

- Hex requires MFA for destructive actions such as package publication.
- Hex is immutable and existing package versions cannot be modified after
  publication.
- Hex provides tooling for viewing diffs between package versions to aid with
  auditing new code.
- Hex's security team will remove malicious package releases.
- Gleam's Hex client always uses version locking to prevent differing versions
  of dependencies from being added unintentionally.

### Vultr account phishing or hijacking

A bad actor could attempt to gain access to a Vultr account.

- MFA is required for Vultr.

### Vultr VPS infiltration attacks

A bad actor could attempt to gain access to the Vultr VPS.

- SSH authentication uses SSH keys and password authentication is disabled.
- The VPS has a restrictive firewall enabled.
- The VPS has automatic OS security updates enabled.
- Minimal software is installed on the VPS and all workloads run inside
  containers.

### Compromised Vultr VPS

A bad actor with root access to the VPS that serves the packages site could
steal data from it or vandalise the website.

- No secrets are stored on the VPS.
- All configuration for the VPS is stored on GitHub so the VPS can be destroyed
  and replaced quickly and easily.
- The packages website data is public, so there is no threat of theft of
  private data.
- The packages website data can be recomputed from Hex and is backed-up
  externally, so there is no threat of permanent data loss.

### Malicious or compromised Rust dependency

A bad actor with access to publish new versions of our Rust dependencies could
attempt to inject malicious code into the `gleam` binary.

- We use Rust's dependency version locking to avoid unexpected versions of
  dependencies from being included in our application.
- A cooldown period is used with dependabot to avoid including new dependency
  versions that may not have been security scanned yet.
- Dependency versions are not upgraded shortly before or during the RC process,
  to minimise risk of new versions with vulnerabilities making it into a release.
- The `cargo-deny` tool is used to alert when a dependency version used has a
  published CVE.
- GitHub code scanning and CodeQL is enabled to alert to further vulnerabilities.

### Malicious or compromised Hex dependency

A bad actor with access to publish new versions of our Hex dependencies could
attempt to inject malicious code into the subtree of one of our Hex packages.

- The Gleam core packages do not have any third-party dependencies.

### Compromise or tampering of precompiled release binaries

A bad actor could attempt to cause a malicious or modified binary to be
published to GitHub Releases, or replace release assets after build.

- The release workflow uses GitHub OIDC and artifact attestations to produce
  provenance for release assets, published alongside release archives as
  `.sigstore` files.
- Checksums are provided for release archives for users' integrity checking.
- The GitHub Actions workflow configuration files are scanned for
  vulnerabilities using GitHub CodeQL.
- GitHub Action versions are pinned using SHAs, and use of tools such as `gh`
  is used instead of third-party Actions where possible.
