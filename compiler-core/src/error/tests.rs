use super::*;
use insta::assert_snapshot;

#[test]
fn test_shell_program_not_found_error() {
    let cmds = vec!["erlc", "rebar3", "deno", "elixir", "node", "bun", "git"];
    let oses = vec!["macos", "linux"];
    let distros = vec!["ubuntu", "other"];

    for cmd in &cmds {
        for os in &oses {
            if os != &"linux" {
                let err = Error::ShellProgramNotFound {
                    program: cmd.to_string(),
                    os: parse_os(os, "other"),
                }
                .to_diagnostics();
                assert_snapshot!(
                    format!("shell_program_not_found_{cmd}_{os}_other"),
                    err[0].text
                );
            } else {
                for distro in &distros {
                    let err = Error::ShellProgramNotFound {
                        program: cmd.to_string(),
                        os: parse_os(os, distro),
                    }
                    .to_diagnostics();
                    assert_snapshot!(
                        format!("shell_program_not_found_{cmd}_{os}_{distro}"),
                        err[0].text
                    );
                }
            }
        }
    }
}
