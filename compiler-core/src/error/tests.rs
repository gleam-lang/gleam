use super::*;
use insta::assert_snapshot;

#[test]
fn test_shell_program_not_found_error() {
    let cmds = vec!["erlc", "rebar3", "deno", "elixir", "node", "bun", "git"];
    let oses = vec!["macos", "linux"];
    let distros = vec!["ubuntu", "other"];

    for cmd in &cmds {
        for os in &oses {
            let os_enum: OS = OS::from(*os);
            match os_enum {
                OS::MacOS | OS::Windows => {
                    let err = Error::ShellProgramNotFound {
                        program: cmd.to_string(),
                        os: os_enum,
                        distro: Distro::Other,
                    }
                    .to_diagnostics();
                    assert_snapshot!(
                        format!("shell_program_not_found_{cmd}_{os}_other"),
                        err[0].text
                    );
                }
                OS::Linux => {
                    for distro in &distros {
                        let distro_enum: Distro = Distro::from(*distro);
                        let err = Error::ShellProgramNotFound {
                            program: cmd.to_string(),
                            os: os_enum,
                            distro: distro_enum,
                        }
                        .to_diagnostics();
                        assert_snapshot!(
                            format!("shell_program_not_found_{cmd}_{os}_{distro}"),
                            err[0].text
                        );
                    }
                }
                OS::Other => (),
            }
        }
    }
}
