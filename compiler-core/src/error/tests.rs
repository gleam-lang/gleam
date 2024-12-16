use super::*;
use insta::assert_snapshot;

#[test]
fn test_shell_program_not_found_error() {
    let cmds = vec![
        "erl", "erlc", "escript", "rebar3", "deno", "elixir", "node", "bun", "git",
    ];
    let oses = vec!["macos", "linux", "windows"];
    let distros = vec!["ubuntu", "unknown"];

    for cmd in &cmds {
        for os in &oses {
            match *os {
                "macos" | "windows" => {
                    let err = Error::ShellProgramNotFound {
                        program: cmd.to_string(),
                        os: os.to_string(),
                        distro: "unknown".to_string(),
                    }
                    .to_diagnostics();
                    assert_snapshot!(format!("{cmd}-{os}-unknown"), err[0].text);
                }
                "linux" => {
                    for distro in &distros {
                        let err = Error::ShellProgramNotFound {
                            program: cmd.to_string(),
                            os: os.to_string(),
                            distro: distro.to_string(),
                        }
                        .to_diagnostics();
                        assert_snapshot!(format!("{cmd}-{os}-{distro}"), err[0].text);
                    }
                }
                _ => (),
            }
        }
    }
}
