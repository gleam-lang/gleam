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

#[test]
fn io_link_file_error() {
    let error = Error::FileIo {
        kind: FileKind::File,
        action: FileIoAction::Link("/dest".into()),
        path: "/src".into(),
        err: Some("Critical error!".to_owned()),
    }
    .pretty_string();
    assert_snapshot!(error);
}

#[test]
fn io_copy_directory_error() {
    let error = Error::FileIo {
        kind: FileKind::Directory,
        action: FileIoAction::Copy("/dest".into()),
        path: "/src".into(),
        err: Some("Critical error!".to_owned()),
    }
    .pretty_string();
    assert_snapshot!(error);
}

#[test]
fn io_delete_file_error() {
    let error = Error::FileIo {
        kind: FileKind::File,
        action: FileIoAction::Delete,
        path: "/file".into(),
        err: Some("Critical error!".to_owned()),
    }
    .pretty_string();
    assert_snapshot!(error);
}

#[test]
fn io_open_file_error() {
    let error = Error::FileIo {
        kind: FileKind::File,
        action: FileIoAction::Open,
        path: "/file".into(),
        err: Some("Critical error!".to_owned()),
    }
    .pretty_string();
    assert_snapshot!(error);
}

#[test]
fn io_parse_file_error() {
    let error = Error::FileIo {
        kind: FileKind::File,
        action: FileIoAction::Parse,
        path: "/file".into(),
        err: Some("Critical error!".to_owned()),
    }
    .pretty_string();
    assert_snapshot!(error);
}

#[test]
fn io_read_file_error() {
    let error = Error::FileIo {
        kind: FileKind::File,
        action: FileIoAction::Read,
        path: "/file".into(),
        err: Some("Critical error!".to_owned()),
    }
    .pretty_string();
    assert_snapshot!(error);
}

#[test]
fn io_create_directory_error() {
    let error = Error::FileIo {
        kind: FileKind::Directory,
        action: FileIoAction::Create,
        path: "/dir".into(),
        err: Some("Critical error!".to_owned()),
    }
    .pretty_string();
    assert_snapshot!(error);
}

#[test]
fn io_write_to_file_error() {
    let error = Error::FileIo {
        kind: FileKind::File,
        action: FileIoAction::WriteTo,
        path: "/file".into(),
        err: Some("Critical error!".to_owned()),
    }
    .pretty_string();
    assert_snapshot!(error);
}

#[test]
fn io_find_parent_of_directory_error() {
    let error = Error::FileIo {
        kind: FileKind::Directory,
        action: FileIoAction::FindParent,
        path: "/dir".into(),
        err: Some("Critical error!".to_owned()),
    }
    .pretty_string();
    assert_snapshot!(error);
}

#[test]
fn io_canonicalise_file_error() {
    let error = Error::FileIo {
        kind: FileKind::File,
        action: FileIoAction::Canonicalise,
        path: "/file".into(),
        err: Some("Critical error!".to_owned()),
    }
    .pretty_string();
    assert_snapshot!(error);
}

#[test]
fn io_update_file_permissions_error() {
    let error = Error::FileIo {
        kind: FileKind::File,
        action: FileIoAction::UpdatePermissions,
        path: "/file".into(),
        err: Some("Critical error!".to_owned()),
    }
    .pretty_string();
    assert_snapshot!(error);
}

#[test]
fn io_read_metadata_of_file_error() {
    let error = Error::FileIo {
        kind: FileKind::File,
        action: FileIoAction::ReadMetadata,
        path: "/file".into(),
        err: Some("Critical error!".to_owned()),
    }
    .pretty_string();
    assert_snapshot!(error);
}
