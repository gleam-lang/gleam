use std::path::PathBuf;

use itertools::Itertools;
use serde::Deserialize;

#[derive(Deserialize)]
struct CargoDenyConfig {
    advisories: CargoDenyAdvisoriesConfig,
    licenses: CargoDenyLicensesConfig,
}

#[derive(Deserialize)]
struct CargoDenyLicensesConfig {
    allow: Vec<String>,
}

#[derive(Deserialize)]
struct CargoDenyAdvisoriesConfig {
    ignore: Vec<CargoDenyIgnoreConfig>,
}

#[derive(Deserialize)]
struct CargoDenyIgnoreConfig {
    id: String,
    reason: String,
}

#[test]
fn licence_files_exist_in_repo() {
    // deny.toml specified which licences dependencies are permitted to have.
    let dependency_licences = load_cargo_deny_config().licenses.allow;

    // Each licence must exist in the licences directory.
    let licences_directory = PathBuf::from("../licences");
    let licences_missing_licence_file = dependency_licences
        .iter()
        .filter(|licence| {
            let path = licences_directory.join(format!("{licence}.txt"));
            !std::fs::exists(path).unwrap()
        })
        .collect_vec();

    assert!(
        licences_missing_licence_file.is_empty(),
        "
All dependency licences must exist in the licences directory.
These licences are missing their file:

- {licences_missing_licence_file:?}"
    );
}

fn load_cargo_deny_config() -> CargoDenyConfig {
    let toml = std::fs::read_to_string("../deny.toml").unwrap();
    let config = toml::from_str(&toml).unwrap();
    config
}

#[test]
fn rust_advisories_ignore_deadlines() {
    let today = time::OffsetDateTime::now_utc().date();
    let regex = regex::Regex::new(r"(?m)FIX-DEADLINE: (\d{4})-(\d{2})-(\d{2})").unwrap();

    for ignored in load_cargo_deny_config().advisories.ignore {
        let id = ignored.id;
        let reason = ignored.reason;
        let Some(captures) = regex.captures(&reason) else {
            panic!(
                "
deny.toml advisory missing deadline!
Add a line to the `reason` property for {id} with this format:

    FIX-DEADLINE: 2026-01-05

{reason:?}
"
            )
        };

        let year = captures[1].parse::<i32>().expect("parse year");
        let month = captures[2].parse::<u8>().expect("parse month int");
        let month = month.try_into().expect("parse month");
        let day = captures[3].parse::<u8>().expect("parse day");
        let deadline = time::Date::from_calendar_date(year, month, day).expect("construct date");

        assert!(
            today <= deadline,
            "
The deadline for fixing advisory {id} has passed.
Fix it now, or bump the deadline in deny.toml.
"
        )
    }
}

#[test]
fn all_files_have_copyright_notice() {
    let paths = ignore::WalkBuilder::new("..")
        .follow_links(false)
        .git_global(true)
        .git_ignore(true)
        .hidden(true)
        .parents(true)
        .build()
        .map(|entry| entry.expect("directory traversal succeeds"))
        .filter(|entry| {
            entry
                .file_type()
                .map(|type_| type_.is_file())
                .unwrap_or(false)
        })
        .map(ignore::DirEntry::into_path)
        .filter_map(|path| {
            let file_name = path.file_name().and_then(|s| s.to_str())?;
            let extension = path.extension()?;

            match extension.to_str().expect("file extension") {
                // Test files.
                "new" => return None,
                "snap" => return None,
                "txt" if path.starts_with("../test/") || path.starts_with("../licences/") => {
                    return None;
                }

                // Static assets
                "gz" => return None,
                "tar" => return None,
                "exs" => return None,
                "dockerfile" => return None,
                "woff2" => return None,
                "png" => return None,
                "css" if file_name.ends_with(".min.css") => return None,
                "js" if file_name.ends_with(".min.js") => return None,

                // Template files to be added to user projects
                "ps1" if file_name == "erlang-shipment-entrypoint.ps1" => return None,
                "sh" if file_name == "erlang-shipment-entrypoint.sh" => return None,

                // Generated files
                "toml" if file_name == "manifest.toml" => return None,

                // Source files
                "config" if file_name == "rebar.config" => (),
                "css" => (),
                "proto" => (),
                "ts" => (),
                "erl" => (),
                "ex" => (),
                "gleam" => (),
                "hrl" => (),
                "js" => (),
                "md" => (),
                "mjs" => (),
                "mts" => (),
                "ps1" => (),
                "rs" => (),
                "sh" => (),
                "toml" => (),

                // HTML templates
                "html" if path.starts_with("../compiler-core/templates/") => (),

                _ => panic!("Unexpected file extension for {path:?}"),
            };

            let text = std::fs::read_to_string(&path).expect("read file");
            if text.contains("SPDX-License-Identifier: Apache-2.0")
                && text.contains("SPDX-FileCopyrightText: 20")
            {
                // Licence information is present
                return None;
            }

            Some(path.to_str().unwrap().to_string())
        })
        .join("\n - ");

    if !paths.is_empty() {
        panic!(
            "Source files missing SPDX licence information!

Add a comment like this:

    // SPDX-License-Identifier: Apache-2.0
    // SPDX-FileCopyrightText: 2016 The Gleam contributors

To these files:

{paths}
"
        )
    }
}
