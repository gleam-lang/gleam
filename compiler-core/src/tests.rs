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

{licences_missing_licence_file:?}"
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
    let regex = regex::Regex::new(r"(?m)^FIX-DEADLINE:\s*(\d{4})-(\d{2})-(\d{2})$").unwrap();

    for ignored in load_cargo_deny_config().advisories.ignore {
        let id = ignored.id;
        let Some(deadline) = regex.captures(&ignored.reason).and_then(|caps| {
            let year = caps[1].parse::<i32>().ok()?;
            let month = caps[2].parse::<u8>().ok()?;
            let day = caps[3].parse::<u8>().ok()?;
            time::Date::from_calendar_date(year, month.try_into().ok()?, day).ok()
        }) else {
            panic!(
                "
deny.toml advisory missing deadline!
Add a line to the `reason` property for {id} with this format:

    FIX-DEADLINE: 2026-01-05
"
            )
        };

        assert!(
            today <= deadline,
            "
The deadline for fixing advisory {id} has passed.
Fix it now, or bump the deadline in deny.toml.
"
        )
    }
}
