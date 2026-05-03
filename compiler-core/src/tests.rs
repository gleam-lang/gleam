use std::path::PathBuf;

use itertools::Itertools;
use serde::Deserialize;

#[test]
fn licence_files_exist_in_repo() {
    #[derive(Deserialize)]
    struct CargoDenoConfig {
        licenses: CargoDenoLicensesConfig,
    }

    #[derive(Deserialize)]
    struct CargoDenoLicensesConfig {
        allow: Vec<String>,
    }

    // deny.toml specified which licences dependencies are permitted to have.
    let toml = std::fs::read_to_string("../deny.toml").unwrap();
    let CargoDenoConfig {
        licenses: CargoDenoLicensesConfig {
            allow: dependency_licences,
        },
    } = toml::from_str(&toml).unwrap();

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
