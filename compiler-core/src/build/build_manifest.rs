use serde::{Deserialize, Serialize};

use std::collections::HashMap;
use std::collections::HashSet;

use crate::io::FileSystemIO;
use std::path::PathBuf;

#[derive(Deserialize, Serialize, std::fmt::Debug)]
pub struct BuildManifestData {
    pub modules: HashMap<String, ModuleManifest>,
}

#[derive(Deserialize, Serialize, std::fmt::Debug)]
pub struct ModuleManifest {
    pub source_hash: String,
    pub name: String,
    pub path: String,
    pub local_deps: Vec<String>,
}

#[derive(std::fmt::Debug)]
pub struct BuildManifest {
    pub data: BuildManifestData,
}

impl BuildManifest {
    pub fn new() -> BuildManifest {
        BuildManifest {
            data: BuildManifestData {
                modules: HashMap::new(),
            },
        }
    }

    pub fn try_decode(&mut self, data: &str) {
        let data = toml::from_str(&data).map_err(|x| ());

        if let Ok(data) = data {
            self.data = data;
        }
    }

    pub fn insert_modules(&mut self, modules: &Vec<crate::build::Module>) {
        let mut local: std::collections::HashSet<String> = std::collections::HashSet::new();
        for m in modules {
            let _ = local.insert(m.name.clone());
        }

        for module in modules {
            let path = module.input_path.to_str().expect("pathbuf").to_string();
            let local_deps: Vec<String> = module
                .deps
                .clone()
                .iter()
                .filter(|x| local.contains(x.clone()))
                .map(|x| x.to_string())
                .collect();

            let manifest = ModuleManifest {
                name: module.name.to_string(),
                path: path.to_string(),
                local_deps: local_deps,
                source_hash: module.source_hash.to_string(),
            };
            let _old = self.data.modules.insert(path, manifest);
        }
    }

    pub fn serialize(&self) -> String {
        toml::to_string(&self.data).expect("serialize build manifest")
    }

    pub fn clear(&mut self) {
        self.data.modules = HashMap::new();
    }

    pub fn check_sources(&mut self, sources: &Vec<crate::build::Source>) -> HashSet<String> {
        let mut same_source: HashMap<String, String> = HashMap::new();
        let mut valid: HashSet<String> = HashSet::new();

        for source in sources {
            let existing = self.data.modules.get(source.path.to_str().expect("to_str"));
            if let Some(existing) = existing {
                if existing.source_hash == crate::build::get_source_hash(&source.code) {
                    let _ = same_source.insert(existing.name.clone(), existing.path.clone());
                }
            } else {
            }
        }

        for source in same_source.iter() {
            let existing = self.data.modules.get(source.1);
            if let Some(existing) = existing {
                let is_valid = existing
                    .local_deps
                    .iter()
                    .all(|x| same_source.get(x).is_some());
                if is_valid {
                    let _ = valid.insert(existing.name.clone());
                }
            }
        }

        self.data.modules.retain(|n, m| valid.contains(&m.name));

        valid
    }
}
