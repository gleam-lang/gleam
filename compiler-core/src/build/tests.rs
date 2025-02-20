use crate::{Error, manifest::ManifestPackage};

use super::project_compiler::{BuildTool, usable_build_tools};

#[test]
fn usable_build_tool_unknown() {
    assert_eq!(
        usable_build_tools(&ManifestPackage::default().with_build_tools(&["unknown"])),
        Err(Error::UnsupportedBuildTool {
            package: "".into(),
            build_tools: vec!["unknown".into()],
        })
    )
}

#[test]
fn usable_build_tool_none() {
    assert_eq!(
        usable_build_tools(&ManifestPackage::default()),
        Err(Error::UnsupportedBuildTool {
            package: "".into(),
            build_tools: vec![],
        })
    )
}

#[test]
fn usable_build_tool_only_mix() {
    assert_eq!(
        usable_build_tools(&ManifestPackage::default().with_build_tools(&["mix"])),
        Ok(vec![BuildTool::Mix])
    )
}

#[test]
fn usable_build_tool_only_rebar3() {
    assert_eq!(
        usable_build_tools(&ManifestPackage::default().with_build_tools(&["rebar3"])),
        Ok(vec![BuildTool::Rebar3])
    )
}

#[test]
fn usable_build_tool_only_gleam() {
    assert_eq!(
        usable_build_tools(&ManifestPackage::default().with_build_tools(&["gleam"])),
        Ok(vec![BuildTool::Gleam])
    )
}

#[test]
fn usable_build_tool_mix_then_rebar3() {
    assert_eq!(
        usable_build_tools(&ManifestPackage::default().with_build_tools(&["mix", "rebar3"])),
        Ok(vec![BuildTool::Mix, BuildTool::Rebar3])
    )
}
