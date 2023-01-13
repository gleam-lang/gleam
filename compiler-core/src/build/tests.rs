use crate::{manifest::ManifestPackage, Error};

use super::project_compiler::{usable_build_tool, BuildTool};

#[test]
fn usable_build_tool_unknown() {
    assert_eq!(
        usable_build_tool(&ManifestPackage::default().with_build_tools(&["unknown"])),
        Err(Error::UnsupportedBuildTool {
            package: "".into(),
            build_tools: vec!["unknown".into()],
        })
    )
}

#[test]
fn usable_build_tool_none() {
    assert_eq!(
        usable_build_tool(&ManifestPackage::default()),
        Err(Error::UnsupportedBuildTool {
            package: "".into(),
            build_tools: vec![],
        })
    )
}

#[test]
fn usable_build_tool_only_mix() {
    assert_eq!(
        usable_build_tool(&ManifestPackage::default().with_build_tools(&["mix"])),
        Ok(BuildTool::Mix)
    )
}

#[test]
fn usable_build_tool_only_rebar3() {
    assert_eq!(
        usable_build_tool(&ManifestPackage::default().with_build_tools(&["rebar3"])),
        Ok(BuildTool::Rebar3)
    )
}

#[test]
fn usable_build_tool_only_gleam() {
    assert_eq!(
        usable_build_tool(&ManifestPackage::default().with_build_tools(&["gleam"])),
        Ok(BuildTool::Gleam)
    )
}

#[test]
fn usable_build_tool_mix_then_rebar3() {
    // We default to rebar3 if it is available, even if mix is also available.
    assert_eq!(
        usable_build_tool(&ManifestPackage::default().with_build_tools(&["mix", "rebar3"])),
        Ok(BuildTool::Rebar3)
    )
}
