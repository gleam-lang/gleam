use std::process::Command;

fn main() {
    // Try to get the current Git commit hash (short format) for use in --version --verbose
    let commit_hash = Command::new("git")
        .args(["rev-parse", "--short", "HEAD"])
        .output()
        .ok() // if the command fails to launch
        .filter(|output| output.status.success()) // if the command fails (e.g. not a git repo)
        .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_string())
        .filter(|s| !s.is_empty())
        .unwrap_or_else(|| "unknown".to_string());

    println!("cargo:rustc-env=GIT_COMMIT_HASH={}", commit_hash);
}
