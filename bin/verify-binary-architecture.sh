#!/usr/bin/env bash
set -xeuo pipefail

if [ $# -ne 2 ]; then
  echo "Usage: $0 <target-triple> <binary-path>"
  exit 1
fi
TARGET_TRIPLE="$1"
BINARY_PATH="$2"

# Architecture patterns
X86_64_PATTERNS='x86-64|x86_64'
AARCH64_PATTERNS='arm64|aarch64|Aarch64'

# Architecture helper functions
parse() { grep -Eo "$X86_64_PATTERNS|$AARCH64_PATTERNS" | head -n1; }
normalize() { sed -E "s/($X86_64_PATTERNS)/x86-64/;s/($AARCH64_PATTERNS)/AArch64/"; }
unknown_architecture_for() { echo "unknown $1 architecture"; }

# Parse and normalize target architecture
parse_and_normalize_target_architecture() {
  local target_triple="$1"
  get() { echo "$target_triple"; }
  get | parse | normalize || unknown_architecture_for "target"
}
TARGET_ARCHITECTURE=$(parse_and_normalize_target_architecture "$TARGET_TRIPLE")

# Parse and normalize binary architecture
parse_and_normalize_binary_architecture() {
  local binary_path="$1"
  debug_pipe() { tee >(cat >&2); }
  get() { file -b "$binary_path" | debug_pipe; }
  get | parse | normalize || unknown_architecture_for "binary"
}
BINARY_ARCHITECTURE=$(parse_and_normalize_binary_architecture "$BINARY_PATH")

# Verify that binary architecture matches target architecture
if [ "$BINARY_ARCHITECTURE" != "$TARGET_ARCHITECTURE" ]; then
  echo "Architecture mismatch for '$TARGET_TRIPLE'!"
  echo "Expected: '$TARGET_ARCHITECTURE'"
  echo "Got: '$BINARY_ARCHITECTURE'"
  exit 1
fi
echo "Architecture match for '$TARGET_TRIPLE'!"
exit 0
