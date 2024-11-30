#!/usr/bin/env bash
set -xeuo pipefail

if [ $# -ne 2 ]; then
  echo "Usage: $0 <target-triple> <binary-path>"
  exit 1
fi
TARGET_TRIPLE="$1"
BINARY_PATH="$2"

# Parse target architecture
parse_target_architecture() {
  local target_triple="$1"
  local parse="x86_64|aarch64"
  local error="unknown target architecture"
  echo "$target_triple" | grep -Eo "$parse" || echo "$error"
}
TARGET_ARCHITECTURE=$(parse_target_architecture "$TARGET_TRIPLE")

# Parse and normalize binary architecture
parse_and_normalize_binary_architecture() {
  local binary_path="$1"
  local parse="x86_64|x86-64|arm64|aarch64|Aarch64"
  local normalize='s/x86-64/x86_64/;s/(arm64|Aarch64)/aarch64/'
  local error="unknown binary architecture"
  file -b "$binary_path" \
  | grep -Eo "$parse" | head -n1 \
  | sed -E "$normalize" \
  || echo "$error"
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
