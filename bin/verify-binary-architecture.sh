#!/usr/bin/env bash
set -xeuo pipefail

if [ $# -ne 2 ]; then
  echo "Usage: $0 <target-triple> <binary-path>"
  exit 1
fi
TARGET_TRIPLE="$1"
BINARY_PATH="$2"
BINARY_FILE_TYPE=$(file -b "$BINARY_PATH")

# Architecture patterns
ARCHITECTURE_PATTERNS_FOR_AARCH64='aarch64|Aarch64|arm64'
ARCHITECTURE_PATTERNS_FOR_X86_64='x86-64|x86_64'

# Architecture helper functions
parse_architecture() {
  grep -Eo \
    -e "$ARCHITECTURE_PATTERNS_FOR_AARCH64" \
    -e "$ARCHITECTURE_PATTERNS_FOR_X86_64" \
    | head -n1
}
normalize_architecture() {
  sed -E \
    -e "s/($ARCHITECTURE_PATTERNS_FOR_AARCH64)/AArch64/" \
    -e "s/($ARCHITECTURE_PATTERNS_FOR_X86_64)/x86-64/"
}

# Parse and normalize architectures
TARGET_ARCHITECTURE=$(
  echo "$TARGET_TRIPLE" | parse_architecture | normalize_architecture \
  || echo "unknown target architecture"
)
BINARY_ARCHITECTURE=$(
  echo "$BINARY_FILE_TYPE" | parse_architecture | normalize_architecture \
  || echo "unknown binary architecture"
)

# Verify that binary architecture matches target architecture
if [ "$BINARY_ARCHITECTURE" != "$TARGET_ARCHITECTURE" ]; then
  echo "Architecture mismatch for '$TARGET_TRIPLE'!"
  echo "Expected: '$TARGET_ARCHITECTURE'"
  echo "Got: '$BINARY_ARCHITECTURE'"
  exit 1
fi
echo "Architecture match for '$TARGET_TRIPLE'!"
exit 0
