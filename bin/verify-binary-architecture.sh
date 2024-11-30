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
X86_64_PATTERNS='x86-64|x86_64'
AARCH64_PATTERNS='arm64|aarch64|Aarch64'

# Architecture helper functions
parse() { grep -Eo "$X86_64_PATTERNS|$AARCH64_PATTERNS" | head -n1; }
normalize() { sed -E "s/($X86_64_PATTERNS)/x86-64/;s/($AARCH64_PATTERNS)/AArch64/"; }

# Parse and normalize architectures
TARGET_ARCHITECTURE=$(
  echo "$TARGET_TRIPLE" | parse | normalize \
  || echo "unknown target architecture"
)
BINARY_ARCHITECTURE=$(
  echo "$BINARY_FILE_TYPE" | parse | normalize \
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
