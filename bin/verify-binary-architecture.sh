#!/usr/bin/env bash
set -xeuo pipefail

if [ $# -ne 2 ]; then
   echo "Usage: $0 <target-triple> <binary-path>"
   exit 1
fi
TARGET_TRIPLE="$1"
BINARY_PATH="$2"

# Parse target architecture
case "${TARGET_TRIPLE}" in
    "x86_64"*) TARGET_ARCHITECTURE="x86_64" ;;
    "aarch64"*) TARGET_ARCHITECTURE="aarch64" ;;
    *) echo "Unknown target architecture '${TARGET_TRIPLE}'"; exit 1 ;;
esac

# Parse binary architecture and map expected binary architecture for target OS
case "${TARGET_TRIPLE}" in
    *"darwin"*)
        # Parse binary architecture
        file_output=$(file -b "${BINARY_PATH}")
        BINARY_ARCHITECTURE=$(echo "${file_output}" | grep -o "x86_64\|arm64" || echo "")
        # Map expected binary architecture
        case "${TARGET_ARCHITECTURE}" in
            "x86_64") EXPECTED_BINARY_ARCHITECTURE="x86_64" ;;
            "aarch64") EXPECTED_BINARY_ARCHITECTURE="arm64" ;;
            *) echo "Unknown Darwin architecture: '${TARGET_ARCHITECTURE}'"; exit 1 ;;
        esac
        ;;
    *"linux"*)
        # Parse binary architecture
        file_output=$(file -b "${BINARY_PATH}")
        BINARY_ARCHITECTURE=$(echo "${file_output}" | grep -o "x86-64\|aarch64" | head -n1 || echo "")
        # Map expected binary architecture
        case "${TARGET_ARCHITECTURE}" in
            "x86_64") EXPECTED_BINARY_ARCHITECTURE="x86-64" ;;
            "aarch64") EXPECTED_BINARY_ARCHITECTURE="aarch64" ;;
            *) echo "Unknown Linux architecture: '${TARGET_ARCHITECTURE}'"; exit 1 ;;
        esac
        ;;
    *"windows"*)
        # Parse binary architecture
        pe_header_output=$(powershell -Command "
          \$bytes = [System.IO.File]::ReadAllBytes('${BINARY_PATH}');
          \$header_offset = [System.BitConverter]::ToInt32(\$bytes, 0x3c);
          \$machine_type = [System.BitConverter]::ToUInt16(\$bytes, \$header_offset + 4);
          \$machine_type
        " 2>&1) || echo "PE header extraction failed"
        # Map binary architecture
        case "${pe_header_output}" in
            *"34404"*) BINARY_ARCHITECTURE="X64" ;;   # 0x8664
            *"43620"*) BINARY_ARCHITECTURE="Arm64" ;; # 0xAA64
            *) echo "Unknown PE machine type: '${pe_header_output}'"; exit 1 ;;
        esac
        # Map expected binary architecture
        case "${TARGET_ARCHITECTURE}" in
            "x86_64") EXPECTED_BINARY_ARCHITECTURE="X64" ;;
            "aarch64") EXPECTED_BINARY_ARCHITECTURE="Arm64" ;;
            *) echo "Unknown Windows architecture: '${TARGET_ARCHITECTURE}'"; exit 1 ;;
        esac
        ;;
    *)
        echo "Unknown target OS: '${TARGET_TRIPLE}'"
        exit 1
        ;;
esac

# Verify binary architecture
if [[ "${BINARY_ARCHITECTURE}" != "${EXPECTED_BINARY_ARCHITECTURE}" ]]; then
    echo "Architecture mismatch for '${TARGET_TRIPLE}'"
    echo "Expected: '${EXPECTED_BINARY_ARCHITECTURE}'"
    echo "Got: '${BINARY_ARCHITECTURE}'"
    exit 1
fi
