#!/bin/bash

# Generate platform-specific C flags for libsql OCaml bindings

# Detect OS and architecture
OS=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)

# Normalize architecture names
case $ARCH in
    "x86_64"|"amd64") ARCH="amd64" ;;
    "arm64"|"aarch64") ARCH="arm64" ;;
esac

# Determine library directory
LIB_DIR="${OS}_${ARCH}"
LIB_PATH="$(pwd)/${LIB_DIR}"

# Check if library directory exists
if [ ! -d "$LIB_PATH" ]; then
    echo "Warning: Library directory $LIB_PATH not found" >&2
    echo "()" > c_flags.sexp
    exit 1
fi

# Base flags
FLAGS=()
FLAGS+=("-L$LIB_PATH")
FLAGS+=("-lsql_experimental")
FLAGS+=("-lsqlite3")

# OS-specific flags
case $OS in
    "darwin")
        FLAGS+=("-framework" "Security")
        FLAGS+=("-framework" "CoreFoundation")
        ;;
    "linux")
        FLAGS+=("-lm" "-ldl" "-lpthread")
        ;;
esac

# Generate sexp format
printf "(" > c_flags.sexp
for flag in "${FLAGS[@]}"; do
    printf '"%s" ' "$flag" >> c_flags.sexp
done
printf ")\n" >> c_flags.sexp

echo "Generated c_flags.sexp for $OS $ARCH"
