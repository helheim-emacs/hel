#!/usr/bin/env bash
#
# Update the copies of the Helix text object queries in this directory.
#
# The script makes a shallow, sparse clone of the Helix repository and copies
# runtime/queries/<language>/textobjects.scm for every language in LANGUAGES.
# Then it copies the Helix license and writes the Helix commit to
# HELIX-REVISION. It stops with an error when a file is missing or carries
# the "Incompatible With Secondary Licenses" mark. It copies the files as
# they are. It copies only files named textobjects.scm, so it never touches
# the hel-textobjects.scm files, which belong to Hel.
#
# Usage: queries/update.sh [GIT-REF]
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -euo pipefail

HELIX_URL="https://github.com/helix-editor/helix.git"
REF="${1:-master}"

# The languages of the tree-sitter major modes that are built into Emacs,
# followed by the files that they inherit from.
LANGUAGES=(
    bash c cpp c-sharp cmake css dockerfile elixir go heex html java
    javascript json lua php python ruby rust toml tsx typescript yaml
    ecma _typescript _jsx
)

dest="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

git clone --quiet --depth 1 --filter=blob:none --sparse --branch "$REF" \
    "$HELIX_URL" "$tmp/helix"
git -C "$tmp/helix" sparse-checkout set runtime/queries

# Check every file before copying any, so that a failed run changes nothing.
for lang in "${LANGUAGES[@]}"; do
    src="$tmp/helix/runtime/queries/$lang/textobjects.scm"
    if [[ ! -f "$src" ]]; then
        echo "error: Helix has no file runtime/queries/$lang/textobjects.scm" >&2
        exit 1
    fi
    if grep -q "Incompatible With Secondary Licenses" "$src"; then
        echo "error: $lang/textobjects.scm is marked" \
             "\"Incompatible With Secondary Licenses\"" >&2
        exit 1
    fi
done

for lang in "${LANGUAGES[@]}"; do
    mkdir -p "$dest/$lang"
    cp "$tmp/helix/runtime/queries/$lang/textobjects.scm" \
       "$dest/$lang/textobjects.scm"
done

cp "$tmp/helix/LICENSE" "$dest/LICENSE"
git -C "$tmp/helix" rev-parse HEAD > "$dest/HELIX-REVISION"

echo "Copied ${#LANGUAGES[@]} files from Helix $(cat "$dest/HELIX-REVISION")"
