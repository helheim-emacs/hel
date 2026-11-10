#!/usr/bin/env bash
#
# Show the changes in the Helix text object queries since the commit in
# HELIX-REVISION.
#
# The files <language>/textobjects.scm derive from the files
# runtime/queries/<language>/textobjects.scm of Helix. This script clones
# Helix without the file contents of the commits it does not need, and
# prints the log with the diffs of those files from HELIX-REVISION to
# GIT-REF. A person ports the changes by hand and then writes the new
# commit to HELIX-REVISION. The script prints that commit last.
#
# It stops with an error when a file at GIT-REF carries the "Incompatible
# With Secondary Licenses" mark of MPL-2.0.
#
# Usage: queries/helix-changes.sh [GIT-REF]
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
base="$(cat "$dest/HELIX-REVISION")"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

git clone --quiet --filter=blob:none --no-checkout --branch "$REF" \
    "$HELIX_URL" "$tmp/helix"

files=()
for lang in "${LANGUAGES[@]}"; do
    files+=("runtime/queries/$lang/textobjects.scm")
done

for file in "${files[@]}"; do
    if git -C "$tmp/helix" cat-file -e "HEAD:$file" 2>/dev/null &&
       git -C "$tmp/helix" show "HEAD:$file" |
           grep -q "Incompatible With Secondary Licenses"; then
        echo "error: $file is marked" \
             "\"Incompatible With Secondary Licenses\"" >&2
        exit 1
    fi
done

git -C "$tmp/helix" --no-pager log --patch --reverse \
    "$base..HEAD" -- "${files[@]}"

echo
echo "Helix $REF is $(git -C "$tmp/helix" rev-parse HEAD)"
