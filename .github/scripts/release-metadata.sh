#!/usr/bin/env bash
# Derive the pending release, if any, from build.sbt and CHANGELOG.md.
#
# The version declared in build.sbt is the single source of truth. A release is
# pending when no git tag v<version> exists yet. The CHANGELOG.md section for
# that version supplies the release title and notes.
#
# Usage:
#   .github/scripts/release-metadata.sh [--notes-file PATH] [--local-tags]
#
#   --notes-file PATH  where to write the CHANGELOG section body when a release
#                      is pending (default: release-notes.md in the current dir)
#   --local-tags       look for the tag among local tags instead of asking the
#                      origin remote; for offline use
#
# Prints key=value lines, and appends them to $GITHUB_OUTPUT when that is set:
#   version=0.1.0        version from build.sbt
#   tag=v0.1.0           tag that the release will create
#   release=true|false   true when that tag does not exist yet
#   title=...            release title: text after the version and date in the
#                        CHANGELOG heading, or the tag when there is none
#
# Exits non-zero when the two files disagree, so that the same script can act
# as a pull request check:
#   - build.sbt has no single version of the form MAJOR.MINOR.PATCH
#   - a release is pending but CHANGELOG.md has no non-empty section for it
set -euo pipefail

notes_file="release-notes.md"
local_tags=false
while [ $# -gt 0 ]; do
  case "$1" in
    --notes-file) notes_file="$2"; shift 2 ;;
    --local-tags) local_tags=true; shift ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done

fail() { echo "release-metadata: $*" >&2; exit 1; }

# --- version from build.sbt -------------------------------------------------
version=$(sed -nE 's/^ThisBuild \/ version := "([^"]+)".*/\1/p' build.sbt)
[ "$(printf '%s\n' "$version" | grep -c .)" -eq 1 ] \
  || fail "expected exactly one 'ThisBuild / version := \"...\"' line in build.sbt"
printf '%s' "$version" | grep -Eq '^[0-9]+\.[0-9]+\.[0-9]+$' \
  || fail "version '$version' in build.sbt is not MAJOR.MINOR.PATCH (no 'v' prefix)"
tag="v$version"

# --- does the tag exist already? --------------------------------------------
if $local_tags; then
  if [ -n "$(git tag -l "$tag")" ]; then release=false; else release=true; fi
else
  set +e
  git ls-remote --exit-code --tags origin "refs/tags/$tag" >/dev/null 2>&1
  rc=$?
  set -e
  case $rc in
    0) release=false ;;
    2) release=true ;;
    *) fail "could not query tags on origin (git ls-remote exit $rc)" ;;
  esac
fi

# --- CHANGELOG section ------------------------------------------------------
title="$tag"
if $release; then
  [ -f CHANGELOG.md ] || fail "CHANGELOG.md is missing"
  # The heading is matched as a literal prefix, not a regular expression: awk
  # implementations disagree on backslash escapes in -v values, so a regex with
  # \[ or \. passed that way breaks on some of them (POSIX awk, busybox) while
  # working on others (mawk). A heading matches when it is exactly the prefix or
  # continues with a space, so "## [0.1.0]" does not match "## [0.1.01]".
  heading_prefix="## [$version]"
  heading=$(awk -v prefix="$heading_prefix" '
    index($0, prefix) == 1 && (length($0) == length(prefix) || substr($0, length(prefix) + 1, 1) == " ")
  ' CHANGELOG.md | head -n 1)
  [ -n "$heading" ] || fail "CHANGELOG.md has no '## [$version]' section for the pending release"
  # Body: lines after the heading up to the next '## ' heading, trimmed of
  # leading and trailing blank lines.
  awk -v prefix="$heading_prefix" '
    found && /^## / { exit }
    found { lines[++n] = $0 }
    !found && index($0, prefix) == 1 && (length($0) == length(prefix) || substr($0, length(prefix) + 1, 1) == " ") { found = 1 }
    END {
      while (n > 0 && lines[n] ~ /^[[:space:]]*$/) n--
      first = 1
      while (first <= n && lines[first] ~ /^[[:space:]]*$/) first++
      for (i = first; i <= n; i++) print lines[i]
    }
  ' CHANGELOG.md > "$notes_file"
  [ -s "$notes_file" ] || fail "the '## [$version]' section in CHANGELOG.md is empty"
  heading_title=$(printf '%s' "$heading" \
    | sed -E 's/^## \[[^]]+\]( - [0-9]{4}-[0-9]{2}-[0-9]{2})?( - )?//')
  [ -n "$heading_title" ] && title="$heading_title"
fi

# --- outputs ----------------------------------------------------------------
emit() {
  printf '%s=%s\n' "$1" "$2"
  [ -n "${GITHUB_OUTPUT:-}" ] && printf '%s=%s\n' "$1" "$2" >> "$GITHUB_OUTPUT"
  return 0
}
emit version "$version"
emit tag "$tag"
emit release "$release"
emit title "$title"
