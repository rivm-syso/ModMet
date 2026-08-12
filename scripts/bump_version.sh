#!/usr/bin/env bash

set -euo pipefail

VERSION_FILE="VERSION"
VERSION_MODULE_FILE="src/modmet_version.f90"

if [[ ! -f "${VERSION_FILE}" ]]; then
  echo "ERROR: ${VERSION_FILE} not found" >&2
  exit 1
fi

if [[ ! -f "${VERSION_MODULE_FILE}" ]]; then
  echo "ERROR: ${VERSION_MODULE_FILE} not found" >&2
  exit 1
fi

current_version="$(tr -d '[:space:]' < "${VERSION_FILE}")"
if [[ -z "${current_version}" ]]; then
  current_version="0.0.0"
fi

normalized_current="${current_version#v}"

is_semver() {
  [[ "$1" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]
}

latest_tag_version="$(git tag --list | sed 's/^v//' | grep -E '^[0-9]+\.[0-9]+\.[0-9]+$' | sort -V | tail -n 1 || true)"

main_version=""
if git rev-parse --verify origin/main >/dev/null 2>&1; then
  main_version="$(git show origin/main:VERSION 2>/dev/null | tr -d '[:space:]' | sed 's/^v//' || true)"
fi

version_candidates="${normalized_current}"
if is_semver "${latest_tag_version:-0.0.0}"; then
  version_candidates+=$'\n'"${latest_tag_version}"
fi
if is_semver "${main_version:-0.0.0}"; then
  version_candidates+=$'\n'"${main_version}"
fi

baseline_version="$(printf '%s\n' "${version_candidates}" | grep -E '^[0-9]+\.[0-9]+\.[0-9]+$' | sort -V | tail -n 1)"

IFS='.' read -r major minor patch extra <<< "${baseline_version}"

if [[ -n "${extra:-}" ]] || [[ -z "${major:-}" ]] || [[ -z "${minor:-}" ]] || [[ -z "${patch:-}" ]]; then
  echo "ERROR: VERSION must use semantic format MAJOR.MINOR.PATCH (optionally prefixed with v)" >&2
  exit 1
fi

if ! [[ "${major}" =~ ^[0-9]+$ && "${minor}" =~ ^[0-9]+$ && "${patch}" =~ ^[0-9]+$ ]]; then
  echo "ERROR: VERSION components must be numeric" >&2
  exit 1
fi

msg="$(git log -1 --pretty=%B)"

if [[ "${msg}" == *"[major]"* ]]; then
  major=$((major + 1))
  minor=0
  patch=0
elif [[ "${msg}" == *"[minor]"* ]]; then
  minor=$((minor + 1))
  patch=0
else
  patch=$((patch + 1))
fi

new_version="${major}.${minor}.${patch}"
release_date="$(date -u +%Y-%m-%d)"

printf '%s\n' "${new_version}" > "${VERSION_FILE}"

sed -i -E "s/(character\(len=\*\), parameter, public :: VERSION = \").*(\")/\1${new_version}\2/" "${VERSION_MODULE_FILE}"
sed -i -E "s/(character\(len=\*\), parameter, public :: RELEASE_DATE = \").*(\")/\1${release_date}\2/" "${VERSION_MODULE_FILE}"

cat > version.env <<EOF
OLD_VERSION=${normalized_current}
NEW_VERSION=${new_version}
RELEASE_DATE=${release_date}
BASELINE_VERSION=${baseline_version}
EOF

echo "Bumped VERSION: ${normalized_current} -> ${new_version} (baseline: ${baseline_version})"
echo "Updated RELEASE_DATE: ${release_date}"