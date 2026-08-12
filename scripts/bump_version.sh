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

IFS='.' read -r major minor patch extra <<< "${normalized_current}"

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
EOF

echo "Bumped VERSION: ${normalized_current} -> ${new_version}"
echo "Updated RELEASE_DATE: ${release_date}"