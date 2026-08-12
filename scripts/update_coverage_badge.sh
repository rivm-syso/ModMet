#!/usr/bin/env bash

set -euo pipefail

README_FILE="README.md"
COVERAGE_INFO_FILE="coverage_html/coverage.info"
VERSION_MODULE_FILE="src/modmet_version.f90"
ENV_OUTPUT_FILE="${COVERAGE_ENV_FILE:-/tmp/modmet_coverage.env}"

if [[ ! -f "${README_FILE}" ]]; then
  echo "ERROR: ${README_FILE} not found" >&2
  exit 1
fi

if [[ ! -f "${COVERAGE_INFO_FILE}" ]]; then
  echo "ERROR: ${COVERAGE_INFO_FILE} not found" >&2
  exit 1
fi

if [[ ! -f "${VERSION_MODULE_FILE}" ]]; then
  echo "ERROR: ${VERSION_MODULE_FILE} not found" >&2
  exit 1
fi

coverage_value="$(awk -F: '
  /^LF:/ { lf += $2 }
  /^LH:/ { lh += $2 }
  END {
    if (lf > 0) {
      printf "%.2f", (100.0 * lh / lf)
    } else {
      printf "0.00"
    }
  }
' "${COVERAGE_INFO_FILE}")"

if [[ "${coverage_value}" == "100.00" ]]; then
  badge_color="green"
elif [[ "${coverage_value}" == "0.00" ]]; then
  badge_color="red"
else
  badge_color="orange"
fi

badge_url="https://img.shields.io/badge/coverage-${coverage_value}%25-${badge_color}"
badge_line="![Coverage](${badge_url})"

release_date=""
if [[ -f "version.env" ]]; then
  # version.env is created by bump_version.sh during CI runs.
  source version.env
  release_date="${RELEASE_DATE:-}"
fi

if [[ -z "${release_date}" ]]; then
  release_date="$(sed -n 's/.*RELEASE_DATE = "\([0-9-]\+\)".*/\1/p' "${VERSION_MODULE_FILE}" | head -n 1)"
fi

if [[ -z "${release_date}" ]]; then
  echo "ERROR: could not determine RELEASE_DATE" >&2
  exit 1
fi

release_date_badge="${release_date//-/--}"
release_badge_url="https://img.shields.io/badge/release-${release_date_badge}-blue"
release_badge_line="![Release Date](${release_badge_url})"

if head -n 1 "${README_FILE}" | grep -q '^!\[Coverage\](https://img.shields.io/badge/coverage-'; then
  sed -i "1s|.*|${badge_line}|" "${README_FILE}"
else
  sed -i "1s|^|${badge_line}\\n|" "${README_FILE}"
fi

if grep -q '^!\[Release Date\](https://img.shields.io/badge/release-' "${README_FILE}"; then
  sed -i "s|^!\[Release Date\](https://img.shields.io/badge/release-.*)|${release_badge_line}|" "${README_FILE}"
else
  echo "${release_badge_line}" >> "${README_FILE}"
fi

echo "Computed coverage: ${coverage_value}%"
echo "Badge color: ${badge_color}"
echo "Release date: ${release_date}"
echo "COVERAGE_PERCENT=${coverage_value}" > "${ENV_OUTPUT_FILE}"
echo "COVERAGE_COLOR=${badge_color}" >> "${ENV_OUTPUT_FILE}"
echo "RELEASE_DATE=${release_date}" >> "${ENV_OUTPUT_FILE}"
echo "Coverage env file: ${ENV_OUTPUT_FILE}"