#!/bin/bash

# Run tests in debug mode with coverage instrumentation enabled.

fpm test --profile debug --flag --coverage --flag -O0

# Generate raw gcov output files under coverage_html/.
mkdir -p coverage_html && (   cd coverage_html &&   gcov ../build/gfortran_*/ModMet/src*.gcda -r ../src/ -b; )

# Collect coverage data into a single LCOV tracefile.
geninfo ./build/gfortran_*/ModMet/src*.gcda -b . -o ./coverage_html/coverage.info

# Render HTML coverage report for local inspection.
genhtml ./coverage_html/coverage.info -o ./coverage_html/temp