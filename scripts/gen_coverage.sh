#!/bin/bash

# Run tests in debug mode with coverage instrumentation enabled.

fpm test --profile debug --flag --coverage --flag -O0;

rm -rf coverage_html;

mkdir -p coverage_html;

gcov build/gfortran_*/ModMet/src*.gcda -b;

mv -f *.f90.gcov coverage_html/;

# Collect coverage data into a single LCOV tracefile.
geninfo ./build/gfortran_*/ModMet/src*.gcda -b . -o ./coverage_html/coverage.info

# Render HTML coverage report for local inspection.
genhtml ./coverage_html/coverage.info -o ./coverage_html/temp