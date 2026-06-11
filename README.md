# ModMet: Modular Meteorology Functions

ModMet is a scientific Fortran library and toolkit for meteorological calculations used in atmospheric and environmental modeling workflows. Developed at RIVM, ModMet provides robust routines for radiation, turbulence, helper utilities, and numerical solvers in a modular codebase.

## Features

- Modular Fortran codebase with strict typing and explicit interfaces.
- Exposed through a single `modmet` module.
- Radiation routines, including cloud fraction and sun height calculations.
- Turbulence routines, including Obukhov length, stability functions, and flux-related calculations.
- Unit tests powered by [test-drive](https://github.com/fortran-lang/test-drive).
- Fully compatible with [fpm](https://github.com/fortran-lang/fpm) for building and testing.

## Getting Started

ModMet is intended for atmospheric scientists, modelers, and developers who need reusable meteorological routines in Fortran applications. The source is organized by domain:

- `src/radiation` for radiation calculations
- `src/turbulence` for turbulence and stability routines
- `src/solvers` for numerical solver utilities
- `src/helpers` for shared support helpers

The central public entry point is `src/modmet.f90`.

## Licensing

ModMet is licensed under the **European Union Public Licence (EUPL) v1.2**.

- Full license text: `LICENSE`
- Copyright holder: RIVM (Rijksinstituut voor Volksgezondheid en Milieu)

When redistributing or creating derivative works, follow the obligations in the EUPL v1.2 text included in this repository.

## Installation

### Prerequisites

Install the following tools:

1. A modern Fortran compiler (for example `gfortran`)
2. [fpm](https://github.com/fortran-lang/fpm)
3. Optional style checker: [fortitude](https://github.com/PlasmaFAIR/fortitude)
4. Optional coverage tools: `gcov`, `lcov` (`geninfo`, `genhtml`)

You can add `ModMet` as a dependency in your own `fpm.toml`:

```toml
[dependencies]
ModMet.git = "https://github.com/rivm-syso/ModMet.git"
```

### Build

```bash
fpm build --profile release
```

## Running and Testing

Run the unit test suite:

```bash
fpm test
```

Run tests with debug coverage flags:

```bash
fpm test --profile debug --flag --coverage --flag -O0
```

## Code Coverage

Generate a coverage report using the provided script:

```bash
./gen_coverage.sh
```

This script:

1. Runs tests with coverage flags
2. Collects coverage data with `gcov`
3. Builds LCOV info with `geninfo`
4. Generates HTML output with `genhtml`

Coverage output is written to:

- `coverage_html/coverage.info`
- `coverage_html/temp/index.html`

## Optional Code Quality Check

Install fortitude:

```bash
pip install fortitude-lint
```

Run style checks:

```bash
fortitude check
```

## Suggested Pre-Push Hook

To run tests and style checks before each push, add the following as `.git/hooks/pre-push`:

```bash
#!/bin/bash

echo "Running fpm test..."
fpm test
FPM_STATUS=$?

echo "Running fortitude check..."
fortitude check
FORTITUDE_STATUS=$?

if [ $FPM_STATUS -ne 0 ] || [ $FORTITUDE_STATUS -ne 0 ]; then
	echo "Pre-push hook failed: tests or style check did not pass."
	exit 1
fi
```

## Version

- 0.6.0 First public release with core features and tests. Based on the old `modmeteo.f90` used within RIVM's MetPro and DEPAC-1D internal packages. We will be releasing further improved and fully tested packages in the future.

## References

1. Beljaars, A.C.M., Holtslag, A.A.M., and Van Westrhenen, R.M. (1989). Description of a software library for the calculation of surface fluxes. KNMI Technical Report TR-112, De Bilt.
2. Hicks, B. B. "Wind profile relationships from the ‘Wangara’experiment." Quarterly Journal of the Royal Meteorological Society 102.433 (1976): 535-551.
3. Holtslag, Albert AM. "Estimates of diabatic wind speed profiles from near-surface weather observations." Boundary-Layer Meteorology 29.3 (1984): 225-250.
4. Holtslag, A. A. M., and H. A. R. De Bruin. "Applied modeling of the nighttime surface energy balance over land." Journal of Applied Meteorology and Climatology 27.6 (1988): 689-704.
5. Holstag, A. A. M., and A. P. Van Ulden. "A simple scheme for daytime estimates of the surface fluxes from routine weather data." Journal of Climate and applied Meteorology 22.4 (1983)
6. Monteith, John L., Mike H. Unsworth, and Ann Webb. "Principles of environmental physics." Quarterly Journal of the Royal Meteorological Society 120.520 (1994): 1699.
7. Van Ulden, Aad P., and Albert AM Holtslag. "Estimation of atmospheric boundary layer parameters for diffusion applications." Journal of Applied Meteorology and Climatology 24.11 (1985): 1196-1207.
8. Holtslag, A. A. M., and A. P. Van Ulden. De meteorologische aspecten van luchtverontreinigingsmodellen: eindrapport van het project klimatologie verspreidingsmodellen. KNMI, 1983.
