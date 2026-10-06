# CRAN readiness notes

The package was assembled with CRAN-oriented structure, documentation, tests, a vignette, and CI configuration.

## Completed static checks

- All NAMESPACE exports have matching R function definitions.
- All registered S3 methods have matching definitions.
- All exported functions have an Rd alias.
- Internal `.afd_*` references resolve to definitions in `R/`.
- Package source, documentation, tests, examples, and vignette contain ASCII-only text.
- The ground-up-loss assumption is repeated in README, package help, fitter help, vignette, and validation errors.
- Optional real-world example dependencies are in Suggests rather than Imports.
- ggplot2 is the only non-base runtime dependency.

## Required before CRAN submission

This build environment did not contain R, so a live package build/check could not be run here. On a machine with R installed, run:

```r
devtools::document()
devtools::test()
devtools::check()
```

Then from a terminal run the CRAN-style source check:

```text
R CMD build actuarialfitdist
R CMD check --as-cran actuarialfitdist_0.1.0.tar.gz
```

Review any roxygen-generated NAMESPACE/Rd changes rather than accepting them blindly, because the checked-in documentation and namespace were generated explicitly for this package draft.

The included GitHub Actions workflow runs R CMD check on Windows, macOS, Linux release, Linux devel, and Linux oldrel-1.

## Documentation regeneration

This source tree uses roxygen comments as the single source of truth for help files.
Before checking a copied-over development tree, delete any pre-existing `man/`
directory left from an older build, then run `devtools::document()`. This prevents
stale hand-written Rd files from coexisting with newly generated per-function Rd
files and producing duplicate-name/duplicate-alias warnings.
