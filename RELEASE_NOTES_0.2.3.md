# actuarialfitdist 0.2.3 — documentation and CRAN-release preparation

This revision is based on the 0.2.2 numerical-fix package. Changes are limited to documentation and release metadata; fitting, factor and plotting computations are identical to 0.2.2.

The help pages now describe the actual returned columns and list fields, distinguish model-observation policy limits from payment caps, and explain deterministic versus simulated graphics. The README includes installation instructions, and DESCRIPTION cites the fifth edition of *Loss Models*.

**Not CRAN-certified yet:** this environment has no R installation. The maintainer must run `devtools::document()`, inspect the help-page diff, run the tests and a complete R CMD check (`--as-cran` including PDF manual) and test on development R before submission.
