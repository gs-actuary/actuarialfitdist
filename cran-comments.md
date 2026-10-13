## Release status

Version 0.2.2 is a **development candidate, not submitted to CRAN**.

The latest user-run `R CMD check --as-cran` of 0.2.1 produced 0 errors, 1 warning, 1 note before its documentation/static-check corrections. The numerical revisions in 0.2.2 have NOT yet been executed through an R test suite or an R CMD check in the producing environment. Run these before any CRAN submission:

```r
devtools::document()
devtools::test()
devtools::check(document = FALSE, args = "--as-cran")
```

See `CRAN_READINESS.md` for remaining validation and statistical caveats.
