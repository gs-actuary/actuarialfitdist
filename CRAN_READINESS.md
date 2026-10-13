# CRAN submission checklist: actuarialfitdist 0.2.3

**Do not submit until all checks have been run on this exact version.**

## Completed in this documentation pass

- Checked the package metadata: Title shortened, an actuarial loss modeling
  reference added, maintainer/author/copyright roles retained.
- Expanded documentation of model outputs, plotting, rating factors,
  statistical assumptions, limitations and terminology.
- Updated the README installation guidance, NEWS and release notes.
- Preserved 0.2.2 statistical algorithms unchanged.

## To do in R on the maintainer's machine

```r
devtools::document()
devtools::test()
devtools::check_doc_fields()
urlchecker::url_check()
devtools::check(document = FALSE, args = "--as-cran", remote = TRUE, manual = TRUE)
devtools::check_win_devel()
```

Review all changes to `man/` and `NAMESPACE` after `document()`.
Inspect the rendered HTML and PDF help pages and both vignettes.
Confirm examples run quickly, without network access or writing outside tempdir.
Run the external 0.2.2 numerical validation script again (it applies to 0.2.3
because the fitting code is unchanged), including the large synthetic data
scenarios. Verify cross-platform CI on a newly pushed 0.2.3 commit.
Check the built source tarball with `R CMD check --as-cran`, including PDF manual.

Update `cran-comments.md` with actual check results before submission.
Use https://r-pkgs.org/release.html and CRAN's submission checklist and policies:
https://cran.r-project.org/web/packages/submission_checklist.html
https://cran.r-project.org/web/packages/policies.html

No R binary was available in the environment that produced this archive.
Static checks cannot establish that the package has 0 errors, 0 warnings,
or 0 notes under `R CMD check`.
