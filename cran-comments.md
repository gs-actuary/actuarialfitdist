## Resubmission

This is a resubmission following the initial CRAN incoming pre-tests.

- Reduced the sample size in the `fit_spliced_severity()` example from 120 to 50 observations to address the Windows example execution-time NOTE.
- The remaining incoming-feasibility NOTE concerns the expected new-submission status and spelling flags for the legitimate author surnames Klugman, Panjer, and Willmot, and the edition notation "th".

All other checks passed in the previous submission.

## Submission status

Initial CRAN submission of actuarialfitdist 0.2.3.

## R CMD check results

- Local Windows 11, R 4.6.1, `--as-cran`: 0 errors, 0 warnings, 1 NOTE.
- Win-builder, R-devel (2026-10-14): 0 errors, 0 warnings, 1 NOTE.
- GitHub Actions: All five jobs passed across Windows, macOS, and Linux, including R-devel.

## Notes

The NOTE on both local and Win-builder checks concerns the expected "New submission" status.

Win-builder additionally identifies possible spelling errors in the DESCRIPTION reference: "Klugman", "Panjer", "Willmot", and "th". The first three are author surnames, and "th" is part of the edition notation. These are not spelling errors.

The local check skipped PDF manual generation because LaTeX is not installed locally. Win-builder successfully generated and checked both the PDF and HTML manuals.

There are no other NOTEs or warnings.