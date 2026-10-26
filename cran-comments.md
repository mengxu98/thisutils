## R CMD check results

0 errors | 0 warnings | 1 note

* This updates the CRAN version 0.5.0 to version 0.5.3.
* Checked the built source tarball with R CMD check --as-cran on macOS arm64
  using R 4.5.3, including examples, tests, vignettes and the PDF manual.
* The note is "unable to verify current time" because the network time check
  was unavailable. No future file timestamps were reported.
* Remote CRAN incoming checks timed out; the completed local check used
  _R_CHECK_CRAN_INCOMING_REMOTE_=false. Current R-devel and final platform
  checks remain to be run on this candidate before submission.

## Reverse dependencies

The CRAN reverse imports inferCSN 1.2.0 and thisplot 0.4.3 were checked against
the rebuilt thisutils 0.5.3. Both completed with status OK. Checks against the
CRAN thisutils 0.5.0 baseline also completed with status OK, with no new problems.
