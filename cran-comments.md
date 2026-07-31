## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

* Windows R-devel (r90327) reported a NOTE while checking compiled code:
  `'cc' is not on the path`. The builder log shows that GCC is available but
  no `cc` executable is present on `PATH`. Windows release and local
  `--as-cran` checks complete without errors, warnings, or notes.
