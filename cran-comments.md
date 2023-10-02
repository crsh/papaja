## Test environments

- macOS 12.1, R 4.3.1 (local)
- macOS-latest, R-release (r-lib actions)
- Apple Silicon (M1), macOS 11.6 Big Sur, R-release (r-hub)
- Windows Server 2022, R-devel (win-builder)
- Windows Server 2008, R-release (win-builder)
- Windows-latest, R-release (r-lib actions)
- Ubuntu 20.04, R-devel (r-lib actions)
- Ubuntu 20.04, R-release (r-lib actions)
- Ubuntu 20.04, R-oldrel (r-lib actions)
- Fedora Linux, R-devel, clang, gfortran (r-hub)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Frederik Aust <frederik.aust@uni-koeln.de>'

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2023-09-26 as check issues were not corrected in time.

Found the following (possibly) invalid URLs:
  URL: https://stackoverflow.com/help/minimal-reproducible-example
    From: man/papaja.Rd
          README.md
    Status: 404
    Message: Not Found


## Comments

This is a resubmission, to address the invalid use of 'paired' in formula method of t.test() and wilcoxon.test() (https://cran-archive.r-project.org/web/checks/2023/2023-09-26_check_results_papaja.html)

All URLs are correct. Maybe the sites are blocking non-human traffic?
