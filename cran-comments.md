## Test environments

- macOS 12.1, R 4.1.2 (local)
- macOS-latest, R-release (r-lib actions)
- Apple Silicon (M1), macOS 11.6 Big Sur, R-release (r-hub)
- Windows Server 2022, R-devel (win-builder)
- Windows Server 2008, R-release (win-builder)
- Windows-latest, R-release (r-lib actions)
- Ubuntu 20.04, R-devel (r-lib actions)
- Ubuntu 20.04, R-release (r-lib actions)
- Ubuntu 20.04, R-oldrel (r-lib actions)
- Ubuntu 20.04, R 3.6 (r-lib actions)
- Fedora Linux, R-devel, clang, gfortran (r-hub)

## R CMD check results

0 ERRORs | 0 WARNINGs | 1 NOTE

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Frederik Aust <frederik.aust@uni-koeln.de>'

Found the following (possibly) invalid URLs:
  URL: https://doi.org/10.1037/1082-989X.9.2.164
    From: man/apa_print.glm.Rd
          man/apa_print.list.Rd
    Status: 400
    Message: Bad Request
  URL: https://doi.org/10.1177/0013164406292030
    From: man/apa_print.list.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://doi.org/10.1177/0013164410379335
    From: man/apa_print.list.Rd
    Status: 503
    Message: Service Unavailable
  URL: https://pandoc.org/
    From: man/papaja.Rd
          README.md
    Status: 221

## Comments

All DOIs/URLs are correct. Maybe the sites are blocking non-human traffic?

This is a resubmission, to address a Apple M1 issue (https://cran.r-project.org/web/checks/check_results_papaja.html)

## Downstream dependencies

There are no downstream dependencies.
