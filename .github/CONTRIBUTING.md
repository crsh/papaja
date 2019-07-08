# Contributing to papaja

This document outlines how to propose a change to `papaja`.

## Minor changes

### Fixing typos

Small typos or grammatical errors in documentation may be edited directly using
the [GitHub web interface](https://help.github.com/en/articles/editing-files-in-your-repository), so long as the changes are made in the _source_ file.

*  YES: you edit a roxygen comment preceeded by `#'` in a `.R` file below `R/`.
*  NO: you edit an `.Rd` file below `man/`.

## Substantial changes

### Prerequisites

Before you make a substantial pull request, you should always file an issue and
make sure someone from the team agrees that it’s a problem.
If you’ve found a bug, create an associated issue and illustrate the bug with a minimal 
[reproducible example](https://www.tidyverse.org/help/#reprex).

### Pull request process

*  We recommend that you create a `git` branch for each pull request (PR).  
*  Look at the Travis build status before and after making changes (Did you break anything?).
The `README` should contain badges for any continuous integration services used
by the package.  
*  New code should follow the tidyverse [style guide](http://style.tidyverse.org).
You can use the [styler](https://CRAN.R-project.org/package=styler) package to
apply these styles, but please don't restyle code that has nothing to do with 
your PR.  
*  We use [`roxygen2`](https://cran.r-project.org/package=roxygen2), with
[Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/markdown.html), 
for documentation. Please update any documentation to reflect your changes.
*  We use [`testthat`](https://cran.r-project.org/package=testthat). Contributions
with test cases included are easier to accept.
*  We validate user input using either `validate()` or [`assertthat`](https://cran.r-project.org/package=assertthat) to ensure informative error messages.
*  For user-facing changes, add a bullet to the top of `NEWS.md` below the
current development version header describing the changes made, followed by your
GitHub username, and links to relevant issue(s)/PR(s).
*  If you are interested in add new methods to `apa_print()`, please review the getting-started vignette (`vignette("add_apa_print_methods", package = "papaja")`).

### Code of Conduct

Please note that the papaja project is released with a [contributor code of conduct](CODE_OF_CONDUCT.md).
By contributing to this project you agree to abide by its terms.
