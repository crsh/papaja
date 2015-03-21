#' Cite R and R-packages
#'
#' Creates a character string citing R and R-packages.
#'
#' @param file Character. The path and name of the \code{.bib}-file holding the references.
#' @param prefix Character. The prefix used for all R-package reference handles.
#' @param footnote Logical. Indicates if packages should be cited in a footnote. Ignored if no package information
#'    is available.
#' @param pkgs Character. Vector of package names to cite. If \code{pkgs = "all"} (default) packages in \code{file}
#'    are cited.
#' @details
#'    If \code{footnote = FALSE} a character string citing R and R-packages including version
#'    numbers is returned. Otherwise a named list with the elements \code{r} and \code{pkgs} is returned. The
#'    former element holds a character string citing R and a reference to a footnote; the latter element contains
#'    a character string for the footnote citing R-packages. For correct rendering, the footnote string needs
#'    to be a separate paragraph.
#' @return If \code{footnote = FALSE} a character string is returned and a named list with the elements \code{r}
#'    and \code{pkgs} otherwise.
#' @seealso \code{\link{r_refs}}, \code{\link[knitr]{write_bib}}
#' @examples cite_r()
#' @export

cite_r <- function(file = NULL, prefix = "R-", footnote = FALSE, pkgs = "all") {
  if(!is.null(file)) validate(file, check_class = "character", check_length = 1)
  validate(prefix, check_class = "character", check_length = 1)
  validate(footnote, check_class = "logical", check_length = 1)
  validate(pkgs, check_class = "character")

  r_version <- as.character(packageVersion("base"))
  cite_just_r <- paste0("R [", r_version, ", @", prefix, "base]")

  if(is.null(file) || !file_test("-f", file)) { # Print R-reference if there is no .bib-file
    if(!is.null(file) || pkgs != "all") warning("File ", file, " not found. Cannot cite R-packages. If knitting again does not solve the problem, please check file path.")
    return(cite_just_r)
  }

  r_bib <- readLines(file)
  cite_keys <- r_bib[grepl(paste0("\\@Manual\\{", prefix), r_bib)]
  cite_keys <- gsub("\\@Manual\\{", "", cite_keys)
  cite_keys <- gsub("\\,", "", cite_keys)

  bib <- sapply(
    cite_keys
    , function(x) {
      y <- list(x)
      names(y) <- gsub(prefix, "", x)
      y
    }
    , USE.NAMES = FALSE
  )

  r_citation <- bib$base
  if(length(pkgs) > 1 && pkgs != "all") {
    pkg_citations <- bib[names(bib) %in% pkgs]
  } else {
    pkg_citations <- bib[!names(bib) == "base"]
  }

  if(length(pkg_citations) == 0) {
    return(cite_just_r)
  }

  pkg_texts <- paste0(
    "*", names(pkg_citations), "* "
    , "[", sapply(names(pkg_citations), function(x) as.character(packageVersion(x)))
    , ", @", unlist(pkg_citations), "]"
  )

  if(length(pkg_texts) > 1) {
    pkg_info <- paste(pkg_texts[1:(length(pkg_texts) - 1)], collapse = ", ")
    pkg_info <- paste0(pkg_info, ", and ", tail(pkg_texts, 1))
  } else {
    pkg_info <- pkg_texts
  }

  if(footnote) {
    res <- list()
    res$r <- paste0("R [", r_version, ", @", bib$base, "][^papaja_pkg_citations]")

    res$pkgs <- paste0("\n\n[^papaja_pkg_citations]: We, furthermore, used the R-packages ", pkg_info, ".\n\n")
  } else {
    res <- paste0(
      "R [", r_version, ", @", bib$base, "] and the R-package"
      , if(length(pkg_texts) > 1) "s", " " , pkg_info
    )
  }

  res
}
