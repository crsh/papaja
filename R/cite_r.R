#' Cite R and loaded R-packages
#'
#' Creates a character string citing R and all loaded R-packages.
#'
#' @param file Character. The path and name of the \code{.bib}-file holding the references.
#' @param prefix Character. The prefix used for all R-package reference handles.
#' @param footnote Logical. Indicates if packages should be cited in a footnote.
#' @param cite Character. Vector of package names to cite. If \code{cite = "all"} (default) all loaded packages
#'    are cited.
#' @details
#'
#'    \code{\link{cite_r}}.
#' @return If \code{footnote = FALSE} a character string citing R and R-packages including version
#'    numbers is returned. Otherwise a named list with the elements \code{r} and \code{pkgs} is returned. The
#'    former element holds a character string citing R and a reference to a footnote; the latter element contains
#'    a character string for the footnote citing R-packages. For correct rendering, the footnote string needs
#'    to be a separate paragraph.
#' @examples NULL
#' @export

cite_r <- function(file = NULL, prefix = "R-", footnote = FALSE, cite = "all") {

  r_version <- sessionInfo()$R.version

  if(!file_test("-f", file)) { # Print R-reference if r_refs() was not run, yet.
    warning("File ", file, " not found. Cannot cite R-packages. If knitting again does not solve the problem, please check file path.")
    cite_just_r <- paste0(
      sentence_beginning
      , paste(r_version$major, r_version$minor, sep = ".")
      , ", @", prefix, "base]"
    )
    return(cite_just_r)
  }

  r_bib <- readLines(file)
  cite_keys <- r_bib[grepl("\\@Manual", r_bib)]
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
  if(cite != "all") {
    pkg_citations <- bib[names(bib) == cite]
  } else {
    pkg_citations <- bib[!names(bib) == "base"]
  }

  if(length(pkg_citations) == 0) {
    cite_just_r <- paste0(
      "R ["
      , paste(r_version$major, r_version$minor, sep = ".")
      , ", @", prefix, "base]"
    )
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
    res$r <- paste0(
      "R ["
      , paste(r_version$major, r_version$minor, sep = ".")
      , ", @", bib$base, "][^papaja_pkg_citations]"
    )

    res$pkgs <- paste0("\n\n[^papaja_pkg_citations]: We, furthermore, used the R-packages ", pkg_info, ".\n\n")
  } else {
    res <- paste0(
      "R ["
      , paste(r_version$major, r_version$minor, sep = ".")
      , ", @", bib$base, "] and the R-package", if(length(pkg_texts) > 1) "s", " "
      , pkg_info
    )
  }

  res
}
