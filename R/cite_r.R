#' Cite R and R Packages
#'
#' Creates character strings to cite R and R packages.
#'
#' @param file Character. Path and name of the `.bib`-file holding the
#'   references. If `NULL`, only R is cited.
#' @param prefix Character. Prefix used for all R-package reference handles.
#' @param footnote Logical. Indicates if packages should be cited in a
#'   footnote. Ignored if no package information is available.
#' @param pkgs Character. Vector of package names to cite or omit depending
#'   on `omit`.
#' @param omit Logical. If `TRUE`, `pkgs` constitutes a list of packages
#'   *not* to cite (a blacklist). If `FALSE`, `pkgs` constitutes a list of
#'   packages to cite (a whitelist).
#' @param ... Additional arguments, which are currently ignored.
#' @details
#'   If `footnote = FALSE`, a character string citing R and R packages
#'   including version numbers is returned. Otherwise a named list with the
#'   elements `r` and `pkgs` is returned. The former element holds a character
#'   string citing R and a reference to a footnote; the latter element contains
#'   a character string that creates the footnote. For correct rendering, the
#'   footnote string needs to be a separate paragraph in the R Markdown
#'   document.
#' @return If `footnote = FALSE` a character string is returned, otherwise a
#'   named list with the elements `r` and `pkgs`.
#' @seealso [r_refs()], [knitr::write_bib()]
#' @examples cite_r()
#' @export

cite_r <- function(file = NULL, prefix = "R-", footnote = FALSE, pkgs = NULL, omit = TRUE, ...) {
  if(!is.null(file)) validate(file, check_class = "character", check_length = 1)
  validate(prefix, check_class = "character", check_length = 1)
  validate(footnote, check_class = "logical", check_length = 1)
  if(!is.null(pkgs)) validate(pkgs, check_class = "character")

  ellipsis <- list(...)
  if(!is.null(ellipsis$withhold)) {
    omit <- ellipsis$withhold
  }
  validate(omit, check_class = "logical", check_length = 1)

  r_version <- as.character(utils::packageVersion("base"))
  cite_just_r <- paste0("R [Version ", r_version, "; @", prefix, "base]")

  if(is.null(file) || !utils::file_test("-f", file)) { # Print R-reference if there is no .bib-file
    if(!is.null(file)) warning("File ", file, " not found. Cannot cite R-packages. If knitting again does not solve the problem, please check file path.")

    if(footnote) {
      res <- list()
      res$r <- cite_just_r

      res$pkgs <- NULL
      return(res)
    } else {
      return(cite_just_r)
    }

  }

  r_bib <- readLines(file)
  cite_keys <- r_bib[grepl(paste0("\\@\\w+\\{", prefix), r_bib)]
  cite_keys <- gsub("\\@\\w+\\{", "", cite_keys)
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

  # Remove packages according to pkgs
  bib$base <- NULL
  if(!is.null(pkgs)) {
    if(omit) {
      pkg_citations <- bib[!(gsub("\\_\\D", "", names(bib)) %in% pkgs)]
    } else {
      pkg_citations <- bib[gsub("\\_\\D", "", names(bib)) %in% pkgs]
    }
  } else {
    if(omit) {
      pkg_citations <- bib
    } else {
      return(cite_just_r)
    }
  }

  if(length(pkg_citations) == 0) {
    return(cite_just_r)
  }

  pkg_citations <- pkg_citations[names(sort(unlist(pkg_citations)))] # Sort packages alphabetically

  # Assemble (multiple) references and add package version numbers
  pkg_names <- names(pkg_citations)
  pkg_names <- unique(gsub("\\_\\D", "", pkg_names))
  pkg_names <- gsub("survival-book", "survival", pkg_names)
  pkg_versions <- sapply(pkg_names, function(x) if(package_available(x)) paste0("Version ", as.character(utils::packageVersion(x)), "\\; ") else "")
  pkg_keys <- sapply(pkg_names, function(x){
    keys <- pkg_citations[grepl(x, names(pkg_citations))]
    paste0("@", keys, collapse = "; ")
  })
  pkg_texts <- paste0(
    "*", pkg_names, "* "
    , "[", pkg_versions
    , pkg_keys, "]"
  )

  if(length(pkg_texts) > 1) {
    pkg_info <- paste(pkg_texts[1:(length(pkg_texts) - 1)], collapse = ", ")
    pkg_info <- paste0(pkg_info, ", and ", utils::tail(pkg_texts, 1))
  } else {
    pkg_info <- pkg_texts
  }

  complete_r_citaiton <- paste0("R [Version ", r_version, "\\; @", r_citation, "]")

  if(footnote) {
    res <- list()
    res$r <- paste0(complete_r_citaiton, "[^papaja_pkg_citations]")

    res$pkgs <- paste0("\n\n[^papaja_pkg_citations]: We, furthermore, used the R-packages ", pkg_info, ".\n\n")
  } else {
    res <- paste0(
      complete_r_citaiton, " and the R-package"
      , if(length(pkg_texts) > 1) "s", " " , pkg_info
    )
  }

  res
}
