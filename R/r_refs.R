#' Create a reference file for R and its packages
#'
#' Creates a .bib-reference file for the used R version and all attached R-packages, so they can be cited
#' in a Markdown-document using pandoc or LaTeX.
#'
#' @param file Character. The path and name of the file to be created or updated.
#' @param prefix Character. A prefix to be used for all R-package reference handles.
#' @param ... Further arguments to pass to \code{\link[knitr]{write_bib}}.
#' @details This function is a wrapper for \code{write_bib} from the \pkg{knitr} package. If a file exists
#'    at the specified location, the function reads the file and appends missing citation information to the
#'    end of the file. It is recommended to create a new .bib-file dedicated to R-related references and
#'    adding it to the \code{bibliography} parameter in the document's yaml-header.
#' @seealso \code{\link[knitr]{write_bib}}
#' @examples NULL
#' @export

r_refs <- function(file, prefix = "R-", ...) {
  validate(file, check_class = "character", check_NA = TRUE, check_length = 1)
  validate(prefix, check_class = "character", check_NA = TRUE, check_length = 1)

  # Ensure that cached packages are also citable
  cache_path <- knitr::opts_chunk$get("cache.path")
  if (!is.null(cache_path) && file_test("-d", cache_path)) {
    cached_pkgs <- readLines(paste0(cache_path, "__packages"))
    cached_pkgs <- setdiff(cached_pkgs, r_session$basePkgs)
    pkgs_to_cite <- unique(c(names(r_session$otherPkgs), cached_pkgs))
  } else {
    pkgs_to_cite <- names(r_session$otherPkgs)
  }

  pkg_list <- c("base", sort(pkgs_to_cite))

  if(file_test("-f", file)) {
    bib_file <- readLines(file)
    missing_pkgs <- sapply(pkg_list, function(x) !any(grep(paste0(prefix, x), bib_file)))
    missing_pkgs <- names(missing_pkgs[missing_pkgs])
  } else {
    missing_pkgs <- pkg_list
  }

  if(!is.null(missing_pkgs)) {
    if(is.null(file)) {
      knitr::write_bib(missing_pkgs, prefix = prefix, ...)
    } else {
      sink(file, append = TRUE)
      knitr::write_bib(missing_pkgs, prefix = prefix, ...)
      sink()
    }
  }
}
