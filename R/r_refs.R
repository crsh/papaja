#' Create a reference file for R and its packages
#'
#' Creates a .bib-reference file for the used R version and all attached R-packages, so they can be cited
#' in a Markdown-document using pandoc or LaTeX.
#' @param file Character. The path and name of the file to be created or updated.
#' @param prefix Character. A prefix to be used for all R-package reference handles.
#' @param ... Further arguments to pass to \code{knitr::write_bib}.
#' @seealso \code{\link[knitr]{write_bib}}
#' @details This function is a wrapper for \code{write_bib} from the \pkg{knitr} package. If a file exists
#'    at the specified location, the function reads the file and appends missing citation information to the
#'    end of the file. It is recommended to create a new .bib-file dedicated to R-related references and
#'    adding it to the \code{bibliography} parameter in the document's yaml-header.
#' @examples r_refs("test_bibliography.bib")
#' @export

r_refs <- function(file, prefix = "R-", ...) {
  requireNamespace("knitr", quietly = TRUE)

  validate(file, check_class = "character", check_NA = TRUE, check_length = 1)
  validate(prefix, check_class = "character", check_NA = TRUE, check_length = 1)

  package_list <- c("base", names(sessionInfo()$otherPkgs))

  if(file.exists(file)) {
    bib_file <- readLines(file)
    missing_packages <- sapply(package_list, function(x) !any(grep(paste0(prefix, x), bib_file)))
    missing_packages <- names(missing_packages[missing_packages])
  } else {
    missing_packages <- package_list
  }
  if(is.null(file)) knitr::write_bib(missing_packages, prefix = prefix, ...) else {
    sink(file, append = TRUE)
    knitr::write_bib(missing_packages, prefix = prefix, ...)
    sink()
  }
}
