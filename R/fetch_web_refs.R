#' Fetch a .bib-reference file from the web
#'
#' Downloads and saves a \code{.bib}-reference file form the web, so it can be used to cite references
#' in a Markdown-document using pandoc or LaTeX.
#'
#' @param x Character. URL of the \code{.bib}-file to fetch.
#' @param bib_name Character. The path and name of the file to be created.
#' @details
#'    If the function is called in an RMarkdown-document the file name specified as \code{bib_name}
#'    can be used in the YAML header as \code{bibliography}
#' @seealso \code{\link{cite_r}}, \code{\link{r_refs}}, \code{\link[knitr]{write_bib}}
#' @examples NULL
#' @export

fetch_web_refs <- function(x, bib_name) {
  validate(x, check_class = "character", check_NA = TRUE, check_length = 1)
  validate(bib_name, check_class = "character", check_NA = TRUE, check_length = 1)

  tmp_bib_file <- paste(sample(c(letters, LETTERS, 0:9), size = 32, replace = TRUE), collapse = "")
  tmp_bib_file <- paste0(tmp_name, ".txt")
  downloader::download(x, destfile = tmp_bib_file, quiet = TRUE)
  bib_file <- readLines(tmp_bib_file, warn = FALSE)
  writeLines(tmp_bib_file, bib_name)
  file.remove(tmp_bib_file)

  invisible(0)
}
