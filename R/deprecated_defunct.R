#' Prepare APA document (deprecated)
#'
#' Prepares objects to be used in the rendering process and creates title page and abstract for MS Word
#' documents. \emph{This function is no longer needed and thus has been deprecated.}
#'
#' @details The function creates and locks a non-exported object \code{apa_lang} that is used by other
#'    \pkg{papaja}-functions. \code{apa_lang} is a \code{list} containing localizations for document
#'    elements such as abstract and title. The selected language is defined by the \code{lang}-parameter
#'    in the documents yaml-header. Currently, English (default) and German ("german") are supported.
#'
#'    If the output document is MS Word (\code{output: \link{apa6_word}}) the function additionally creates
#'    a title page and adds the abstract. You should, therefore, always call \code{apa_prepare_doc} at the
#'    very beginning of the R Markdown document.
#' @seealso \code{\link{apa6_word}}
#' @examples apa_prepare_doc()
#' @export

apa_prepare_doc <- function() {
  .Deprecated(msg = "'apa_prepare_doc' is deprecated as of version 0.1.0.9423 because it is no longer needed. You may remove it from your manuscript without consequences.")
  return(invisible())
}



#' Fetch a .bib-reference file from the web (defunct)
#'
#' Downloads and saves a \code{.bib}-reference file form the web, so it can be used to cite references
#' in a Markdown-document using pandoc or LaTeX. \emph{This function has been defunct. Please use
#' \code{\link[downloader]{download}} instead.}
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
  .Defunct("downloader::download")
  return(invisible())
}


apa_table.word <- function(x, ...) {
  .Deprecated(msg = "'apa_table.word' is deprecated as of version 0.1.0.9793 because it was more accurately renamed to 'apa_table.markdown'.")

  apa_table.markdown(x, ...)
}
