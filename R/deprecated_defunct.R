#' Prepare APA document (deprecated)
#'
#' Prepares objects to be used in the rendering process and creates title page
#' and abstract for MS Word documents. *This function is no longer needed
#' and thus has been deprecated.*
#'
#' @details The function creates and locks a non-exported object `apa_lang`
#'   that is used by other \pkg{papaja}-functions. `apa_lang` is a
#'   `list` containing localizations for document elements such as abstract
#'   and title. The selected language is defined by the `lang`-parameter
#'   in the documents yaml-header. Currently, English (default) and German
#'   (`"german"`) are supported.
#'
#'   If the output document is MS Word (`output: \link{apa6_word}`) the
#'   function additionally creates a title page and adds the abstract. You
#'   should, therefore, always call `apa_prepare_doc` at the very
#'   beginning of the R Markdown document.
#' @seealso \code{\link{apa6_word}}
#' @export

apa_prepare_doc <- function() {
  .Deprecated(msg = "'apa_prepare_doc' is deprecated as of version 0.1.0.9423 because it is no longer needed. You may remove it from your manuscript without consequences.")
  return(invisible(NULL))
}



#' Fetch a .bib-reference file from the web (defunct)
#'
#' Downloads and saves a `.bib`-reference file form the web, so it can be
#' used to cite references in a Markdown-document using pandoc or LaTeX.
#' *This function has been defunct. Please use
#' \code{\link[downloader]{download}} instead.*
#'
#' @param x Character. URL of the \code{.bib}-file to fetch.
#' @param bib_name Character. The path and name of the file to be created.
#' @details
#'   If the function is called in an RMarkdown-document the file name
#'   specified as \code{bib_name} mcan be used in the YAML header as
#'   \code{bibliography}.
#' @seealso \code{\link{cite_r}}, \code{\link{r_refs}},
#'   \code{\link[knitr]{write_bib}}
#' @examples NULL
#' @export

fetch_web_refs <- function(x, bib_name) {
  .Defunct("downloader::download")
  return(invisible(NULL))
}


#' Save a collection from a Zotero-Account to a BibTeX-file (defunct)
#'
#' Downloads and saves a Zotero reference library (or a subset) and saves it as
#' BibTeX file. *This function now lives in the package \pkg{citr}.*
#'
#' @param x Character. Zotero user or group ID, see details.
#' @param lib_type Character. Specifies if the supplied ID is associated with a
#'   Zotero `user` or `group`.
#' @param collection Character. Optional ID of a collection in the Zotero
#'   library, see details.
#' @param API_key Character. Zotero API key, see details.
#' @param bib_name Character. Name of the BibTeX-file references are saved to.
#'
#' @details
#'  This function retrieves references through the Zotero web API. `x`
#'  takes a Zotero user or group ID that can be retrieved from the Zotero.org
#'  user or group \href{https://www.zotero.org/settings/keys}{Feeds/API settings}.
#'  An authentication key (`API_key`) is required to access nonpublic
#'  Zotero libraries. Authentication keys can also be generated in the
#'  Zotero.org user or group \href{https://www.zotero.org/settings/keys}{Feeds/API settings}.
#'
#'  If the requested reference collection is larger than 100 records, multiple
#'  API calls are initiated because the number of retrieved records is limited
#'  to 100 per API call. Frequent API calls will result in a temporary access
#'  block. Thus, there is an (currently unknown) upper limit to the length of
#'  reference collections that can be retrieved through this function. It is
#'  generally recommended to comment out calls to this function in R
#'  Markdown documents during periods of frequent knitting to limit the number
#'  of API calls and limit the number of references to those needed for the
#'  current document by setting up collections in your Zotero library.
#'
#'  Collection keys (\code{collection}), i.e. identifiers of reference library
#'  subsets, can be retrieved by accessing them via a web browser. They keys
#'  are contained in the URL:
#'
#' `https://www.zotero.org/<USERNAME>/items/collectionKey/<COLLECTIONKEY>`
#'
#'  Zotero web API calls can be slow, especially for large reference
#'  collections. If available, this function will use the
#'  \pkg{downloader}-package, which speeds up reference downloads considerably.
#'
#' @author Christoph Stahl, Frederik Aust
#' @return Returns \code{bib_name} invisibly.
#'
#' @seealso \code{\link{r_refs}}, \code{\link{cite_r}}
#'
#' @examples NULL

fetch_zotero_refs <- function(
  x
  , bib_name
  , API_key = NULL
  , collection = NULL
  , lib_type = "user"
) {
  .Defunct("citr::fetch_zotero_refs")
  return(invisible(NULL))
}


apa_table.word <- function(x, ...) {
  .Deprecated(msg = "'apa_table.word' is deprecated as of version 0.1.0.9793 because it was more accurately renamed to 'apa_table.markdown'.")

  apa_table.markdown(x, ...)
}
