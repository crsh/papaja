#' Save a collection from a Zotero-Account to a BibTeX-file
#'
#' Downloads and saves a Zotero reference library (or a subset) and saves it as BibTeX file.
#'
#' @param x Character. Zotero user or group ID, see details.
#' @param lib_type Character. Specifies if the supplied ID is associated with a Zotero \code{user} or \code{group}.
#' @param collection Character. Optional ID of a collection in the Zotero library, see details.
#' @param API_key Character. Zotero API key, see details.
#' @param bib_name Character. Name of the BibTex-file references are saved to.
#'
#' @details
#' This function retrieves references through the Zotero web API. \code{x} takes a Zotero user or group ID that can
#' be retrieved from the Zotero.org user or group \url{Feeds/API settings}{https://www.zotero.org/settings/keys}.
#' An authentication key (\code{API_key}) is required to access nonpublic Zotero libraries using the Zotero API.
#' Authentication keys can also be generated in the Zotero.org user or group
#' \url{Feeds/API settings}{https://www.zotero.org/settings/keys}.
#'
#' If the requested reference collection is larger than 100 records, multiple API calls are initiated because the
#' number of retrieved records is limited to 100 per API call. Frequent API calls will result in a temporary access
#' block. Thus, there is an (currently unknown) upper limit to the length of reference collections that can be
#' retrieved through this function. It is generally advisable to comment out the use of this function during periods
#' in which the R Markdown file is frequently rebuilt to limit the number of API calls and limit the number of references
#' to those needed for the current document by setting up collections in your Zotero library.
#'
#' Collection keys (\code{collection}), i.e. identifiers of reference library subsets, can be retrieved by accessing
#' them via a web browser. They keys are contained in the URL:
#'
#' \code{https://www.zotero.org/<USERNAME>/items/collectionKey/<COLLECTIONKEY>}
#'
#' Zotero web API calls can be slow, especially for large reference collections. If available, this function will
#' utilize the \pkg{downloader}-package, which speeds up reference downloads considerably.
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
  validate(bib_name, check_class = "character", check_length = 1)
  validate(lib_type, check_class = "character", check_length = 1)
  if(!lib_type %in% c("user", "group")) stop("Unknown lib_type. Must be either 'user' or 'group'.")
  if(!is.null(API_key)) validate(API_key, check_class = "character", check_length = 1)
  if(!is.null(collection)) validate(collection, check_class = "character", check_length = 1)

  downloader_installed <- "downloader" %in% rownames(installed.packages())

  # Load list of entry keys (no limit) and determine the number of splits for the final library
  collection_url <- paste0("https://api.zotero.org/", paste0(lib_type, "s/"), x, "/")
  if(!is.null(collection)) collection_url <- paste0(collection_url, "collections/", collection, "/")
  key_url <- paste0(collection_url, "items?format=keys")
  if(!is.null(API_key)) key_url <- paste0(key_url, "&key=", API_key)

  if(downloader_installed) {
    tmp_bib_file <- paste(sample(c(letters, LETTERS, 0:9), size = 32, replace = TRUE), collapse = "")
    downloader::download(key_url, destfile = paste0(tmp_bib_file, ".txt"), quiet = TRUE, mode = "a")
    collection_keys <- readLines(paste0(tmp_bib_file, ".txt"), warn = FALSE)
    file.remove(paste0(tmp_bib_file, ".txt"))
  } else {
    collection_keys <- readLines(key_url)
  }
  no_batches <- length(collection_keys) %/% 100 + 1

  # Download BibTeX for all entries in batches of 100 using downloder-package or readLines()
  if(downloader_installed) {
    tmp_bib_file <- paste(sample(c(letters, LETTERS, 0:9), size = 32, replace = TRUE), collapse = "")

    for(i in 1:no_batches) {
      bib_url <- paste0(collection_url, "/items?&limit=100&start=", (i-1)*10, "&format=bibtex")
      if(!is.null(API_key)) bib_url <- paste0(bib_url, "&key=", API_key)

      downloader::download(bib_url, destfile = paste0(tmp_bib_file, ".txt"), quiet = TRUE, mode = "a")
      bib_file <- readLines(paste0(tmp_bib_file, ".txt"), warn = FALSE)
      writeLines(bib_file, bib_name)
    }

    file.remove(paste0(tmp_bib_file, ".txt"))
  } else {
    for(i in 1:no_batches) {
      bib_url <- paste0(collection_url, "/items?&limit=100&start=", (i-1)*10, "&format=bibtex")
      if(!is.null(API_key)) bib_url <- paste0(bib_url, "&key=", API_key)

      ref_batch <- readLines(bib_url, warn = FALSE)
      ref_batch <- paste(ref_batch, collapse = "\n")
      writeLines(ref_batch, bib_name)
    }
  }

  return(invisible(bib_name))
}
