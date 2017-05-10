#' Create title page
#'
#' Creates text for the title and abstract page for MS Word documents.
#' \emph{This function is not exported.}
#'
#' @param x List. Meta data of the document as a result from \code{\link[yaml]{yaml.load}}.
#' @seealso \code{\link{apa6_word}}

word_title_page <- function(x) {
  # Create title page and abstract
  # Hack together tables for centered elements -.-

  apa_terms <- getOption("papaja.terms")

  authors <- sapply(x$author, function(y) {
      affiliation <- if(!is.null(y[["affiliation"]])) paste0("^", y[["affiliation"]], "^") else ""
      paste0(y["name"], affiliation, collapse = "")
    })

  authors <- paste(authors, collapse = "")

  affiliations <- lapply(x$affiliation, function(y) c(paste0("^", y["id"], "^"), y["institution"]))
  affiliations <- sapply(affiliations, paste, collapse = " ")

  corresponding_author <- x$author[which(unlist(lapply(lapply(x$author, "[[", "corresponding"), isTRUE)))]

  author_note <- c()
  if(!is.null(x$author_note)) author_note <- x$author_note
  if(length(corresponding_author) > 0) author_note <- c(author_note, corresponding_author_line(corresponding_author[[1]]))

  if(length(author_note) > 0) {
    author_note <- c(
      paste0("# ", apa_terms$author_note)
      , "\n"
      , paste(author_note, collapse = "\n\n")
    )
  }

  padding <- paste0(c("\n", rep("&nbsp;", 148)), collapse = "") # Add spacer to last row
  # author_note <- paste(author_note, padding, sep = "\n")

  c(
    "\n\n"
    , paste(knitr::kable(c(authors, padding, affiliations, padding, x$note), format = "pandoc", align = "c"), collapse = "\n")
    , "\n\n&nbsp;\n"
    , author_note
    , "\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n"
    , paste0("# ", apa_terms$abstract)
    , "\n\n\n"
    , x$abstract
    , "\n\n"
    , paste0("*", apa_terms$keywords, ":* ", x$keywords)
    , "\n"
    , paste0(apa_terms$word_count, ": ", x$wordcount)
    , "\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n"
    , paste0("# ", x$title, "\n\n")
  )
}
