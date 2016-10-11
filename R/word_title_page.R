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

  apa_terms <- options("papaja.terms")[[1]]

  authors <- sapply(x$author, function(y) {
      affiliation <- if(!is.null(y[["affiliation"]])) paste0("^", y[["affiliation"]], "^") else ""
      paste0(y["name"], affiliation, collapse = "")
    })

  authors <- paste(authors, collapse = "")

  affiliations <- lapply(x$affiliation, function(y) c(paste0("^", y["id"], "^"), y["institution"]))
  affiliations <- sapply(affiliations, paste, collapse = " ")

  corresponding_author <- x$author[[which(unlist(lapply(x$author, "[[", "corresponding")))]]

  author_note <- paste(
    x$author_note
    , paste0(getOption("papaja.terms")$correspondence, corresponding_author$name, ", ", corresponding_author$address, ". ", getOption("papaja.terms")$email, ": ", corresponding_author$email)
    , sep = "\n\n"
  )

  padding <- paste0(c("\n", rep("&nbsp;", 148)), collapse = "") # Add spacer to last row
  # author_note <- paste(author_note, padding, sep = "\n")

  c(
    "\n\n"
    , paste(knitr::kable(c(authors, padding, affiliations, padding, x$note), format = "pandoc", align = "c"), collapse = "\n")
    , "\n\n&nbsp;\n"
    , paste0("# ", apa_terms$author_note)
    , "\n"
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
