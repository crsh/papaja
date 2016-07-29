#' Create title page
#'
#' Creates text for the title and abstract page for MS Word documents.
#' \emph{This function is not exported.}
#'
#' @param x List. Meta data of the document as a result from \code{\link[yaml]{load.yaml}}.
#' @seealso \code{\link{apa6_word}}

word_title_page <- function(x) {
  # Create title page and abstract
  # Hack together tables for centered elements -.-

  apa_terms <- options("papaja.terms")[[1]]

  authors <- paste0(sapply(x$author, function(y) paste0(y["name"], paste0("^", y["affiliation"], "^"), collapse = "")), collapse = "\n")

  affiliations <- lapply(x$affiliation, function(y) c(paste0("^", y["id"], "^"), y["institution"]))
  affiliations <- sapply(affiliations, paste, collapse = " ")

  corresponding_author <- x$author[[which(sapply(x$author, "[[", "corresponding"))]]

  author_note <- paste(
    x$author_note
    , paste0("Correspondence concerning this article should be addressed to ", corresponding_author$name, ", ", corresponding_author$address, ". E-mail: ", corresponding_author$email)
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
    , paste0("*", apa_terms$keywords, "* ", x$keywords)
    , "\n&nbsp;\n"
    , paste0(apa_terms$word_count, " Insert word count here!")
    , "\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n"
    , paste0("# ", x$title, "\n\n")
  )
}
