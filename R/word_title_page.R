#' Create title page
#'
#' Creates text for the title and abstract page for MS Word documents.
#' \emph{This function is not exported.}
#'
#' @param x List. Meta data of the document as a result from \code{\link[yaml]{yaml.load}}.
#' @keywords internal
#' @seealso \code{\link{apa6_word}}

word_title_page <- function(x) {
  # Create title page and abstract
  # Hack together tables for centered elements

  apa_terms <- getOption("papaja.terms")

  author_note <- c()
  author_information <- c()

  if(is.null(x$mask) || !x$mask) {
    ## Concatenate author names
    authors <- paste_authors(x$author, format = "word")

    ## Add superscripts to affiliation line
    affiliations <- paste_affiliations(x$affiliation, format = "word")

    ## Assemble author note
    corresponding_author <- x$author[which(unlist(lapply(lapply(x$author, "[[", "corresponding"), isTRUE)))]

    if(!is.null(x$author_note)) author_note <- x$author_note
    if(length(corresponding_author) > 0) author_note <- c(author_note, corresponding_author_line(corresponding_author[[1]]))

    if(length(author_note) > 0) {
      author_note <- c(
        paste0("# ", apa_terms$author_note)
        , "\n"
        , paste(author_note, collapse = "\n\n")
      )
    }

    ## Hack together author information table
    padding <- paste0(c("\n", rep("&nbsp;", 148)), collapse = "") # Add spacer to last row
    author_information <- c(
      "\n\n"
      , paste(
        knitr::kable(
          c(authors, affiliations, padding, x$note)
          , format = "pandoc"
          , align = "c"
          , col.names = NULL
        )
        , collapse = "\n"
      )
      , "\n\n&nbsp;\n"
    )
  }

  c(
    author_information
    , "\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n"
    , author_note
    , "\n"
    , paste0("<div custom-style='Title'>", apa_terms$abstract, "</div>")
    , "\n"
    , x$abstract
    , paste0("*", apa_terms$keywords, ":* ", x$keywords)
    , "\n"
    , paste0(apa_terms$word_count, ": ", x$wordcount)
    , "\n"
    , paste0("<div custom-style='Title'>", x$title, "</div>\n\n")
  )
}
