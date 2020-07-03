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
    if(!is.null(x$author_note)) author_note <- x$author_note
    if(!is.null(x$authornote)) author_note <- x$authornote

    contributors <- x$author[unlist(lapply(x$author, function(y) length(y$role) > 0))]

    if(length(contributors) > 0) {
      contributions <- unlist(lapply(contributors, function(x) paste0(x$name, ": ", paste(x$role, collapse = ", "))))

      author_note <- c(
        author_note
        , paste0(
          "The authors made the following contributions. "
          , paste(contributions, collapse = "; ")
          , "."
        )
      )
    }

    corresponding_author <- x$author[which(unlist(lapply(lapply(x$author, "[[", "corresponding"), isTRUE)))]

    if(length(corresponding_author) > 0) author_note <- c(author_note, corresponding_author_line(corresponding_author[[1]]))

    if(length(author_note) > 0) {
      author_note <- c(
        "<div custom-style='Author'>", apa_terms$author_note, "</div>"
        , "\n"
        , paste(author_note, collapse = "\n\n")
      )
    }

    ## Hack together author information table
    padding <- paste0(c("\n", rep("&nbsp;", 148)), collapse = "") # Add spacer to last row
    author_information <- c(
      "\n\n"
      , "<div custom-style='Author'>", authors, "</div>"
      , "<div custom-style='Author'>", affiliations, "</div>"
      , padding
      , "<div custom-style='Author'>", x$note, "</div>"
      , "\n\n&nbsp;\n"
    )
  }

  abstract <- paste0(
    "<div custom-style='h1-pagebreak'>", apa_terms$abstract, "</div>"
    , "\n"
    , x$abstract
    , "\n"
  )

  keywords <- paste0("*", apa_terms$keywords, ":* ", x$keywords, "\n")
  wordcount <- paste0("*", apa_terms$word_count, ":* ", x$wordcount, "\n")

  c(
    author_information
    , "\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n"
    , author_note
    , "\n"
    , ifelse(is.null(x$abstract), "", abstract)
    # is.null(x$abstract) for consistency with apa6_pdf()
    , ifelse(is.null(x$abstract) || is.null(x$keywords), "", keywords)
    , ifelse(is.null(x$abstract) || is.null(x$wordcount), "", wordcount)
    , paste0("<div custom-style='h1-pagebreak'>", x$title, "</div>\n\n")
  )
}


paste_authors <- function(x, format) {

  if(format == "latex") {
    authors <- lapply(x, function(y) {
      affiliation <- if(!is.null(y[["affiliation"]])) paste0("\\textsuperscript{", y[["affiliation"]], "}") else ""
      paste0(y["name"], affiliation, collapse = "")
    })
  } else if(format %in% c("docx", "word")) {
    authors <- lapply(x, function(y) {
      affiliation <- if(!is.null(y[["affiliation"]]) && y[["affiliation"]] != "") paste0("^", y[["affiliation"]], "^") else ""
      paste0(y["name"], affiliation, collapse = "")
    })
  } else {
    stop("Format not supported.")
  }

  authors <- unlist(authors)

  n_authors <- length(authors)
  x[[1]]$name <- authors[1]
  if(n_authors >= 2) {
    if(n_authors > 2) {
      x[[n_authors]]$name <- paste(", &", authors[n_authors])
      for(i in 2:(n_authors - 1)) {
        x[[i]]$name <- paste(",", authors[i])
      }
    } else {
      x[[n_authors]]$name <- paste("\\ &", authors[n_authors]) # Otherwise space before ampersand disappears
    }
  }
  if(format == "latex") x[[n_authors]]$name <- gsub("\\&", "\\\\&", x[[n_authors]]$name)
  paste(unlist(lapply(x, "[[", "name")), collapse = "")
}

paste_affiliations <- function(x, format) {
  add_superscript <- function(y, format) {
    if(is.null(y[["id"]]) || y[["id"]] == "") {
      superscript <- NULL
    } else if(format == "latex") {
      superscript <- paste0("\\textsuperscript{", y[["id"]], "}")
    } else if(format %in% c("docx", "word")) {
      superscript <- paste0("^", y[["id"]], "^")
    }  else {
      stop("Format not supported.")
    }

    location <- c(y[["institution"]], y[["city"]], y[["state"]], y[["country"]])
    location <- paste(escape_latex(location), collapse = ", ")

    paste(superscript, location)
  }

  affiliations <- vapply(x, add_superscript, format = format, FUN.VALUE = "a")
  if(format == "latex") {
    paste(affiliations, collapse = "\\\\")
  } else {
    paste(affiliations, collapse = "\n\n")
  }
}
