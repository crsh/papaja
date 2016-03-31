#' Prepare APA document
#'
#' Prepares objects to be used in the rendering process and creates title page and abstract for MS Word
#' documents.
#'
#' @details The function creates and locks a non-exported object \code{apa_lang} that is used by other
#'    \pkg{papaja}-functions. \code{apa_lang} is a \code{list} containing localizations for document
#'    elements such as abstract and title. The selected language is defined by the \code{lang}-parameter
#'    in the documents yaml-header. Currently, English (default) and German ("german") are supported.
#'
#'    If the output document is MS Word (\code{output: \link{apa6_word}}) the function additionally creates
#'    a title page and adds the abstract. You should, therefore, allways call \code{apa_prepare_doc} at the
#'    very beginning of the R Markdown document.
#' @seealso \code{\link{apa6_word}}
#' @examples apa_prepare_doc()
#' @export

apa_prepare_doc <- function() {
  # Run only if document is being rendered
  if(length(knitr::opts_knit$get("rmarkdown.pandoc.to")) > 0) {
    apa_metadata <- rmarkdown::metadata

    # Only one call allowed while rendering
    if(!exists("apa_lang" , envir = apa_doc_env, inherits = FALSE)) {
      create_apa_lang(apa_metadata$lang)

      # Hack MS Word output
      output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
      if(output_format == "word") {
        # Create title page and abstract
        # Hack together tables for centered elements -.-
        authors <- paste0(sapply(apa_metadata$author, function(x) paste0(x["name"], paste0("^", x["affiliation"], "^"), collapse = "")), collapse = "\n")

        padding <- paste0(c("\n", rep("&nbsp;", 148)), collapse = "") # Add spacer to last row
        affiliations <- lapply(apa_metadata$affiliation, function(x) c(paste0("^", x["id"], "^"), x["institution"]))
        affiliations <- sapply(affiliations, paste, collapse = " ")

        note <- paste(apa_metadata$note, padding, sep = "\n")

        cat("\n\n")
        print(knitr::kable(c(authors, padding, affiliations), format = "pandoc", align = "c"))
        cat(
          "\n\n"
          , "# ", apa_doc_env$apa_lang$author_note
          , "\n\n\n"
          , apa_metadata$note
          , "\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n"
          , "# ", apa_doc_env$apa_lang$abstract
          , "\n\n\n"
          , apa_metadata$abstract
          , "\n\n*", apa_doc_env$apa_lang$keywords, "* ", apa_metadata$keywords
          , "\n\n", apa_doc_env$apa_lang$word_count, " Insert word count here!"
          , "\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n"
          , "# ", apa_metadata$title, "\n\n"
          , sep = ""
        )
      }
    } else warning("Only one call to apa_before_body() can be executed.")
  }

  create_apa_lang("american")
}
