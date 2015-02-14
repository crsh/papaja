#' Prepare APA document
#'
#' Prepares objekts to be used in the rendering process and creates title page and abstract in MS Word
#' documents.
#'
#' @param x Character. Document language to set. Ignored and replaced by yaml-header parameter \code{lang}
#'    when document is rendered.
#' @details The function creates and locks a non-exported object \code{apa_lang} that is used by other
#'    \pkg{papaja}-functions. \code{apa_lang} is a \code{list} containing localizations for document
#'    elements such as abstract title. The selected language is defined by the \code{lang}-parameter
#'    in the documents yaml-header. Currently, English (default) and German ("german") are supported.
#'
#'    If the output document is MS Word (\code{output: \link{apa6_word}}) the function additionally creates
#'    a title page and adds the abstract.
#' @seealso \code{\link{apa6_word}}
#' @examples apa_prepare_doc()
#' @export

apa_prepare_doc <- function(lang = "american") {
  requireNamespace("rmarkdown", quietly = TRUE)

  validate(lang, "lang", check.class = "character", check.NA = TRUE, check.length = 1)

  # Run only if document is being rendered
  if(length(knitr::opts_knit$get("rmarkdown.pandoc.to")) > 0) {

    # Only one call allowed while rendering
    if(!exists("apa_lang" , envir = papaja:::apa_doc_env, inherits = FALSE)) {
      create_apa_lang(lang)

      # Hack MS Word output
      requireNamespace("knitr", quietly = TRUE)
      output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
      if(output_format == "word") {
        # Create title page and abstract
        requireNamespace("rmarkdown", quietly = TRUE)
        apa_metadata <- rmarkdown::metadata

        # Hack together tables for centered elements -.-
        padding <- paste0(rep("&nbsp;", 148), collapse = "") # Add spacer to last row
        affiliations <- paste0(apa_metadata$affiliation, padding)
        note <- paste0(apa_metadata$note, padding)
        cat("\n\n")
        print(knitr::kable(affiliations, format = "pandoc", align = "c"))
        cat(
          "\n\n", apa_metadata$note
          , "\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n"
          , "# ", apa_doc_env$apa_lang$abstract
          , "\n\n\n"
          , apa_metadata$abstract
          , "\n\n*", apa_doc_env$apa_lang$keywords, "* ", apa_metadata$keywords
          , "\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n"
          , "# ", apa_metadata$title, "\n\n"
          , sep = ""
        )
      }
    } else warning("Only one call to apa_before_body() can be executed.")
  }

  create_apa_lang(lang)
}
