#' Prepare APA document
#'
#' Prepares objekts to be used in the rendering process and creates title page and abstract in MS Word
#' documents.
#'
#' @param x Character. Document language to be set. Ignored and replaced by yaml-header parameter \code{lang}
#'    when document is rendered.
#' @details The function creates and locks the object \code{apa_lang} in the global environment that is
#'    needed by other \pkg{papaja}-functions. \code{apa_lang} is a \code{list} containing localizations for
#'    document elements such as abstract title. The selected language is defined by the \code{lang}-parameter
#'    in the documents yaml-header. Currently, English (default) and German ("german") are supported.
#'
#'    If the output document is MS Word (\code{output: \link{apa6_word}}) the function additionally creates
#'    a title page and adds the abstract before the document body.
#' @seealso \code{\link{apa6_word}}
#' @examples apa_before_body()
#' @export

apa_before_body <- function(x = "default") {
  requireNamespace("rmarkdown", quietly = TRUE)

  # Run only if document is being rendered
  if(length(knitr::opts_knit$get("rmarkdown.pandoc.to")) > 0) {

    # Only one call allowed while rendering
    if(!exists("apa_lang", where = .GlobalEnv)) {
      create_apa_lang()

      # Hack MS Word output
      requireNamespace("knitr", quietly = TRUE)
      output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
      if(output_format == "word") {
        # Create title page and abstract
        requireNamespace("rmarkdown", quietly = TRUE)
        apa_metadata <- rmarkdown::metadata
        cat(paste0(
          "\n\n\n", paste("#", apa_metadata$affiliation, collapse = "\n\n"),
          "\n\n\n# ", apa_metadata$note,
          "\n\n\n# ", apa_lang$abstract,
          "\n\n\n",
          apa_metadata$abstract,
          "\n\n*", apa_lang$keywords, "* ", apa_metadata$keywords,
          "\n\n\n# ", apa_metadata$title, "\n"
        )
        )
      }
    } else warning("Only one call to apa_before_body() can be executed.")
  }

  create_apa_lang()
}

# Defines phrases used throughout the manuscript
localize <- function(x) {
  if(length(x) == 0) x <- "default"
  switch(
    x
    , list( # Default
      abstract = "Abstract"
      , keywords = "Keywords:"
      , table = "Table"
      , figure = "Figure"
      , note = "Note."
    )
    , german = list(
      abstract = "Zusammenfassung"
      , keywords = "Stichwörter:"
      , table = "Tabelle"
      , figure = "Abbildung"
      , note = "Anmerkung."
    )
  )
}

create_apa_lang <- function() {
  requireNamespace("rmarkdown", quietly = TRUE)

  # Run only if document is being rendered
  if(length(knitr::opts_knit$get("rmarkdown.pandoc.to")) > 0) {

    # Only one call allowed while rendering
    if(!exists("apa_lang", where = .GlobalEnv)) {

      # Define document language and lock in global environment
      apa_metadata <- rmarkdown::metadata
      .GlobalEnv$apa_lang <- localize(apa_metadata$lang)
      lockBinding("apa_lang", env = .GlobalEnv)
    }
  } else {
    unlockBinding("apa_lang", env = .GlobalEnv)
    .GlobalEnv$apa_lang <- localize(x)
    lockBinding("apa_lang", env = .GlobalEnv)
  }
}
