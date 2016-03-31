# Create environment to store metadata
apa_doc_env <- new.env()

# Create localization object in apa_doc_env
create_apa_lang <- function(lang) {
  if(is.null(lang)) lang <- "american"

  # Run only if document is being rendered
  if(length(knitr::opts_knit$get("rmarkdown.pandoc.to")) > 0) {

    # Only one call allowed while rendering
    if(!exists("apa_lang", envir = apa_doc_env, inherits = FALSE)) {

      # Define document language and lock in global environment
      assign("apa_lang", localize(lang), envir = apa_doc_env)
    }
  } else {
    if(exists("apa_lang", envir = apa_doc_env, inherits = FALSE)) {
    }
    apa_doc_env$apa_lang <- localize(lang)
  }
}

# Defines phrases used throughout the manuscript
localize <- function(lang) {
  switch(
    lang
    , list( # Default
      author_note = "Author note"
      , abstract = "Abstract"
      , keywords = "Keywords:"
      , word_count = "Word count:"
      , table = "Table"
      , figure = "Figure"
      , note = "Note"
    )
    , german = list(
      author_note = "Anmerkung des Autors"
      , abstract = "Zusammenfassung"
      , keywords = "Schlüsselwörter:"
      , word_count = "Wortanzahl:"
      , table = "Tabelle"
      , figure = "Abbildung"
      , note = "Anmerkung"
    )
  )
}
