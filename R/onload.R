.onLoad <- function(libname, pkgname) { # nocov start
  lang <- if(length(knitr::opts_knit$get("rmarkdown.pandoc.to")) > 0 && !is.null(rmarkdown::metadata$lang)) {
    rmarkdown::metadata$lang
  } else "english"

  op <- options()
  op_papaja <- list(
    papaja.language = lang
    , papaja.terms = localize(lang)
    , papaja.na_string = "NA"
    , papaja.plot_colors = "greyscale"
    , papaja.mse = TRUE
    , papaja.sphericity_correction = "GG"
  )
  toset <- !(names(op_papaja) %in% names(op))
  if(any(toset)) options(op_papaja[toset])


  # Fix dplyr R CMD Check note
  if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

  invisible()
} # nocov end
