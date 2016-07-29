.onLoad <- function(libname, pkgname) { # nocov start
  lang <- if(length(knitr::opts_knit$get("rmarkdown.pandoc.to")) > 0) {
    rmarkdown::metadata$lang
  } else "english"

  op <- options()
  op_papaja <- list(
    papaja.language = lang
    , papaja.terms = localize(lang)
    , papaja.plot_theme = "greyscale"
    , papaja.mse = TRUE
    , papaja.sphericity_correction = "GG"
  )
  toset <- !(names(op_papaja) %in% names(op))
  if(any(toset)) options(op_papaja[toset])

  invisible()
} # nocov end
