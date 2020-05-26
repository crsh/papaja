#' @export

print.apa_results_table <- function(x, ...) {
  column_labels <- variable_label(x)
  n_labels <- length(unlist(column_labels))

  if(n_labels == 0) {
    base::print.data.frame(x, ...)
  } else {
    cat("A data.frame with ", n_labels, " labelled column", if(n_labels > 1) "s" else NULL, ":\n\n", sep = "")

    base::print.data.frame(x, ...)

    x_labels <- unlist(
      lapply(
        column_labels
        , function(x) if(is.null(x)) NA else x
      )
    )
    x_units <- unlist(
      lapply(
        x
        , function(x) if(is(x, "annotated_vector") && length(x@annotation@unit) > 0) x@annotation@unit else NA
      )
    )

    x_legend <- data.frame(
      column = colnames(x)
      , label = x_labels
      , unit = x_units
      , stringsAsFactors = FALSE
    )
    x_legend <- x_legend[!is.na(x_legend$label) | !is.na(x_legend$unit), ]
    max_char <- max(nchar(x_legend$column))

    apply(
      x_legend[1:min(5, nrow(x_legend)), ]
      , 1
      , function(x) {
        cat("\n", format(x["column"], width = max_char), ": ", sep = "")
        if(!is.na(x["label"])) cat(x["label"], " ", sep = "")
        if(!is.na(x["unit"])) cat("[", x["unit"], "]", sep = "")
      }
    )

    if(n_labels > 5) cat("\n... (", n_labels - 5, " more label", if(n_labels > 6) "s" else NULL, ")", sep = "")
  }

  invisible(x)
}


#' #' @export
#'
#' `$.apa_results_table` <- function(x, name) {
#'
#'   aliases <- list(
#'     "F" = "statistic"
#'     # , "p" = "p.value" # no, because partical matching solves this issue
#'     , "predictor" = "term"
#'   )
#'   pmatch(name, c(names(aliases), colnames(x)))
#'
#'   if(name %in% names(aliases)) {
#'     message("Indexing an apa_results_table with `$"
#'     , name
#'     , "` is deprecated. Use `$"
#'     , aliases[name]
#'     , "` instead.")
#'     name <- aliases[name]
#'   }
#'   NextMethod("$", object = x, name = name) # dispatch to data.frame method
#' }
