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
    max_shown <- ifelse(nrow(x_legend) == 6L, 6L, 5L)
    x_legend <- x_legend[seq_len(min(max_shown, n_labels)), , drop = FALSE]

    x_legend$column <- encodeString(x_legend$column, width = NA)
    x_legend$label <- encodeString(x_legend$label)

    apply(
      x_legend
      , MARGIN = 1
      , function(x) {
        cat("\n", x["column"], ": ", sep = "")
        if(!is.na(x["label"])) cat(x["label"], " ", sep = "")
        if(!is.na(x["unit"])) cat("[", encodeString(x["unit"]), "]", sep = "")
      }
    )

    if(n_labels > max_shown) cat("\n... (", n_labels - max_shown, " more label", if(n_labels > (max_shown + 1L)) "s" else NULL, ")", sep = "")
  }

  invisible(x)
}


#' #' Extract or Replace Columns of an APA Results Table
#' #'
#' #' blabla
#' #'
#' #' @rdname extract_apa_results_table
#' #' @export
#'
#' `$.apa_results_table` <- function(x, name) {
#'
#'   aliases <- c(
#'     "F"         = "statistic"
#'     , "t"         = "statistic"
#'     , "p"         = "p.value"
#'     , "Predictor" = "term"
#'     , "Effect"    = "term"
#'     , "term"      = "term"
#'   )
#'   name <- c(aliases, colnames(x))[pmatch(name, c(names(aliases), colnames(x)))]
#'
#'   if(names(name) %in% names(aliases)) {
#'     message("Indexing an apa_results_table with `$"
#'             , names(name)
#'             , "` is deprecated. Use `$"
#'             , name
#'             , "` instead.")
#'   }
#'   x[[name]]
#' }

