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


#' Extract Parts of an APA Results Table
#'
#' \emph{These methods are only defined for backward compatibility with older
#' versions of \pkg{papaja}}. In the past, the column names of`apa_results_table`s
#' were less standardized than they are today. In order to maintain backwards
#' compatibility, it is still possible to extract columns with the old columns names,
#' because we here provide *aliased* indexing. Note that aliased indexing will be
#' defunct in a future release of \pkg{papaja}.
#'
#' @inheritParams base::Extract
#' @rdname extract_apa_results_table
#' @md
#' @export

`$.apa_results_table` <- function(x, name) {

  aliases <- c(
    "F"         = "statistic"
    , "t"         = "statistic"
    # , "p"         = "p.value"  # with partial matching, this is okay
    , "Predictor" = "term"
    , "Effect"    = "term"
  )
  new_name <- aliases[pmatch(name, names(aliases), nomatch = 0L)]

  if(length(new_name) > 0L) {
    warning(
      "Indexing an apa_results_table with `$"
      , name
      , "` is deprecated. Use `$"
      , unname(new_name)
      , "` instead."
      , call. = FALSE
    )
    name <- new_name
  }
  NextMethod()
}

#' @rdname extract_apa_results_table
#' @export

`[[.apa_results_table` <- function(x, i, exact = TRUE) {

  if(missing(i) || is.null(i) || is.na(i)) NextMethod()

  aliases <- c(
    "F"         = "statistic"
    , "t"         = "statistic"
    , "p"         = "p.value"
    , "Predictor" = "term"
    , "Effect"    = "term"
  )
  if(!exact) {
    aliases <- aliases[-3] # okay if exact == FALSE
    new_name <- aliases[pmatch(i, names(aliases), nomatch = 0L)]
  } else {
    new_name <- aliases[intersect(i, names(aliases))]
  }

  if(length(new_name) > 0L) {
    warning("Indexing an apa_results_table with '[[\""
            , i
            , "\"]]' is deprecated. Use '[[\""
            , unname(new_name)
            , "\"]]' instead.", call. = FALSE)
    return(x[[new_name, exact = exact]])
  }
  NextMethod()
}

#' @rdname extract_apa_results_table
#' @export

`[.apa_results_table` <- function(x, i, j, ..., drop = TRUE) {

  if(missing(j) || is.null(j) || is.na(j)) {
    NextMethod()
  } else {

    aliases <- c(
      "F"         = "statistic"
      , "t"         = "statistic"
      , "p"         = "p.value"
      , "Predictor" = "term"
      , "Effect"    = "term"
    )
    if(any(j %in% names(aliases))) {
      j_change <- j %in% names(aliases)
      prepend <- append <- "\""
      if(sum(j_change) > 1) {
        prepend <- "c(\""
        append <- "\")"
      }
      warning("Indexing an apa_results_table with '[..., "
              ,  paste0(prepend, paste(j[j_change], collapse = "\", \""), append)
              , "]' is deprecated. Use '[..., "
              , paste0(prepend, paste(aliases[j[j_change]], collapse = "\", \""), append)
              , "]' instead.", call. = FALSE)

      j[j_change] <- aliases[j[j_change]]
    }
  }
  NextMethod()
}
