
#' Fuse Degrees-of-Freedom Columns into Variable Labels
#'
#' Takes the output from [apa_print()] methods and modifies the results table
#' by fusing information about degrees of freedom into the variable labels of
#' test-statistic columns.
#'
#' @param x    Either the complete output object created by [apa_print()] methods,
#'   or the `table` component of such objects.
#' @param quietly Logical. If FALSE (the default), checks if df columns contain non-integer values.
#' @param ... further arguments passed from an to other methods
#'
#' @examples
#'   apa_out <- apa_print(aov(yield ~ N * P, npk))
#'
#'   # Standard output with separate columns for degrees of freedom:
#'   apa_out
#'
#'   # Modified output where degrees of freedom are incorporated into the variable
#'   # label of column 'statistic':
#'   fuse_df(apa_out)
#'
#'
#' @rdname fuse_df
#' @export

fuse_df <- function(x, quietly = FALSE, ...) {
  UseMethod("fuse_df")
}



#' @rdname fuse_df
#' @export
fuse_df.apa_results <- function(x, quietly = FALSE, ...) {
  x$table <- fuse_df(x$table, quietly = quietly, ...)
  x
}



#' @rdname fuse_df
#' @export

fuse_df.apa_results_table <- function(x, quietly = FALSE, ...) {

  quietly <- isTRUE(quietly)

  if(is.null(x$df) && is.null(x$df.residual)) {
    if(!quietly) message("There are no df columns to fuse. Returning original input.")
    return(x)
  }

  df_columns <- intersect(colnames(x), c("df", "df.residual", "multivariate.df", "multivariate.df.residual"))

  for (i in df_columns) {
    if( length(unique(x[[i]])) > 1L ) {
      stop("Cannot fuse columns into variable label:\n Degrees of freedom (in column ", encodeString(i, quote = "'"), ") vary by model term.", call. = FALSE)
    }
  }

  if(!quietly) {
    for (i in df_columns) {
      if( length(x[[i]]) > 1L && any(as.numeric(x[[i]]) %% 1 != 0) ) {
        warning("Column 'df' contains non-integer values. Please check if these values are really equal to each other before fusing them.")
      }
    }
  }

  tinylabels::variable_label(x$statistic) <- paste0(
    "$"
    , strip_math_tags(tinylabels::variable_label(x$statistic))
    , "("
    , paste(c(x$df[[1L]], x$df.residual[[1L]]), collapse = ", ")
    , ")$"
  )
  if(any(colnames(x) == "multivariate.statistic")) {
    tinylabels::variable_label(x$multivariate.statistic) <- paste0(
      "$"
      , strip_math_tags(tinylabels::variable_label(x$multivariate.statistic))
      , "("
      , paste(c(x$multivariate.df[[1L]], x$multivariate.df.residual[[1L]]), collapse = ", ")
      , ")$"
    )
  }

  x$df <- x$df.residual <- x$multivariate.df <- x$multivariate.df.residual <- NULL
  x
}
