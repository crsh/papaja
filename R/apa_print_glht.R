#' Typeset Statistical Results from General Linear Hypothesis Tests
#'
#' \emph{These methods are not properly tested and should be
#' considered experimental.}
#'
#' @param x Object
#' @param test Function. Computes p-values (adjusted for multiple comparisons).
#' @param conf.int Numeric. If \code{NULL} (default) the function tries to obtain confidence intervals from \code{x}.
#'    Other confidence intervals can be supplied as a \code{vector} of length 2 (lower and upper boundary, respectively)
#'    with attribute \code{conf.level}, e.g., when calculating bootstrapped confidence intervals.
# #' @param contrast_names Character. A vector of names to identify calculated contrasts.
#' @param ... Further arguments to pass to \code{\link{printnum}} to format the estimate.
#' @inheritParams glue_apa_results
#'
#' @evalRd apa_results_return_value()
#'
#' @family apa_print
#' @examples
#'    # From the multcomp::glht() examples:
#'    library(multcomp)
#'    amod <- aov(breaks ~ tension, data = warpbreaks)
#'    glht_out <- glht(amod, linfct = mcp(tension = "Tukey"))
#'    apa_print(glht_out)
#'
#'   # In this example, because degrees of freedom are equal across all rows
#'   # of the output, it is possible to move that information to the variable
#'   # labels. This is useful if a compact results table is required:
#'
#'   df_into_label(apa_print(glht_out))
#' @method apa_print glht
#' @export

apa_print.glht <- function(x, test = multcomp::adjusted(), ...) {
  summary_x <- summary(x, test = test)

  apa_print(summary_x, ...)
}

#' @rdname apa_print.glht
#' @method apa_print summary.glht
#' @export

apa_print.summary.glht <- function(
  x
  , conf.int = 0.95
  , in_paren = FALSE
  , ...
) {
  ellipsis_ci <- deprecate_ci(conf.int, ...)
  ellipsis <- ellipsis_ci$ellipsis
  conf.int <- ellipsis_ci$conf.int

  validate(x, check_class = "summary.glht")
  validate(conf.int, check_class = "numeric", check_length = 1, check_range = c(0, 1))
  validate(in_paren, check_class = "logical", check_length = 1)

  tidy_x <- broom::tidy(x)
  test_stat <- if(x$df == 0) "z" else "t"
  conf_level <- paste0(conf.int * 100, "% CI")
  p_value <- names(tidy_x)[grepl("p.value", names(tidy_x), fixed = TRUE)]

  # Assemble table
  ## Add (adjusted) confidence interval
  multcomp_adjustment <- if(x$test$type == "none") multcomp::univariate_calpha() else multcomp::adjusted_calpha()
  print_ci <- stats::confint(x, level = conf.int, calpha = multcomp_adjustment)$confint
  dimnames(print_ci) <- NULL
  table_ci <- unlist(do.call("print_confint", c(list(x = print_ci[, -1]), ellipsis))) # Remove point estimate from matrix
  tidy_x$std.error <- table_ci
  colnames(tidy_x)[colnames(tidy_x) == "std.error"] <- "conf.int"



  ## Typeset columns
  sanitized_contrasts <- sanitize_terms(tidy_x$contrast)
  tidy_x$contrast <- beautify_terms(tidy_x$contrast)
  tidy_x$estimate <- do.call("printnum", c(list(x = tidy_x$estimate), ellipsis))
  tidy_x$statistic <- printnum(tidy_x$statistic, digits = 2)
  tidy_x[[p_value]] <- printp(tidy_x[[p_value]])

  if(x$df != 0) {
    tidy_x$df <- print_df(x$df)
    tidy_x <- tidy_x[, c("contrast", "null.value", "estimate", "conf.int", "statistic", "df", p_value)] # sort columns
    variable_label(tidy_x$df) <- "$\\mathit{df}$"
  }

  ## Add variable labels
  variable_labels(tidy_x) <- c(
    contrast = "Contrast"
    , null.value = "$\\mu_0$"
    , estimate = "$\\Delta M$"
    , conf.int = conf_level
    , statistic = paste0("$", test_stat, "$")
  )
  variable_labels(tidy_x[[p_value]]) <- if(p_value == "p.value") "$p$" else if(p_value == "adj.p.value") "$p_{adj}$"

  if(all(tidy_x$null.value == 0)) tidy_x$null.value <- NULL

  class(tidy_x) <- c("apa_results_table", "data.frame")

  glue_apa_results(
    tidy_x
    , est_glue = construct_glue(tidy_x, "estimate")
    , stat_glue = construct_glue(tidy_x, "statistic")
    , term_names = sanitized_contrasts
    , in_paren = in_paren
  )
}
