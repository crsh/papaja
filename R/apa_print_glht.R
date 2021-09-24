#' Typeset Statistical Results from General Linear Hypothesis Tests
#'
#' \emph{These methods are not properly tested and should be
#' considered experimental.}
#'
#' @param x Object
#' @param test Function. Computes p-values (adjusted for multiple comparisons).
#' @param ci Numeric. If \code{NULL} (default) the function tries to obtain confidence intervals from \code{x}.
#'    Other confidence intervals can be supplied as a \code{vector} of length 2 (lower and upper boundary, respectively)
#'    with attribute \code{conf.level}, e.g., when calculating bootstrapped confidence intervals.
# #' @param contrast_names Character. A vector of names to identify calculated contrasts.
#' @param ... Further arguments to pass to \code{\link{printnum}} to format the estimate.
#' @inheritParams glue_apa_results
#'
#' @evalRd apa_resutls_return_value()
#'
#' @family apa_print
#' @examples
#'    NULL
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
  , ci = 0.95
  , in_paren = FALSE
  , ...
) {
  validate(x, check_class = "summary.glht")
  validate(ci, check_class = "numeric", check_length = 1, check_range = c(0, 1))
  validate(in_paren, check_class = "logical", check_length = 1)

  tidy_x <- broom::tidy(x)
  test_stat <- ifelse(x$df == 0, "z", paste0("t(", x$df, ")"))
  conf_level <- paste0(ci * 100, "% CI")
  p_value <- names(tidy_x)[grepl("p.value", names(tidy_x), fixed = TRUE)]

  # Assamble table
  ## Add (adjusted) confidence intervall
  multcomp_adjustment <- if(x$test$type == "none") multcomp::univariate_calpha() else multcomp::adjusted_calpha()
  print_ci <- stats::confint(x, level = ci, calpha = multcomp_adjustment)$confint
  dimnames(print_ci) <- NULL
  table_ci <- unlist(print_confint(print_ci[, -1], ...)) # Remove point estimate from matrix
  tidy_x$std.error <- table_ci
  colnames(tidy_x)[colnames(tidy_x) == "std.error"] <- "conf.int"

  ## Typeset columns
  sanitzied_contrasts <- sanitize_terms(tidy_x$contrast)
  tidy_x$contrast <- prettify_terms(tidy_x$contrast)
  tidy_x$estimate <- printnum(tidy_x$estimate, ...)
  tidy_x$statistic <- printnum(tidy_x$statistic, digits = 2)
  tidy_x[[p_value]] <- printp(tidy_x[[p_value]])

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
    , term_names = sanitzied_contrasts
    , in_paren = in_paren
  )
}
