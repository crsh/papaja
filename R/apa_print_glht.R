#' Format statistics (APA 6th edition)
#'
#' Takes \code{glht} objects to create formatted character strings to report the results in
#' accordance with APA manuscript guidelines.  \emph{These methods are not properly tested and should be
#' considered experimental.}
#'
#' @param x Object
#' @param test Function. Computes p-values (adjusted for multiple comparisons).
#' @param ci Numeric. If \code{NULL} (default) the function tries to obtain confidence intervals from \code{x}.
#'    Other confidence intervals can be supplied as a \code{vector} of length 2 (lower and upper boundary, respectively)
#'    with attribute \code{conf.level}, e.g., when calculating bootstrapped confidence intervals.
#' @param in_paren Logical. Indicates if the formated string will be reported inside parentheses.
#' @inheritDotParams printnum
#' @details
#'
#'    If \code{in_paren} is \code{TRUE} parentheses in the formated string, such as those surrounding degrees
#'    of freedom, are replaced with brackets.
#'
#' @return \code{apa_print()} returns a list containing the following components according to the input:
#'
#'    \describe{
#'      \item{\code{statistic}}{A character string giving the test statistic, parameters (e.g., degrees of freedom),
#'          and \emph{p} value.}
#'      \item{\code{estimate}}{A character string giving the descriptive estimates and confidence intervals if possible}
#'          % , either in units of the analyzed scale or as standardized effect size.
#'      \item{\code{full_result}}{A joint character string comprised of \code{est} and \code{stat}.}
#'      \item{\code{table}}{A data.frame containing the complete contrast table, which can be passed to \code{\link{apa_table}}.}
#'    }
#'
#' @family apa_print
#' @examples
#'    NULL
#' @export

apa_print.glht <- function(x, test = multcomp::adjusted(), ...) {
  summary_x <- summary(x, test = test)

  apa_print(summary_x, ...)
}

#' @rdname apa_print.glht
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
  conf_level <- paste0(ci * 100, "\\% CI")

  # Assamble table
  ## Add (adjusted) confidence intervall
  multcomp_adjustment <- if(x$test$type == "none") multcomp::univariate_calpha() else multcomp::adjusted_calpha()
  print_ci <- stats::confint(x, level = ci, calpha = multcomp_adjustment)$confint
  dimnames(print_ci) <- NULL
  table_ci <- unlist(print_confint(print_ci[, -1], ...)) # Remove point estimate from matrix
  contrast_table <- cbind(estimate = tidy_x$estimate, confint = table_ci, tidy_x[, c("statistic", "p.value")])
  rownames(contrast_table) <- sanitize_terms(tidy_x$lhs)

  ## Format numbers
  contrast_table$estimate <- printnum(contrast_table$estimate, ...)
  contrast_table$statistic <- printnum(contrast_table$statistic, digits = 2)
  contrast_table$p.value <- printp(contrast_table$p.value)

  # Concatenate character strings and return as named list
  apa_res <- apa_print_container()

  apa_res$estimate <- apply(contrast_table, 1, function(y) {
    paste0("$\\Delta M = ", y["estimate"], "$, ", conf_level, " ", y["confint"])
  })

  apa_res$statistic <- apply(contrast_table, 1, function(y) {
    if(!grepl("<|>", y["p.value"])) eq <- "= " else eq <- ""

    paste0("$", test_stat, " = ", y["statistic"], "$, $p ", eq, y["p.value"], "$")
  })

  apa_res$full_result <- paste(apa_res$estimate, apa_res$stat, sep = ", ")
  names(apa_res$full_result) <- names(apa_res$estimate)

  apa_res <- lapply(apa_res, as.list)

  # Add table
  rownames(contrast_table) <- tidy_x$lhs
  colnames(contrast_table) <- c("estimate", "ci", "statistic", "p.value")
  variable_label(contrast_table) <- c(
    estimate = "$\\Delta M$"
    , ci = conf_level
    , statistic = paste0("$", test_stat, "$")
    , p.value = "$p$"
  )
  apa_res$table <- contrast_table
  attr(apa_res$table, "class") <- c("apa_results_table", "data.frame")
  apa_res
}
