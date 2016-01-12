#' Format statistics (APA 6th edition)
#'
#' Takes \code{lm} objects from various statistical methods to create formated chraracter
#' strings to report the results in accordance with APA manuscript guidelines.
#'
#' @param x \code{lm} object.
#' @param est_name Character. If \code{NULL} (default) the name given in \code{x} (or a formally correct
#'    adaptation, such as "\eqn{b^*}" instead of "b" for standardized regression coefficients) is used,
#'    otherwise the supplied name is used. See details.
#' @param standardized Logical. Indicates if coefficients are standardized or unstandardized and leading
#'    zeros are omitted if appropriate. See details.
#' @param ci Numeric. Either a single value (range [0, 1]) giving the confidence level or a two-column
#'    \code{matrix} with confidence region bounds as column names (e.g. "2.5 \%" and "97.5 \%") and
#'    coefficient names as row names (in the same order as they appear in \code{summary(x)$coefficients}.
#'    See details.
#' @param in_paren Logical. Indicates if the formated string will be reported inside parentheses. See details.
#' @param ... Further arguments to pass to \code{\link{printnum}} to format the estimate.
#' @details
#'    The coefficients names are sanitized to facilitate their use as list names (see Value section). Parentheses
#'    are omitted and other non-word characters are replaced by \code{_}.
#'
#'    If \code{in_paren} is \code{TRUE} parentheses in the formated string, such as those surrounding degrees
#'    of freedom, are replaced with brackets.
#'
#'    \code{est_name} is placed in the output string and is thus passed to pandoc or LaTeX through \pkg{kntir}.
#'    Thus, to the extent it is supported by the final document type, you can pass LaTeX-markup to format the final
#'    text (e.g., \code{\\\\beta} yields \eqn{\beta}).
#'
#'    If \code{standardized} is \code{TRUE} "scale()" is removed from coefficients names (see examples).
#'
#'    If \code{ci} is a single value, confidence intervals are calculated using \code{\link{confint}}.
#'
#'    Confidence intervals for \eqn{R^2} are computed using \code{\link[MBESS]{ci.pvaf}} to obtain a confidence
#'    region that corresponds to the \eqn{\alpha}-level chosen for the confidence intervals of parameters (e.g.,
#'    95\% CI or \eqn{\alpha = 0.05} for parameters yields a 90\% CI for \eqn{R^2}, see Steiger, 2004)
#'
#' @return
#'    \code{apa_print.lm} returns a list containing the following components according to the input:
#'
#'    \describe{
#'      \item{\code{stat}}{A named list of character strings giving the test statistic, parameters, and \emph{p}
#'          value for each term.}
#'      \item{\code{est}}{A named list of character strings giving the descriptive estimates and confidence intervals
#'          for each term.} % , either in units of the analyzed scale or as standardized effect size.
#'      \item{\code{full}}{A named list of character strings comprised of \code{est} and \code{stat} for each term.}
#'      \item{\code{table}}{A data.frame containing the complete regression table, which can be passed to \code{\link{apa_table}}.}
#'    }
#'
#' @references
#' Steiger (2004). Beyond the F Test: Effect Size Confidence Intervals and Tests of Close Fit in the Analysis of
#' Variance and Contrast Analysis. \emph{Psychological Methods}, 9(2), 164-182.
#' doi:\href{http://dx.doi.org/10.1037/1082-989X.9.2.164}{10.1037/1082-989X.9.2.164}
#'
#' @family apa_print
#' @seealso \code{\link{confint}}, \code{\link[MBESS]{ci.pvaf}}
#' @examples
#' # Data from Dobson (1990), p. 9.
#' ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
#' trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
#' group <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
#' weight <- c(ctl, trt)
#' lm_fit <- lm(weight ~ group)
#'
#' apa_print(lm_fit)
#'
#' trt <- rep(trt, 2) # More data is allways better
#' ctl <- rep(ctl, 2)
#' lm_fit2 <- lm(scale(trt) ~ scale(ctl))
#' apa_print(lm_fit2, standardized = TRUE)
#' @export

apa_print.lm <- function(
  x
  , est_name = NULL
  , standardized = FALSE
  , ci = 0.95
  , in_paren = FALSE
  , ...
) {

  validate(x, check_class = "lm")
  if(!is.null(est_name)) validate(est_name, check_class = "character", check_length = 1)
  validate(standardized, check_class = "logical", check_length = 1)
  if(!is.null(ci)) {
    if(length(ci) == 1) {
      validate(ci, check_class = "numeric", check_length = 1, check_range = c(0, 1))
      ci <- confint(x, level = ci)
    } else {
      validate(ci, check_class = "matrix")
      sapply(ci, validate, check_class = "numeric")
    }
  } else validate(ci)
  validate(in_paren, check_class = "logical", check_length = 1)

  ellipsis <- list(...)

  # Model coefficients
  if(is.null(est_name)) if(standardized) est_name <- "b^*" else est_name <- "b"
  if(standardized) ellipsis$gt1 <- FALSE

  summary_x <- summary(x)
  tidy_x <- broom::tidy(x)
  tidy_x <- cbind(tidy_x, ci) # Also adds term rownames
  rownames(tidy_x) <- sanitize_terms(tidy_x$term, standardized)
  glance_x <- broom::glance(x)

  # Concatenate character strings and return as named list
  apa_res <- list()
  apa_res$stat <- apply(tidy_x[, -1], 1, function(y) {
    p <- printp(y["p.value"])
    if(!grepl("<|>", p)) eq <- "= " else eq <- ""

    stat <- paste0("$t(", glance_x$df.residual, ") = ",  printnum(y["statistic"]), "$, $p ", eq, p, "$")
    if(in_paren) stat <- in_paren(stat)
    stat
  })

  if(is.matrix(ci)) {
    conf_level <- as.numeric(gsub("[^.|\\d]", "", colnames(ci), perl = TRUE))
    conf_level <- 100 - conf_level[1] * 2
  } else {
    conf_level <- 100 * ci
  }

  apa_res$est <- apply(tidy_x[, -1], 1, function(y) {
    paste0(
      "$", est_name, " = ", do.call(function(...) printnum(y["estimate"], ...), ellipsis), "$, "
      , do.call(function(...) print_confint(y[tail(names(y), 2)], conf_level, ...), ellipsis)
    )
  })

  apa_res$full <- paste(apa_res$est, apa_res$stat, sep = ", ")
  names(apa_res$full) <- names(apa_res$est)

  apa_res <- lapply(apa_res, as.list)

  ## Assamble regression table
  regression_table <- data.frame(tidy_x[, c("term", "estimate", "statistic", "p.value")], row.names = NULL)
  regression_table$ci <- apply(tidy_x[, tail(names(tidy_x), 2)], 1, print_confint, conf_level = NULL) # Don't add "x% CI" to each line
  regression_table <- regression_table[, c("term", "estimate", "ci", "statistic", "p.value")] # Change order of columns
  regression_table$term <- sanitize_terms(regression_table$term, standardized)

  regression_table$estimate <- do.call(function(...) printnum(regression_table$estimate, ...), ellipsis)
  regression_table$statistic <- printnum(regression_table$statistic, digits = 2)
  regression_table$p.value <- printp(regression_table$p.value)

  colnames(regression_table) <- c("Predictor", paste0("$", est_name, "$"), paste0(conf_level, "\\% CI"), paste0("$t(", glance_x$df.residual, ")$"), "$p$")
  apa_res$table <- regression_table

  # Model fit
  p <- printp(glance_x$p.value)
  if(!grepl("<|>", p)) eq <- "= " else eq <- ""

  apa_res$stat$modelfit$r2 <- paste0("$F(", summary_x$fstatistic[2], ", ", glance_x$df.residual, ") = ", printnum(glance_x$statistic), "$, $p ", eq, p, "$") # glance_x$df
  if(in_paren) apa_res$stat$modelfit$r2 <- in_paren(apa_res$stat$modelfit$r2)

  ci_conf_level <- 100 - ((100 - conf_level) * 2)
  # Steiger (2004). Beyond the F Test: Effect Size Confidence Intervals and Tests of Close Fit in the Analysis of Variance and Contrast Analysis.
  # Psychological Methods, 9(2), 164-182. doi: 10.1037/1082-989X.9.2.164
  # See also http://daniellakens.blogspot.de/2014/06/calculating-confidence-intervals-for.html
  gibberish <- capture.output(r2_ci <- MBESS::ci.pvaf(
    F.value = glance_x$statistic
    , df.1 = summary_x$fstatistic[2] # glance_x$df
    , df.2 = glance_x$df.residual
    , N = length(x$residuals)
    , conf.level = ci_conf_level / 100
  ))

  if(!any(is.na(c(r2_ci$Lower, r2_ci$Upper)))) { # MBESS::ci.pvaf can sometimes result in NA if F is really small
    apa_res$est$modelfit$r2 <- paste0("$R^2 = ", printnum(glance_x$r.squared, gt1 = FALSE, zero = FALSE), "$, ", print_confint(c(r2_ci$Lower, r2_ci$Upper), conf_level = ci_conf_level))
  } else {
    apa_res$est$modelfit$r2 <- paste0("$R^2 = ", printnum(glance_x$r.squared, gt1 = FALSE, zero = FALSE), "$")
  }

  apa_res$est$modelfit$r2_adj <- paste0("$R^2_{adj} = ", printnum(glance_x$adj.r.squared, gt1 = FALSE, zero = FALSE), "$")
  apa_res$est$modelfit$aic <- paste0("$AIC = ", printnum(glance_x$AIC), "$")
  apa_res$est$modelfit$bic <- paste0("$BIC = ", printnum(glance_x$BIC), "$")

  apa_res$full$modelfit$r2 <- paste(apa_res$est$modelfit$r2, apa_res$stat$modelfit$r2, sep = ", ")

  apa_res
}


#' @rdname apa_print.lm
#' @method apa_print summary.lm
#' @export

apa_print.summary.lm <- function(x, ...) {
  validate(x, check_class = "summary.lm")

  x <- eval.parent(x$call, n = 1)
  apa_print(x, ...)
}
