#' Format statistics (APA 6th edition)
#'
#' Takes \code{htest} objects from various statistical methods to create
#' formatted chraracter strings to report the results in accordance with APA manuscript guidelines.
#'
#' @param x \code{htest} object. See details.
#' @param stat_name Character. If \code{NULL} (default) the name given in \code{x} (or a formally correct
#'    adaptation, such as \eqn{\chi^2} instead of "x-squared") is used for the \emph{test statistic}, otherwise the
#'    supplied name is used. See details.
#' @param est_name Character. If \code{NULL} (default) the name given in \code{x} (or a formally correct
#'    adaptation, such as \eqn{r_S} instead of "rho") is used for the \emph{estimate}, otherwise the supplied name is
#'    used. See details.
#' @param n Numeric. Size of the sample; required when reporting \eqn{\chi^2} tests, otherwise this parameter
#'    is ignored.
#' @param ci Numeric. If \code{NULL} (default) the function tries to obtain confidence intervals from \code{x}.
#'    Other confidence intervals can be supplied as a \code{vector} of length 2 (lower and upper boundary, respectively)
#'    with attribute \code{conf.level}, e.g., when calculating bootstrapped confidence intervals.
#' @param in_paren Logical. Indicates if the formatted string will be reported inside parentheses. See details.
#' @param ... Further arguments to pass to \code{\link{printnum}} to format the estimate.
#' @details The function should work on a wide range of \code{htest} objects. Due to the large number of functions
#'    that produce these objects and their idiosyncracies, the produced strings may sometimes be inaccurate. If you
#'    experience inaccuracies you may report these \href{https://github.com/crsh/papaja/issues}{here} (please include
#'    a reproducible example in your report!).
#'
#'    \code{stat_name} and \code{est_name} are placed in the output string and are thus passed to pandoc or LaTeX through
#'    \pkg{kntir}. Thus, to the extent it is supported by the final document type, you can pass LaTeX-markup to format the
#'    final text (e.g., \code{\\\\tau} yields \eqn{\tau}).
#'
#'    If \code{in_paren} is \code{TRUE} parentheses in the formatted string, such as those surrounding degrees
#'    of freedom, are replaced with brackets.
#'
#' @return \code{apa_print()} returns a list containing the following components according to the input:
#'
#'    \describe{
#'      \item{\code{stat}}{A character string giving the test statistic, parameters (e.g., degrees of freedom),
#'          and \emph{p} value.}
#'      \item{\code{est}}{A character string giving the descriptive estimates and confidence intervals if possible}
#'          % , either in units of the analyzed scale or as standardized effect size.
#'      \item{\code{full}}{A joint character string comprised of \code{est} and \code{stat}.}
#'    }
#'
#' @family apa_print
#' @examples
#' # Comparisions of central tendencies
#' t_stat <- t.test(extra ~ group, data = sleep)
#' apa_print(t_stat)
#' apa_print(t_stat, stat_name = "tee")
#'
#' wilcox_stat <- wilcox.test(extra ~ group, data = sleep)
#' apa_print(wilcox_stat)
#'
#' # Correlations
#' ## Data from Hollander & Wolfe (1973), p. 187f.
#' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#' y <- c( 2.6,  3.1,  2.5,  5.0,  3.6,  4.0,  5.2,  2.8,  3.8)
#' cor_stat <- cor.test(x, y, method = "spearman")
#' apa_print(cor_stat)
#'
#' # Contingency tables
#' ## Data from Fleiss (1981), p. 139.
#' smokers  <- c(83, 90, 129, 70)
#' patients <- c(86, 93, 136, 82)
#' prop_stat <- prop.test(smokers, patients)
#' apa_print(prop_stat, n = sum(patients))
#' @export

apa_print.htest <- function(
  x
  , stat_name = NULL
  , est_name = NULL
  , n = NULL
  , ci = NULL
  , in_paren = FALSE
  , ...
) {
  validate(x, check_class = "htest")
  if(!is.null(stat_name)) validate(stat_name, check_class = "character", check_length = 1)
  if(!is.null(est_name)) validate(est_name, check_class = "character", check_length = 1)
  if(!is.null(n)) validate(n, check_class = "numeric", check_integer = TRUE, check_range = c(0, Inf), check_length = 1)
  if(!is.null(ci)) validate(ci, check_class = "matrix", check_length = 2)
  validate(in_paren, check_class = "logical", check_length = 1)

  ellipsis <- list(...)

  if(is.null(stat_name) & !is.null(names(x$statistic))) {
    stat_name <- names(x$statistic)
    stat_name <- convert_stat_name(stat_name)
  }
  stat <- printnum(x$statistic)

  if(!is.null(x$sample.size)) n <- x$sample.size

  if(!is.null(x$parameter)) {
    # Statistic and degrees of freedom
    if(tolower(names(x$parameter)) == "df") {
      if(x$parameter %%1 == 0) printdigits <- 0 else printdigits = 2
      if(stat_name == "\\chi^2") {
        if(is.null(x$sample.size) & is.null(n)) stop("Please provide the sample size to report.") # Demand sample size information if it's a Chi^2 test
        stat_name <- paste0(stat_name, "(", printnum(x$parameter[grep("df", names(x$parameter), ignore.case = TRUE)], digits = printdigits), ", n = ", n, ")")
      } else {
        stat_name <- paste0(stat_name, "(", printnum(x$parameter[grep("df", names(x$parameter), ignore.case = TRUE)], digits = printdigits), ")")
      }
    }
  }

  # p-value
  p <- printp(x$p.value)
  if(!grepl("<|>", p)) eq <- "= " else eq <- ""

  apa_res <- list()
  apa_res$stat <- paste0("$", stat_name, " = ", stat, "$, $p ", eq, p, "$")
  if(in_paren) apa_res$stat <- in_paren(apa_res$stat)

  # Estimate
  if(is.null(est_name) & !is.null(names(x$estimate))) {
    est_name <- convert_stat_name(names(x$estimate))
  }

  if(is.null(x$estimate)) {
    est <- NULL
  } else {
    if(is.null(est_name)) {
      warning("Cannot determine name of estimate supplied in ", deparse(substitute(x)), " of class 'htest'. Estimate is omitted from output string. Please set parameter 'est_name' to obtain an estimate.")
      est <- NULL
    } else if(!is.null(names(x$estimate)) && convert_stat_name(names(x$estimate)) == "\\Delta M") {
      est <- do.call(function(...) printnum(diff(x$estimate), ...), ellipsis)
    } else if(length(x$estimate) == 1) {
      if(est_name %in% c("r", "r_{\\mathrm{s}}", "\\uptau") & is.null(ellipsis$gt1)) ellipsis$gt1 <- FALSE
      est <- do.call(function(...) printnum(x$estimate, ...), ellipsis)
    }
  }

  if(!is.null(est)) {
    if(!grepl("<|>", est)) eq <- " = " else eq <- ""

    if(is.null(ci) && !is.null(x$conf.int)) { # Use CI in x
      apa_res$est <- paste0("$", est_name, eq, est, "$, ", do.call(function(...) print_confint(x$conf.int, ...), ellipsis))
    } else if(!is.null(ci)) { # Use supplied CI
      ellipsis$margin <- 2 # Ignore margin argument passed by user
      apa_res$est <- paste0("$", est_name, eq, est, "$, ", do.call(function(...) print_confint(ci, ...), ellipsis))
    } else if(is.null(ci) && is.null(x$conf.int)) { # No CI
      apa_res$est <- paste0("$", est_name, eq, est, "$")
    }

    apa_res$full <- paste(apa_res$est, apa_res$stat, sep = ", ")
  }

  apa_res
}
