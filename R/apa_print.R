#' Format statistics (APA 6th edition)
#'
#' Formats output objects from various statistical methods to create formated chraracter strings to report
#' the results in accordance with APA manuscript guidelines.
#'
#' @param x Output object. See details.
#' @param stat_name Character. Name of the reported statistic; if \code{NULL} the name specified in the
#'    supplied object is used.
#' @param n Numeric. Size of the sample. Needed when reporting $\chi^2$, otherwise this parameter is
#'    ignored.
#' @param standardized Logical. If \code{x} is an object of class \code{summary.lm} indicates if
#'    coefficients are standardized or unstandardized; otherwise ignored.
#' @param ci Numeric. Either a \code{vector} of length 2 with attribute \code{conf.level} or a two-column
#'    \code{matrix} and confidence region bounds as column names (e.g. "2.5 %" and "97.5 %") and coefficient
#'    names as row names. If \code{x} is of class \code{htest} and \code{ci = NULL} the function tries
#'    to obtain confidence intervals from \code{x}.
#' @param in_paren Logical. Indicates if the formated string be reported inside parentheses. See details.
#' @details Currently the following output objects are supported:
#'
#'    \itemize{
#'      \item \code{htest}
#'      \item \code{lm} and \code{summary.lm}
#'      \item \code{anova}
#'    }
#'
#'    If \code{in_paren} is \code{TRUE} parentheses in the formated string, such as those surrounding degrees
#'    of freedom, are replaced with brackets.
#'
#'    `apa_print()` is a generic function.
#' @return `apa_print()` returns a list containing the following components according to the input:
#'
#'    \describe{
#'      \item{\code{stat}}{A character string giving the test statistic, parameters, and \emph{p} value.}
#'      \item{\code{est}}{A character string giving the descriptive estimates, either in units of the analyzed
#'      scale or as standardized effect size.}
#'      \item{\code{full}}{A character string comprised of \code{est} and \code{stat}.}
#'    }
#'
#' @examples
#' t_stat <- t.test(extra ~ group, data = sleep)
#' apa_print(t_stat)
#'
#' cor_stat <- cor.test(iris$Sepal.Length, iris$Sepal.Width)
#' apa_print(cor_stat)
#'
#' iris_lm <- lm(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
#' apa_print(summary(iris_lm), ci = confint(iris_lm))
#'
#' iris_lm2 <- update(iris_lm, formula = . ~ + Sepal.Length:Sepal.Width)
#' apa_print(anova(iris_lm2, iris_lm))
#' @export

apa_print <- function(x, ...) {

  if(is.null(x)) stop("The parameter 'x' is NULL. Please provide a value for 'x'.")

#   if(!is.null(stat_name)) validate(stat_name, check_class = "character", check_length = 1)
#   if(!is.null(n)) validate(n, check_class = "numeric", check_integer = TRUE, check_range = c(0, Inf), check_length = 1)
#   validate(standardized, check_class = "logical", check_length = 1)
#   validate(in_paren, check_class = "logical", check_length = 1)

  UseMethod("apa_print", x)
}

apa_print.default <- function(x, ...) {
  stop(paste0("Objects of class '", class(x), "' are currently not supported (no method defined).
              Visit https://github.com/crsh/papaja/issues to request support for this class."))
}
