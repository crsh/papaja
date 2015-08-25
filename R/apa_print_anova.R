#' Format statistics from ANOVA (APA 6th edition)
#'
#' These methods take objects from various R functions that calculate ANOVA to create formated chraracter
#' strings to report the results in accordance with APA manuscript guidelines. \code{anova}-objects from e.g. model comparisons are currently
#' only supported for \code{lm}-objects.
#'
#' @param x Output object. See details.
#' @param es Character. The effect-size measure to be calculated; can be either \code{ges} for generalized eta-squared or \code{pes} for partial eta-squared.
#' @param observed Character. The names of the factors that are observed, (i.e., not manipulated). Necessary for calculation of generalized eta-squared; otherwise ignored.
#' @param correction Character. In the case of repeated-measures ANOVA, the type of sphericity correction to be used. Either \code{GG} for Greenhouse-Geisser or \code{HF} for Huyn-Feldt methods or \code{none} is also possible.
#' @param in_paren Logical. Indicates if the formated string will be reported inside parentheses. See details.
#' @param models List. List containing fitted \code{lm}- objects that were compared using \code{anova()}. If the list is named, element names are used as model names in the ouptut object.
#' @param ci Numeric. Confidence level for the confidence interval for \eqn{\Delta R^2} if \code{x} is a model comparison object of class \code{anova}. If \code{ci = NULL} no confidence intervals are estimated.
#' @param boot_samples Numeric. Number of bootstrap samples to estimate confidence intervals for \eqn{\Delta R^2} if \code{x} is a model comparison object of class \code{anova}; ignored if \code{ci = NULL}.
#' @param ... Additional arguments passed to or from other methods.
#' @details
#'    Currently, methods for the following objects are available:
#'    \itemize{
#'      \item{\code{aov}}
#'      \item{\code{summary.aov}}
#'      \item{\code{aovlist}}
#'      \item{\code{summary.aovlist}}
#'      \item{\code{anova}}
#'      \item{\code{Anova.mlm}}
#'    }
#'
#'    The factor names are sanitized to facilitate their use as list names (see Value section). Parentheses
#'    are omitted and other non-word characters are replaced by \code{_}.
#'
#'    If \code{in_paren} is \code{TRUE} parentheses in the formated string, such as those surrounding degrees
#'    of freedom, are replaced with brackets.
#'
#'    As demonstrated by Algina, Keselman & Penfield (2007), asymptotic confidence intervals for \eqn{\Delta R^2}
#'    are often unreliable. Confidence intervals for model comparisons of \code{lm}-objects are, therefore, estimated
#'    using their modified percentile bootstrap method. Note that the accuracy of the confidence intervals depends on
#'    the number of predictors \eqn{p}, their distribution, and the sample size \eqn{n}:
#'
#'    \emph{"When the predictor distribution is multivariate normal, one can obtain accurate CIs for \eqn{\rho^2} with
#'    \eqn{n \geq 50} when \eqn{p = 3}. For \eqn{p = 6} and for \eqn{p = 9}, \eqn{n \geq 100} is advisable. When the
#'    predictor distribution is nonnormal in form, sample size requirements vary with type of nonnormality." (p. 939)}
#' @return
#'    \code{apa_print.aov} and related functions return a named list containing the following components according to the input:
#'
#'    \describe{
#'      \item{\code{stat}}{A named list of character strings giving the test statistic, parameters, and \emph{p}
#'          value for each factor.}
#'      \item{\code{est}}{A named list of character strings giving the effect size estimates for each factor.} % , either in units of the analyzed scale or as standardized effect size.
#'      \item{\code{full}}{A named list of character strings comprised of \code{est} and \code{stat} for each factor.}
#'      \item{\code{table}}{A data.frame containing the complete ANOVA table, which can be passed to \code{\link{apa_table}}.}
#'    }
#' @references
#'    Algina, J., Keselman, H. J., & Penfield, R. D. (2007). Confidence Intervals for an Effect Size Measure in Multiple Linear Regression.
#'    \emph{Educational and Psychological Measurement}, 67(2), 207--218. doi:\href{http://dx.doi.org/10.1177/0013164406292030}{10.1177/0013164406292030}
#'
#'    Algina, J., Keselman, H. J., & Penfield, R. D. (2010). Confidence Intervals for Squared Semipartial Correlation Coefficients: The Effect of Nonnormality.
#'    \emph{Educational and Psychological Measurement}, 70(6), 926--940. doi:\href{http://dx.doi.org/10.1177/0013164410379335}{10.1177/0013164410379335}
#'
#'    Bakeman, R. (2005). Recommended effect size statistics for repeated measures designs. \emph{Behavior Research Methods}
#'    , 37 (3), 379--384. doi:\href{http://dx.doi.org/10.3758/BF03192707}{10.3758/BF03192707}
#'
#' @family apa_print
#' @seealso \code{\link{aov}}, \code{\link[car]{Anova}}
#' @examples
#'    ## From Venables and Ripley (2002) p. 165.
#'    npk_aov <- aov(yield ~ block + N * P * K, npk)
#'    apa_print(npk_aov)
#' @export

apa_print.aov <- function(x, ...) {
  summary_x <- summary(x)

  apa_print(summary_x, ...)
}


#' @rdname apa_print.aov
#' @method apa_print summary.aov
#' @export

apa_print.summary.aov <- function(x, ...) {
  variance_table <- arrange_anova(x)

  print_anova(variance_table, ...)
}


#' @rdname apa_print.aov
#' @method apa_print aovlist
#' @export

apa_print.aovlist <- function(x, ...) {
  summary_x <- summary(x)

  apa_print(summary_x, ...)
}


#' @rdname apa_print.aov
#' @method apa_print summary.aovlist
#' @export

apa_print.summary.aovlist <- function(x, ...) {
  variance_table <- arrange_anova(x)

  print_anova(variance_table, ...)
}


#' @rdname apa_print.aov
#' @method apa_print Anova.mlm
#' @export

apa_print.Anova.mlm <- function(x, correction = "GG", ...) {
  summary_x <- summary(x, multivariate = FALSE) # car:::summary.Anova.mlm

  apa_print(summary_x, correction = correction, ...)
}


#' @rdname apa_print.aov
#' @method apa_print summary.Anova.mlm
#' @export

apa_print.summary.Anova.mlm <- function(x, correction = "GG", ...) {
  variance_table <- arrange_anova(x, correction)

  print_anova(variance_table, ...)
}


#' @rdname apa_print.aov
#' @method apa_print afex_aov
#' @export

apa_print.afex_aov <- function(x, correction = "GG", ...) {
  summary_x <- summary(x$Anova)

  apa_print(summary_x, correction = correction, ...)
}


#' @rdname apa_print.aov
#' @method apa_print anova
#' @export

apa_print.anova <- function(
  x
  , models = NULL
  , ci = 0.90
  , boot_samples = 1000
  , ...
) {
  if(!is.null(ci)) validate(ci, check_class = "numeric", check_length = 1, check_range = c(0, 1))

  variance_table <- arrange_anova(x)

  if(
    any(
      grepl("Model 1", attr(x, "heading")) &
      grepl("Model 2", attr(x, "heading"))
    ) || is.null(x[["Sum Sq"]])
  ) {
    return(print_model_comp(variance_table, in_paren = in_paren, models = models, ci = ci, boot_samples = boot_samples))
  } else {
    return(print_anova(variance_table, ...))
  }
}
