#' Format statistics from ANOVA (APA 6th edition)
#'
#'  This methods performs comparisons of \code{lm}-objects and creates formatted chraracter
#' strings and a model comparison table to report the results in accordance with APA manuscript guidelines.
#'
#' @param x List. List containing to be compared \code{lm}-objects. If the list is completely named, element names are used as model names in the ouptut object.
#' @param anova_fun Function. Function to compare model-objects contained in \code{x}.
#' @param ci Numeric. Confidence level for the bootstrap confidence interval for \eqn{\Delta R^2} (range [0, 1]); ignored if \code{boot_samples = 0}.
#' @param boot_samples Numeric. Number of bootstrap samples to estimate confidence intervals for \eqn{\Delta R^2}.
#' @param in_paren Logical. Indicates if the formated string will be reported inside parentheses. See details.
#' @param ...
#'
#' @details
#'
#'    As demonstrated by Algina, Keselman & Penfield (2007), asymptotic confidence intervals for \eqn{\Delta R^2}
#'    are often unreliable. Confidence intervals for model comparisons of \code{lm}-objects are, therefore, estimated
#'    using their modified percentile bootstrap method. Note that the accuracy of the confidence intervals depends on
#'    the number of predictors \eqn{p}, their distribution, and the sample size \eqn{n}:
#'
#'    \emph{"When the predictor distribution is multivariate normal, one can obtain accurate CIs for \eqn{\rho^2} with
#'    \eqn{n \geq~50} when \eqn{p = 3}. For \eqn{p = 6} and for \eqn{p = 9}, \eqn{n \geq~100} is advisable. When the
#'    predictor distribution is nonnormal in form, sample size requirements vary with type of nonnormality." (p. 939,
#'    Algina, Keselman & Penfield, 2010)}
#'
#'    Confidence intervals for \eqn{R^2} and \eqn{\Delta R^2} are computed for the requested confidence level (\code{ci}), default being a 90\% CI, see Steiger (2004).
#'    The confidence intervals for the regression coefficients in the model comparison table are correspond to the \eqn{\alpha}-level chosen for \eqn{R^2} and \eqn{\Delta R^2} (e.g., 90\% CI or \eqn{\alpha = 0.10} for \eqn{R^2} and \eqn{\Delta R^2} yields a 95\% CI for regression coefficients, Steiger, 2004)
#'
#'    If \code{in_paren} is \code{TRUE} parentheses in the formatted string, such as those surrounding degrees
#'    of freedom, are replaced with brackets.
#' @return
#'    \code{apa_print.list} returns a named list containing the following components according to the input:
#'
#'    \describe{
#'      \item{\code{stat}}{A named list of character strings giving the test statistic, parameters, and \emph{p}
#'          value for each non-baseline model.}
#'      \item{\code{est}}{A named list of character strings giving the effect size estimates for each non-baseline model}
#'      \item{\code{full}}{A named list of character strings comprised of \code{est} and \code{stat} for each non-baseline model}
#'      \item{\code{table}}{A data.frame containing the complete model comparison table including regression coefficients, which can be passed to \code{\link{apa_table}}.}
#'    }
#' @references
#'    Algina, J., Keselman, H. J., & Penfield, R. D. (2007). Confidence Intervals for an Effect Size Measure in Multiple Linear Regression.
#'    \emph{Educational and Psychological Measurement}, 67(2), 207--218. doi:\href{http://dx.doi.org/10.1177/0013164406292030}{10.1177/0013164406292030}
#'
#'    Algina, J., Keselman, H. J., & Penfield, R. D. (2010). Confidence Intervals for Squared Semipartial Correlation Coefficients: The Effect of Nonnormality.
#'    \emph{Educational and Psychological Measurement}, 70(6), 926--940. doi:\href{http://dx.doi.org/10.1177/0013164410379335}{10.1177/0013164410379335}
#'
#' Steiger (2004). Beyond the F Test: Effect Size Confidence Intervals and Tests of Close Fit in the Analysis of
#' Variance and Contrast Analysis. \emph{Psychological Methods}, 9(2), 164-182.
#' doi:\href{http://dx.doi.org/10.1037/1082-989X.9.2.164}{10.1037/1082-989X.9.2.164}
#' @family apa_print
#' @seealso \code{\link{anova}}
#' @examples
#'    mod1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
#'    mod2 <- update(mod1, formula = . ~ . + Petal.Length)
#'    mod3 <- update(mod2, formula = . ~ . + Petal.Width)
#'
#'    # No bootstrapped Delta R^2 CI
#'    apa_print(list(Baseline = mod1, Length = mod2, Both = mod3), boot_samples = 0)
#' @export

apa_print.list <- function(
  x
  , anova_fun = anova
  , ci = 0.90
  , boot_samples = 10000
  , in_paren = FALSE
  , ...
) {
  if(length(x) == 1) apa_print(x[[1]]) else {
    if(class(x[[1]]) != "lm") stop("Currently, only model comparisons for 'lm' objects are supported.")
  }

  validate(ci, check_class = "numeric", check_length = 1, check_range = c(0, 1))
  validate(boot_samples, check_class = "numeric", check_length = 1)
  validate(in_paren, check_class = "logical", check_length = 1)

  model_labels <- names(x)

  # Compare models
  names(x) <- NULL
  model_comp <- do.call(anova_fun, x, ...)

  variance_table <- arrange_anova(model_comp)
  if(!is.null(model_labels) & sum(model_labels != "") == length(model_labels)) {
    variance_table$term <- model_labels[-1]
    names(x) <- model_labels
  } else {
    names(x) <- paste("Model", 1:length(x))
  }

  if("apa_model_comp" %in% class(variance_table)) { # Model comparison object
    return(print_model_comp(variance_table, models = x, ci = ci, boot_samples = boot_samples, in_paren = in_paren))
  }
}
