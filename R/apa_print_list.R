#' Typeset Statistical Results from Linear-Model Comparisons
#'
#' This method performs comparisons of [lm][lm()]-objects and creates formatted
#' character strings and a model comparison table to report the results in
#' accordance with APA manuscript guidelines.
#'
#' @param x List. A `list` containing to-be-compared [lm][lm()] objects.
#'   If the list is completely named, element names are used as model names in
#'   the output object.
#' @param anova_fun Function. Function to compare model-objects contained in
#'   `x`.
#' @param ci Numeric. Confidence level for the bootstrap confidence interval
#'   for \eqn{\Delta R^2} (range \[0, 1\]); ignored if \code{boot_samples = 0}.
#' @param boot_samples Numeric. Number of bootstrap samples to estimate
#'   confidence intervals for \eqn{\Delta R^2}.
#' @param progress_bar Logical. Determines whether a progress bar is printed
#'   while bootstrapping.
#' @param ... Additional arguments passed to the function specified as
#'   `anova_fun`.
#' @inheritParams glue_apa_results
#' @inheritParams apa_print.glm
#'
#' @details
#'  As demonstrated by Algina, Keselman & Penfield (2007), asymptotic
#'  confidence intervals for \eqn{\Delta R^2} are often unreliable. Confidence
#'  intervals for model comparisons of \code{lm} objects are, therefore,
#'  estimated using their modified percentile bootstrap method. Note that the
#'  accuracy of the confidence intervals depends on the number of predictors
#'  \eqn{p}, their distribution, and the sample size \eqn{n}:
#'
#'  *"When the predictor distribution is multivariate normal, one can obtain
#'  accurate CIs for \eqn{\rho^2} with \eqn{n \geq~50} when \eqn{p = 3}. For
#'  \eqn{p = 6} and for \eqn{p = 9}, \eqn{n \geq~100} is advisable. When the
#'  predictor distribution is nonnormal in form, sample size requirements vary
#'  with type of nonnormality." (p. 939, Algina, Keselman & Penfield, 2010)*
#'
#'  If \pkg{MBESS} is available, confidence intervals for \eqn{R^2} are
#'  computed using [MBESS::ci.R2()] to obtain a confidence region that
#'  corresponds to the confidence level `ci`, the default being a 90% CI (see
#'  Steiger, 2004). If `observed = FALSE`, it is assumed that predictors are
#'  fixed variables, i.e., "the values of the \[predictors\] were selected a
#'  priori as part of the research design" (p. 15, Kelly, 2007); put
#'  differently, it is assumed that predictors are not random. The confidence
#'  intervals for the regression coefficients in the model comparison table
#'  correspond to the \eqn{\alpha}-level chosen for \eqn{R^2} and
#'  \eqn{\Delta R^2} (e.g., 90% CI or \eqn{\alpha = 0.10} for \eqn{R^2} and
#'  \eqn{\Delta R^2} yields a 95% CI for regression coefficients,
#'  Steiger, 2004).
#'
#' @evalRd apa_results_return_value()
#'
#' @references
#'    Algina, J., Keselman, H. J., & Penfield, R. D. (2007).
#'    Confidence intervals for an effect size measure in multiple linear regression.
#'    *Educational and Psychological Measurement*, *67*(2), 207--218.
#'    doi:[10.1177/0013164406292030](https://doi.org/10.1177/0013164406292030)
#'
#'    Algina, J., Keselman, H. J., & Penfield, R. D. (2010).
#'    Confidence intervals for squared semipartial correlation coefficients: The effect of nonnormality.
#'    *Educational and Psychological Measurement*, *70*(6), 926--940.
#'    doi:[10.1177/0013164410379335](https://doi.org/10.1177/0013164410379335)
#'
#'    Steiger (2004).
#'    Beyond the F test: Effect size confidence intervals and tests of close fit in the analysis of variance and contrast analysis.
#'    *Psychological Methods*, *9*(2), 164--182.
#'    doi:[10.1037/1082-989X.9.2.164](https://doi.org/10.1037/1082-989X.9.2.164)
#'
#'    Kelley, K. (2007).
#'    Confidence intervals for standardized effect sizes: Theory, application, and  implementation.
#'    *Journal of Statistical Software*, *20*(8), 1--24.
#'    doi:[10.18637/jss.v020.i08](https://doi.org/10.18637/jss.v020.i08)
#'
#' @family apa_print
#' @seealso [stats::anova()]
#' @examples
#'   mod1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
#'   mod2 <- update(mod1, formula = . ~ . + Petal.Length)
#'   mod3 <- update(mod2, formula = . ~ . + Petal.Width)
#'
#'   # No bootstrapped Delta R^2 CI
#'   apa_print(list(Baseline = mod1, Length = mod2, Both = mod3), boot_samples = 0)
#' @method apa_print list
#' @export

apa_print.list <- function(
  x
  , anova_fun = stats::anova
  , ci = 0.90
  , boot_samples = 10000
  , progress_bar = interactive()
  , observed = TRUE
  , in_paren = FALSE
  , ...
) {
  if(length(x) == 1) apa_print(x[[1]]) else {
    if(class(x[[1]]) != "lm") stop("Currently, only model comparisons for 'lm' objects are supported.")
  }

  validate(ci, check_class = "numeric", check_length = 1, check_range = c(0, 1))
  validate(boot_samples, check_class = "numeric", check_length = 1)
  validate(progress_bar, check_class = "logical", check_length = 1L)
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
    return(print_model_comp(variance_table, models = x, ci = ci, boot_samples = boot_samples, progress_bar = progress_bar, in_paren = in_paren))
  }
}
