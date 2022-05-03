#' Typeset Statistical Results from GLM
#'
#' These methods take (generalized) linear model objects to create formatted
#' character strings to report the results in accordance with APA manuscript
#' guidelines.
#'
#' @param x `glm` or `lm` object.
#' @param est_name Character. If `NULL` (the default) the name given in `x`
#'   (or a formally correct adaptation, such as "\eqn{b^*}" instead of "b" for
#'   standardized regression coefficients) is used. Otherwise the supplied name
#'   is used. See details.
#' @param standardized Logical. Indicates if coefficients were standardized
#'   (e.g., using \code{scale()}), and leading zeros should be omitted if
#'   appropriate. See details.
#' @param conf.int Numeric. Either a single value (range \[0, 1\]) giving the
#'   confidence level or a two-column `matrix` with confidence region bounds as
#'   column names (e.g. `"2.5 %"` and `"97.5 %"`) and coefficient names as row
#'   names (in the same order as they appear in `summary(x)$coefficients`.
#'   See details.
#' @param observed Logical. Indicates whether predictor variables were
#'   observed. See details.
#' @inheritParams glue_apa_results
#' @inheritDotParams apa_num
#' @details
#'   The coefficient names are sanitized to facilitate their use as list names.
#'   Parentheses are omitted and other non-word characters are replaced by `_`
#'   (see [sanitize_terms()]).
#'
#'   `est_name` is placed in the output string and is then passed to pandoc or
#'   LaTeX through \pkg{knitr}. Thus, to the extent it is supported by the
#'   final document type, you can pass LaTeX-markup to format the final text
#'   (e.g., `"\\\\beta"` yields \eqn{\beta}).
#'
#'   If `standardized = TRUE`, `scale()` is removed from coefficient names
#'   (see examples). This option is currently ignored for `glm`-objects.
#'
#'   If `conf.int` is a single value, confidence intervals are calculated using
#'   [stats::confint()].
#'
#'   If `x` is an `lm` object and the \pkg{MBESS} package is available,
#'   confidence intervals for \eqn{R^2} are computed using [MBESS::ci.R2()] to
#'   obtain a confidence region that corresponds to the \eqn{\alpha}-level
#'   chosen for the confidence intervals of regression coefficients (e.g.,
#'   95% CI or \eqn{\alpha = 0.05} for regression coefficients yields a 90% CI
#'   for \eqn{R^2}, see Steiger, 2004). If `observed = FALSE`, it is assumed
#'   that predictors are fixed variables, i.e., "the values of the
#'   \[predictors\] were selected a priori as part of the research design"
#'   (p. 15, Kelly, 2007); put differently, it is assumed that predictors are
#'   not random.
#'
#' @evalRd apa_results_return_value()
#'
#' @references
#'    Steiger (2004). Beyond the F Test: Effect Size Confidence Intervals and Tests of Close Fit in the Analysis of
#'    Variance and Contrast Analysis. *Psychological Methods*, *9*(2), 164-182.
#'    doi: \doi{10.1037/1082-989X.9.2.164}
#'
#'    Kelley, K. (2007). Confidence intervals for standardized effect sizes: Theory, application, and
#'    implementation. *Journal of Statistical Software*, *20*(8), 1-24.
#'    doi: \doi{10.18637/jss.v020.i08}
#'
#' @family apa_print
#' @seealso [stats::confint()], [MBESS::ci.pvaf()]
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
#' trt <- rep(trt, 2) # More data is always better
#' ctl <- rep(ctl, 2)
#' lm_fit2 <- lm(scale(trt) ~ scale(ctl))
#'
#' apa_print(lm_fit2, standardized = TRUE)
#'
#' # It is possible to simplify the regression table with transmute_df_into_label():
#' transmute_df_into_label(apa_print(lm_fit2, standardized = TRUE))
#'
#'
#' # Dobson (1990) Page 93: Randomized Controlled Trial :
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' d.AD <- data.frame(treatment, outcome, counts)
#' glm.D93 <- glm(counts ~ outcome + treatment, family = poisson())
#'
#' apa_print(glm.D93)
#' @rdname apa_print.glm
#' @method apa_print lm
#' @export

apa_print.lm <- function(
  x
  , est_name = NULL
  , standardized = FALSE
  , conf.int = 0.95
  , observed = TRUE
  , in_paren = FALSE
  , ...
) {

  ellipsis_ci <- deprecate_ci(conf.int = conf.int, ...)
  ellipsis <- ellipsis_ci$ellipsis
  conf.int <- ellipsis_ci$conf.int

  validate(x, check_class = "lm")
  if(!is.null(est_name)) validate(est_name, check_class = "character", check_length = 1)
  validate(standardized, check_class = "logical", check_length = 1)
  if(!is.null(conf.int)) {
    if(length(conf.int) == 1) {
      validate(conf.int, check_class = "numeric", check_length = 1, check_range = c(0, 1))
      conf.int <- stats::confint(x, level = conf.int)
    } else {
      validate(conf.int, check_class = "matrix")
      sapply(conf.int, validate, check_class = "numeric")
    }
  } else validate(conf.int)
  validate(in_paren, check_class = "logical", check_length = 1)


  if(is.null(est_name)) {
    if(standardized) est_name <- "$b^*$" else est_name <- "$b$"
  } else {
    est_name <- paste0("$", strip_math_tags(est_name), "$")
  }
  if(standardized) ellipsis$gt1 <- FALSE

  if(is.matrix(conf.int)) {
    conf_level <- as.numeric(gsub("[^.|\\d]", "", colnames(conf.int), perl = TRUE))
    conf_level <- 100 - conf_level[1] * 2
    confint_out <- conf.int
  } else {
    conf_level <- 100 * conf.int
    confint_out <- stats::confint(x, level = conf.int)
  }

  is_glm <- inherits(x, "glm")
  summary_x <- summary(x)

  x_df <- as.data.frame(summary_x$coefficients)
  x_df$Predictor <- rownames(x_df)
  x_df$conf.int <- Map(
    confint_out[, 1L]
    , confint_out[, 2L]
    , f = list
  )
  attr(x_df$conf.int, "conf.level") <- conf_level / 100
  if(!is_glm) x_df$df <- stats::df.residual(x)

  canonical_table <- canonize(
    x_df
    , est_label = est_name
    , stat_label = if(is_glm) gsub(colnames(x_df)[3L], pattern = " value", replacement = "", fixed = TRUE) else "t"
  )
  beautiful_table <- do.call("beautify", c(ellipsis, list(x = canonical_table, standardized = standardized)))

  # Concatenate character strings and return as named list
  apa_res <- glue_apa_results(
    beautiful_table
    , est_glue = construct_glue(beautiful_table, "estimate")
    , stat_glue = construct_glue(beautiful_table, "statistic")
    , in_paren = in_paren
    , est_first = TRUE
    , simplify = FALSE
  )

  # Add model fit ----

  if(!is_glm) {

  p <- apa_p(
    stats::pf(
      q = summary_x$fstatistic[[1L]]
      , df1 = summary_x$fstatistic[[2L]]
      , df2 = summary_x$fstatistic[[3L]]
      , lower.tail = FALSE
    )
  )
  p <- add_equals(p)

  apa_res$statistic$modelfit$r2 <- paste0(
    "$F("
    , apa_df(summary_x$fstatistic[[2L]])
    , ", "
    , apa_df(summary_x$fstatistic[[3L]])
    , ") = "
    , apa_num(summary_x$fstatistic[[1L]])
    , "$, $p "
    , p
    , "$"
  )
  if(in_paren) apa_res$statistic$modelfit$r2 <- in_paren(apa_res$statistic$modelfit$r2)

  if(package_available("MBESS")) {
    ci_conf_level <- 100 - ((100 - conf_level) * 2)
    # Steiger (2004). Beyond the F Test: Effect Size Confidence Intervals and Tests of Close Fit in the Analysis of Variance and Contrast Analysis.
    # Psychological Methods, 9(2), 164-182. doi: 10.1037/1082-989X.9.2.164
    # See also http://daniellakens.blogspot.de/2014/06/calculating-confidence-intervals-for.html

    r2_ci <- MBESS::ci.R2(
      R2 = summary_x$r.squared
      , df.1 = summary_x$fstatistic[[2L]] # glance_x$df
      , df.2 = summary_x$fstatistic[[3L]]
      , conf.level = ci_conf_level / 100
      , Random.Predictors = observed
    )

    if(!any(is.na(c(r2_ci$Lower, r2_ci$Upper)))) { # MBESS::ci.R2 can sometimes result in NA if F is really small
      apa_res$estimate$modelfit$r2 <- paste0("$R^2 = ", apa_num(summary_x$r.squared, gt1 = FALSE, zero = FALSE), "$, ", apa_confint(c(r2_ci$Lower, r2_ci$Upper), conf.int = ci_conf_level, enclose_math = TRUE))
    }
  } else {
    apa_res$estimate$modelfit$r2 <- paste0("$R^2 = ", apa_num(summary_x$r.squared, gt1 = FALSE, zero = FALSE), "$")
  }

  apa_res$estimate$modelfit$r2_adj <- paste0("$R^2_{adj} = ", apa_num(summary_x$adj.r.squared, gt1 = FALSE, zero = FALSE), "$")
  }
  apa_res$estimate$modelfit$aic <- paste0("$\\mathrm{AIC} = ", apa_num(stats::AIC(x)), "$")
  apa_res$estimate$modelfit$bic <- paste0("$\\mathrm{BIC} = ", apa_num(stats::BIC(x)), "$")

  if(!is_glm) {
    apa_res$full_result$modelfit$r2 <- paste(apa_res$estimate$modelfit$r2, apa_res$statistic$modelfit$r2, sep = ", ")
  }

  apa_res
}

# for backward compatibility, we also export apa_print.glm() (it used to be a separate method)
#' @rdname apa_print.glm
#' @export
apa_print.glm <- apa_print.lm



#' @rdname apa_print.glm
#' @method apa_print summary.glm
#' @export

apa_print.summary.glm <- function(x, ...) {
  validate(x, check_class = "summary.glm")

  x <- eval.parent(x$call, n = 1)
  apa_print(x, ...)
}


#' @rdname apa_print.glm
#' @method apa_print summary.lm
#' @export

apa_print.summary.lm <- function(x, ...) {
  validate(x, check_class = "summary.lm")

  x <- eval.parent(x$call, n = 1)
  apa_print(x, ...)
}
