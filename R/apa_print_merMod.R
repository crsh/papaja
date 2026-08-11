#' Typeset Statistical Results from Hierarchical GLM
#'
#' These methods take objects from various R functions that calculate
#' hierarchical (generalized) linear models to create formatted character
#' strings to report the results in accordance with APA manuscript guidelines.
#'
#' @param x A fitted hierarchical (generalized) linear model, either from
#'   [lme4::lmer()], [lmerTest::lmer()], [afex::mixed()], [lme4::glmer()], or
#'   [glmmTMB::glmmTMB()].
#' @param effects Character. Determines which information is returned.
#'   Currently, only fixed-effects terms (`"fixed"`) are supported.
#' @param conf.int Numeric specifying the required confidence level *or* a named
#'   list specifying additional arguments that are passed to
#'   [lme4::confint.merMod()] or [glmmTMB::confint.glmmTMB()], see details.
#' @param est_name An optional character. The label to be used for
#'   fixed-effects coefficients.
#' @inheritParams beautify
#' @inheritParams glue_apa_results
#' @details
#'   Confidence intervals are calculated by calling [lme4::confint.merMod()] or
#'   [glmmTMB::confint.glmmTMB()].
#'   By default, *Wald* confidence intervals are calculated, but this may
#'   change in the future.
#'
#' @evalRd apa_results_return_value()
#'
#' @examples
#' \donttest{
#'   # Fit a linear mixed model using the lme4 package
#'   # or the lmerTest package (if dfs and p values are desired)
#'   library(lmerTest)
#'   fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'   # Format statistics for fixed-effects terms (the default)
#'   apa_print(fm1)
#'
#'   # Alternatively, use glmmTMB:
#'   library(glmmTMB)
#'   fm2 <- glmmTMB(Reaction ~ Days + (Days | Subject), sleepstudy)
#'   apa_print(fm2)
#'
#'
#' }
#'
#' @family apa_print
#' @rdname apa_print.merMod
#' @method apa_print merMod
#' @export

apa_print.merMod <- function(
  x
  , effects = "fixed"
  , conf.int = .95
  , in_paren = FALSE
  , est_name = NULL
  , ...
) {

  # Input validation and processing ----
  ellipsis_ci <- deprecate_ci(conf.int, ...)
  ellipsis <- ellipsis_ci$ellipsis
  conf.int <- ellipsis_ci$conf.int

  if(is.list(conf.int)) {
    validate(conf.int, check_class = "list")
  } else {
    validate(conf.int, check_class = "numeric", check_length = 1L)
    conf.int <- list(level = conf.int)
  }

  effects <- match.arg(effects, several.ok = FALSE)
  is_glmmTMB <- inherits(x, "glmmTMB")



  if(is.null(est_name)) {
    est_name <- "$\\hat{\\beta}$"
  } else {
    validate(est_name, check_class = "character", check_length = 1L)
    est_name <- paste0("$", strip_math_tags(est_name), "$")
  }

  args_confint <- defaults(
    conf.int
    , set = list(
      object = x
      , parm = "beta_"
    )
    , set.if.null = list(
      level = .95
      , method = "Wald"
    )
  )
  if(is_glmmTMB) {
    args_confint$estimate <- FALSE
  } else {
    if(is.null(args_confint$nsim)) args_confint$nsim <- 2e3L
  }
  # glmmTMB: x, parm = "beta_", level = conf.int, component = "cond", estimate = FALSE

  # GLMM with non-fixed scale? (cf. lme4::profile.merMod)
  # no_profile <- lme4::isGLMM(x) && x@devcomp$dims[["useSc"]]
  # if(args_confint$method == "profile" && no_profile)



  # Rearrange ----
  if(is_glmmTMB) {
    res_table <- as.data.frame(summary(x)$coefficients$cond)
  } else {
    res_table <- as.data.frame(summary(x)$coefficients)
  }

  res_table$Term <- rownames(res_table)
  rownames(res_table) <- NULL


  # Add confidence intervals ----
  confidence_intervals <-
    do.call("confint", args_confint)[res_table$Term, ] # ensure same arrangement as in model object

  res_table$conf.int <- asplit(confidence_intervals, MARGIN = 1L, drop = TRUE)
  attr(res_table$conf.int, "conf.level") <- args_confint$level


  # canonize, beautify, glue ----
  ellipsis$x <- canonize(res_table, est_label = est_name)
  beautiful_table <- do.call("beautify", ellipsis)

  glue_apa_results(
    beautiful_table
    , est_glue = construct_glue(beautiful_table, "estimate")
    , stat_glue = construct_glue(beautiful_table, "statistic")
    , in_paren = in_paren
    , simplify = FALSE
  )
}

#' @rdname apa_print.merMod
#' @method apa_print mixed
#' @export

apa_print.mixed <- function(x, ...) {

  anova_table <- x$anova_table
  attr(anova_table, "method") <- attr(x, "method")
  apa_print(anova_table, ...)
}


#' @family apa_print
#' @rdname apa_print.merMod
#' @method apa_print glmmTMB
#' @export

apa_print.glmmTMB <- apa_print.merMod
